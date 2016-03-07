library(foreach)
library(glmnet)
library(e1071)
library(randomForest)
library(party)
library(pROC)
library(ROCR)


source("functions/manualStratify.R")
source("functions/WeakLearners.R")
source("functions/Aggregate.R")

Swap2MakeFirstPositive <- function(yTrain, trainIDs)
{
  firstPosLoc <- (which(yTrain==1))[1]
  tmp <- trainIDs[1]
  trainIDs[1] <- trainIDs[firstPosLoc]
  trainIDs[firstPosLoc] <- tmp
  
  return (trainIDs)
}

MinMaxStandardise <- function(vec, minVal=NULL, maxVal=NULL)
{
  if (((is.null(minVal)) & (!is.null(maxVal))) | 
      ((!is.null(minVal)) & (is.null(maxVal))))
  {
    stop("Error! If the minVal and maxVal are to be specified, they need to be both specified.")
  }
  
  if (minVal == maxVal)
    stop("Error! The specified minVal and maxVal cannot have the same value.")
  
  if ((is.null(minVal)) & (is.null(maxVal))) # calculate min / max using the data
  {
    if (sd(vec) == 0)
      return (vec)
    
    vec <- (vec - min(vec)) / (max(vec) - min(vec))
  } else # use the specified min / max
  {
    vec <- (vec - minVal) / (maxVal - minVal)
  }
  
  return (vec)
}

GenWeakLearnerPool <- function(weakLearnerSeed, posNegRatios, resultDir)
{
  file <- file(paste(resultDir, "WeakLearnerPool.txt", sep=""), "w")
  pool <- list()
  iLearner <- 1
  for (modelType in names(weakLearnerSeed))
  {
    allCombinationsThisType <- 
      expand.grid(weakLearnerSeed[[modelType]])
    writeLines(paste(modelType, ", models ", iLearner, " - ", 
                     iLearner+nrow(allCombinationsThisType-1), ":", sep=""), 
               file)
    
    for (iComb in 1:nrow(allCombinationsThisType))
    {
      for (iRatio in 1:length(posNegRatios))
      {
        writeLines(paste("Model ", iLearner, ": ", sep=""), sep="", file)
        writeLines(paste(modelType, ", ", sep=""), sep="", file)
        for (iParam in 1:ncol(allCombinationsThisType))
        {
          writeLines(paste(colnames(allCombinationsThisType)[iParam],
                           ":", allCombinationsThisType[iComb, iParam],
                           ", ", sep=""), sep="", file)
        }
        writeLines(paste("Positive-Negative Training Data Ratio:", 
                         posNegRatios[iRatio]), file)
        writeLines("", file)
        # print(paste("iLearner:", iLearner))
        pool[[iLearner]] <- list(type=modelType)
        if (ncol(allCombinationsThisType) == 1)
        {
          paramName <- names(weakLearnerSeed[[modelType]])[1]
          pool[[iLearner]]$hyperParams <- 
            data.frame(allCombinationsThisType[iComb, ])
          colnames(pool[[iLearner]]$hyperParams) <- paramName
        } else
          pool[[iLearner]]$hyperParams <- allCombinationsThisType[iComb, ]
        pool[[iLearner]]$posNegRatio <- posNegRatios[iRatio]
        
        iLearner <- iLearner + 1
      }
    }
    writeLines("", file)
  }
  close(file)
  
  return (pool)
}

CV_AllWeakLeaners <- function(y, X, 
                              kValiFolds, 
                              posWeightsTrainVali, 
                              weakLearnerPool,
                              bParallel)
{
  y4Stratification <- rep(0, length(y))
  y4Stratification[posWeightsTrainVali>0.5] <- 1
  valiFolds <- stratifySmallSample(y4Stratification, kValiFolds)
  
  # standardise X
  X <- apply(X, 2, MinMaxStandardise)
  
  # 
  
  if (bParallel)
  {
    stop("cbind below might need special care")
    predsAllLearners <- 
      foreach(iLearner=(1:length(weakLearnerPool)), .combine="cbind", 
              .maxcombine=1e5,
              .packages=c("glmnet", "e1071", "randomForest", "party", 
                          "pROC", "pROC")) %dopar%
              {
                learnerSignature <- weakLearnerPool[[iLearner]]
                
                predsAllData <- rep(1e5, nrow(X))
                
                for (iFold in 1:kValiFolds)
                {
                  trainIDs <- valiFolds[[iFold]]
                  yTrain <- y[trainIDs]
                  # validation data extracted before subsampling training
                  XVali <- X[-trainIDs,]
                  yVali <- y[-trainIDs]
                  
                  # sub-sample a fraction of the negatives
                  
                  trainIDsPos <- trainIDs[yTrain == 1]
                  trainIDsNeg <- trainIDs[yTrain == 0]
                  nNegs2Sample <- 
                    ceiling(length(trainIDsPos) / posWeightsTrainVali)
                  if (nNegs2Sample > length(trainIDsNeg))
                    nNegs2Sample <- length(trainIDsNeg)
                  trainIDsNegSubSampled <- 
                    sample(trainIDsNeg)[1:nNegs2Sample]
                  trainIDs <- c(trainIDsPos, trainIDsNeg)
                  
                  # swap to make sure the first training datum is labelled 1
                  if (yTrain[1] != 1)
                  {
                    trainIDs <- Swap2MakeFirstPositive(yTrain, trainIDs)
                  }
                  
                  XTrain <- X[trainIDs,]
                  yTrain <- y[trainIDs]
                  
                  posWeightsTrain <- posWeightsTrainVali[trainIDs]
                  
                  # 
                  model <- TrainAWeakLearner(yTrain, XTrain, posWeightsTrain,
                                             learnerSignature)
                  
                  predsAllData[-trainIDs] <- 
                    PredictWithAWeakLearner(model, XVali, learnerSignature)
                  
#                   rawPreds <- 
#                     predict(model, XVali, decision.values=T)
#                   predsAllData[-trainIDs] <- 
#                     attr(rawPreds, "decision.values")
                }
              }
  } else 
  {
    stop("non-parallel version not implemented yet. ")
  }
  
  # before returning, select the top 5% using accuracy + independence
  
  winnerPortion <- 0.05
  threshold <- 0.6
  winnerIndices <- SelectWinners(predsAllLearners, y, winnerPortion,
                                 threshold)
  
  return (winnerIndices)
}

# SelfEvalModel evaluates the trained ensemble on a subset of the input data
SelfEvalModel <- function(y, X, posWeights,
                          kEvalFolds, kValiFolds, 
                          weakLearnerSeed,
                          posNegRatios,
                          bParallel, 
                          resultDir)
{
  #
  ## transform to a vector with the same length as y, for the ease of stratification
  
  posWeightsAllData <- rep(0, length(y))
  posWeightsAllData[y == 1] <- posWeights
  
  #
  ## stratify for ensemble evaluation 
  
  # a more sophisticated stratification: every fold have similar-sized
  # 'purer / easier' positive patients;
  y4Stratification <- rep(0, length(y))
  y4Stratification[posWeightsAllData>0.5] <- 1
  evalFolds <- stratifySmallSample(y4Stratification, kEvalFolds)
  
  weakLearnerPool <- 
    GenWeakLearnerPool(weakLearnerSeed, posNegRatios, resultDir)
  
  #
  ## ensemble evaluation
  
  for (iEvalFold in 1:length(evalFolds))
  {
    trainValiIDs <- evalFolds[[iEvalFold]]
    yTrainVali <- y[trainValiIDs]
    
    # swap to make sure the first training datum is labelled 1
    if (yTrainVali[1] != 1)
      trainValiIDs <- Swap2MakeFirstPositive(yTrainVali, trainValiIDs)
    
    XTrainVali <- X[trainValiIDs, ]
    XEval <- X[-trainValiIDs, ]
    yTrainVali <- y[trainValiIDs]
    yEval <- y[-trainValiIDs]
    
    posWeightsTrainVali <- posWeightsAllData[trainValiIDs]
    
    cvResult <- CV_AllWeakLeaners(y=yTrainVali, X=XTrainVali, 
                                  kValiFolds=kValiFolds, 
                                  posWeightsTrainVali=posWeightsTrainVali, 
                                  weakLearnerPool=weakLearnerPool,
                                  bParallel=bParallel)
    
    # select the best 5% (maybe should be within cv)
    
    
    
    # train the selected using all the trainVali data
    
    
    
    # evaluate on the left-out fold
    standardise the evaluation data using min/max computed from cv
    apply(X, 2, MinMaxStandardise, minVal=)
    
    # save the ensemble, the evaluation
  }
  
  # might not be necessary to estimate another ensemble using all x and y
  # if that's needed, do it in another funciton
}