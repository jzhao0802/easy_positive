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

MinMaxStandardise <- function(vec, minVal=NULL, maxVal=NULL)
{
  if (sd(vec) == 0)
    return (vec)
  
  vec <- (vec - min(vec)) / (max(vec) - min(vec))
  
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
                     iLearner+nrow(allCombinationsThisType)*length(posNegRatios)-1, 
                     ":", sep=""), 
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
        # writeLines("", file)
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
  valiFolds <- StratifyEasyDifficultPositives(y, posWeightsTrainVali, kValiFolds)
  
  # 
  
  if (bParallel)
  {
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
                  XTrain <- X[trainIDs,]
                  # validation data extracted before subsampling training
                  XVali <- X[-trainIDs,]
                  yVali <- y[-trainIDs]
                  
                  # sub-sample a fraction of the negatives
                  
                  trainIDsPos <- trainIDs[yTrain == 1]
                  trainIDsNeg <- trainIDs[yTrain == 0]
                  nNegs2Sample <- 
                    ceiling(length(trainIDsPos) / learnerSignature$posNegRatio)
                  if (nNegs2Sample < length(trainIDsNeg))
                  {
                    trainIDsNegSubSampled <- 
                      sample(trainIDsNeg)[1:nNegs2Sample]
                    trainIDs <- c(trainIDsPos, trainIDsNeg)
                  }
                  
                  posWeightsTrain <- posWeightsTrainVali[trainIDs]
                  
                  # 
                  model <- TrainAWeakLearner(yTrain, XTrain, posWeightsTrain,
                                             learnerSignature)
                  
                  predsAllData[-trainIDs] <- 
                    PredictWithAWeakLearner(model, XVali, learnerSignature)
                }
              }
  } else 
  {
    predsAllLearners <- 
      foreach(iLearner=(1:length(weakLearnerPool)), .combine="cbind", 
              .maxcombine=1e5,
              .packages=c("glmnet", "e1071", "randomForest", "party", 
                          "pROC", "pROC")) %do%
              {
                learnerSignature <- weakLearnerPool[[iLearner]]
                
                predsAllData <- rep(1e5, nrow(X))
                
                for (iFold in 1:kValiFolds)
                {
                  trainIDs <- valiFolds[[iFold]]
                  yTrain <- y[trainIDs]
                  XTrain <- X[trainIDs,]
                  # validation data extracted before subsampling training
                  XVali <- X[-trainIDs,]
                  yVali <- y[-trainIDs]
                  
                  # sub-sample a fraction of the negatives
                  
                  trainIDsPos <- trainIDs[yTrain == 1]
                  trainIDsNeg <- trainIDs[yTrain == 0]
                  nNegs2Sample <- 
                    ceiling(length(trainIDsPos) / learnerSignature$posNegRatio)
                  if (nNegs2Sample < length(trainIDsNeg))
                  {
                    trainIDsNegSubSampled <- 
                      sample(trainIDsNeg)[1:nNegs2Sample]
                    trainIDs <- c(trainIDsPos, trainIDsNeg)
                  }
                  
                  posWeightsTrain <- posWeightsTrainVali[trainIDs]
                  
                  # 
                  model <- TrainAWeakLearner(yTrain, XTrain, posWeightsTrain,
                                             learnerSignature)
                  
                  predsAllData[-trainIDs] <- 
                    PredictWithAWeakLearner(model, XVali, learnerSignature)
                }
              }
  }
  
  # before returning, select the top 5% using accuracy + independence
  
  winnerPortion <- 0.05
  threshold <- 0.6
  winnerIndices <- SelectWinners(predsAllLearners, y, winnerPortion,
                                 threshold)
  
  return (winnerIndices)
}

TrainWinnerLearners <- function(y, X, posWeightsTrainVali, 
                                weakLearnerPool, winnerIndices,
                                bParallel)
{
  if (bParallel)
  {
    learners <- 
      foreach(
        iLearner=(1:length(winnerIndices)), 
        .maxcombine=1e5,
        .packages=c("glmnet", "e1071", "randomForest", "party", 
                    "pROC", "pROC")) %dopar%
        {
          learnerSignature <- weakLearnerPool[[winnerIndices[iLearner]]]
          
          # sub-sample a fraction of the negatives
          
          trainIDs <- 1:length(y)
          trainIDsPos <- trainIDs[yTrain == 1]
          trainIDsNeg <- trainIDs[yTrain == 0]
          nNegs2Sample <- 
            ceiling(length(trainIDsPos) / learnerSignature$posNegRatio)
          if (nNegs2Sample < length(trainIDsNeg))
          {
            trainIDsNegSubSampled <- 
              sample(trainIDsNeg)[1:nNegs2Sample]
            trainIDs <- c(trainIDsPos, trainIDsNeg)
          }
            
          posWeightsTrain <- posWeightsTrainVali[trainIDs]
          
          model <- TrainAWeakLearner(y[trainIDs], XTrain[trainIDs, ], 
                                     posWeightsTrain, learnerSignature)
          
          list(model=model, 
               signature=learnerSignature)
        }
  } else 
  {
    learners <- 
      foreach(
        iLearner=(1:length(winnerIndices)), 
        .maxcombine=1e5,
        .packages=c("glmnet", "e1071", "randomForest", "party", 
                    "pROC", "pROC")) %dopar%
        {
          learnerSignature <- weakLearnerPool[[winnerIndices[iLearner]]]
          
          # sub-sample a fraction of the negatives
          
          trainIDs <- 1:length(y)
          trainIDsPos <- trainIDs[yTrain == 1]
          trainIDsNeg <- trainIDs[yTrain == 0]
          nNegs2Sample <- 
            ceiling(length(trainIDsPos) / learnerSignature$posNegRatio)
          if (nNegs2Sample < length(trainIDsNeg))
          {
            trainIDsNegSubSampled <- 
              sample(trainIDsNeg)[1:nNegs2Sample]
            trainIDs <- c(trainIDsPos, trainIDsNeg)
          }
          
          posWeightsTrain <- posWeightsTrainVali[trainIDs]
          
          model <- TrainAWeakLearner(y[trainIDs], XTrain[trainIDs, ], 
                                     posWeightsTrain, learnerSignature)
          
          list(model=model, 
               signature=learnerSignature)
        }
  }
  
  return (learners)
}

PredictWithAnEnsemble <- function(weakLearners, XEval,
                                  bParallel)
{
  if (bParallel)
  {
    predsAllWeakLearners <- 
      foreach(
        iLearner=(1:length(weakLearners)), 
        .maxcombine=1e5,
        .packages=c("glmnet", "e1071", "randomForest", "party", 
                    "pROC", "pROC")
        ) %dopar%
      {
        preds <- 
          PredictWithAWeakLearner(weakLearners[[iLearner]]$model, 
                                  XEval, weakLearners[[iLearner]]$signature)
      }
  } else 
  {
    predsAllWeakLearners <- 
      foreach(
        iLearner=(1:length(weakLearners)), 
        .maxcombine=1e5,
        .packages=c("glmnet", "e1071", "randomForest", "party", 
                    "pROC", "pROC")
      ) %do%
      {
        preds <- 
          PredictWithAWeakLearner(weakLearners[[iLearner]]$model, 
                                  XEval, weakLearners[[iLearner]]$signature)
      }
  }
  
  return (predsAllWeakLearners)
}

SaveEnsemble <- function(resultDir, iEvalFold, winnerLearners)
{
  n <- length(winnerLearners)
  fileSummary <- 
    file(paste(resultDir, 
               "winnersSummary_EvalFold", 
               iEvalFold, ".txt", sep=""), "w")
  for (iLearner in 1:length(winnerLearners))
  {
    # summary
    writeLines(paste("Winner ", iLearner, ": ", sep=""), sep="", fileSummary)
    writeLines(paste(winnerLearners$signature$type, 
                     ", ", sep=""), 
               sep="", fileSummary)
    paramNames <- names(winnerLearners$signature$hyperParams)
    paramVals <- winnerLearners$signature$hyperParams
    for (iParam in 1:length(paramNames))
    {
      writeLines(paste(paramNames[iParam],
                       ":", paramVals[[paramNames[iParam]]],
                       ", ", sep=""), sep="", fileSummary)
    }
    writeLines(paste("Positive-Negative Training Data Ratio:", 
                     winnerLearners$signature$posNegRatio), fileSummary)
    writeLines("", fileSummary)
    
    # the models and signatures
    filename <- paste(resultDir, "WeakLearner", iLearner, "Of", n, 
                      "_EvalFold", iEvalFold, ".rds")
    saveRDS(winnerLearners[[iLearner]], filename)
  }
  
  close(fileSummary)
}

SaveEvalResult <- function(resultDir, preds, labels)
{
  write.table(preds, sep=",", 
              file=paste(resultDir, "preds.csv", sep=""), 
              col.names=T, row.names=F)
  
  predsObj <- prediction(predictions=preds[,2], labels=labels)
  perf <- performance(predsObj, measure="prec", x.measure="rec")
  write.table(cbind(perf@x.values[[1]],perf@y.values[[1]]), sep=",", 
              file=paste(resultDir, "PR.csv", sep=""), 
              col.names=c("recall", "precision"),
              row.names=F)
  
  precision0.05Recall <- 
    approx(perf@x.values[[1]], perf@y.values[[1]], xout=0.05)
  write.table(precision0.05Recall, sep=",", 
              file=paste(resultDir, "precision0.05Recall.csv", sep=""), 
              col.names=F, row.names=F)
}

# SelfEvalModel evaluates the trained ensemble on a subset of the input data
SelfEvalModel <- function(y, X, posWeights,
                          kEvalFolds, kValiFolds, 
                          weakLearnerSeed,
                          posNegRatios,
                          obsIDs=NULL,
                          bParallel, 
                          resultDir)
{
  #
  ## transform to a vector with the same length as y, for the ease of stratification
  
  posWeightsAllData <- rep(0, length(y))
  posWeightsAllData[y == 1] <- posWeights
  
  #
  ## stratify for ensemble evaluation 
  
  # a more sophisticated stratification: every fold has similar-sized
  # 'purer / easier' positive patients;
  evalFolds <- StratifyEasyDifficultPositives(y, posWeightsAllData, kEvalFolds)
  
  weakLearnerPool <- 
    GenWeakLearnerPool(weakLearnerSeed, posNegRatios, resultDir)
  stop("after GenWeakLearnerPool.")
  
  #
  ## ensemble evaluation
  
  predsAllData <- rep(1e5,length(y))
  
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
    
    # standardise the training + validation data
    
    XTrainVali <- apply(XTrainVali, 2, MinMaxStandardise)
    
    # standardise the evaluation data accordingly
    
    for (iVar in 1:ncol(XEval))
    {
      r <- max(XTrainVali[, iVar]) - min(XTrainVali[, iVar])
      if (r != 0)
      {
        XEval[, iVar] <- (XEval - min(XTrainVali)) / r
      }
    }
    
    # 
    
    posWeightsTrainVali <- posWeightsAllData[trainValiIDs]
    
    winnerIndices <- 
      CV_AllWeakLeaners(y=yTrainVali, X=XTrainVali, 
                        kValiFolds=kValiFolds, 
                        posWeightsTrainVali=posWeightsTrainVali, 
                        weakLearnerPool=weakLearnerPool,
                        bParallel=bParallel)
    
    # train the selected using all the trainVali data
    winnerLearners <- 
      TrainWinnerLearners(y=yTrainVali, X=XTrainVali, 
                          posWeightsTrainVali=posWeightsTrainVali, 
                          weakLearnerPool=weakLearnerPool,
                          winnerIndices=winnerIndices,
                          bParallel=bParallel)
    
    
    # evaluate on the left-out fold
    predsAllData[-trainValiIDs] <-
      PredictWithAnEnsemble(weakLearners=winnerLearners, XEval=XEval, 
                            bParallel=bParallel)
    
    
    # save the ensemble, the evaluation
    
    SaveEnsemble(resultDir, iEvalFold, winnerLearners)
  }
  
  # save the evaluation result
  
  if (!is.null(obsIDs))
  {
    predsAllData <- cbind(obsIDs, predsAllData)
    colnames(predsAllData) <- c("ObservationID", "Prediction")
  }
    
  SaveEvalResult(resultDir, preds=predsAllData, labels=y)
}