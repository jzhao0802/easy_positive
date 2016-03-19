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
source("functions/MinMaxNormalise.R")

# MinMaxStandardise <- function(vec)
# {
#   if (sd(vec) == 0)
#     return (vec)
#   
#   vec <- (vec - min(vec)) / (max(vec) - min(vec))
#   
#   return (vec)
# }

GenWeakLearnerPool <- function(weakLearnerSeed, posNegRatios, resultDir)
{
  file <- file(paste(resultDir, "WeakLearnerPool.txt", sep=""), "w")
  pool <- list()
  
  if (is.null(weakLearnerSeed[[CON_WEAK_LEARNER_TYPES$LR_LASSO]]))
    lassoSkipNonSkipIDs <- NULL
  else
  {
    lassoSkipNonSkipIDs <- 
      matrix(-1, 
             ncol=length(posNegRatios), 
             nrow=nrow(expand.grid(weakLearnerSeed[[CON_WEAK_LEARNER_TYPES$LR_LASSO]])))
  }
    
  
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
        
        # LASSO skip / nonskip IDs
        if (modelType == CON_WEAK_LEARNER_TYPES$LR_LASSO)
        {
          # first row: nonskip ID; 
          # other rows: skip IDs
          lassoSkipNonSkipIDs[iComb, iRatio] <- iLearner
        }
        
        iLearner <- iLearner + 1
      }
    }
    writeLines("", file)
  }
  close(file)
  
  return (list(pool=pool,
               lassoSkipNonSkipIDs=lassoSkipNonSkipIDs))
}

GetLogLambdasUnsorted <- function(weakLearnerPool, lrlassoLearnerIDs)
{
  logLambdas <- rep(1e5, length(lrlassoLearnerIDs))
  for (iLearner in 1:length(lrlassoLearnerIDs))
  {
    learnerID <- lrlassoLearnerIDs[iLearner]
    learnerSignature <- weakLearnerPool[[learnerID]]
    logLambdas[iLearner] <- learnerSignature$hyperParams$logLambda
  }
  
  return (logLambdas)
}

CV_AllWeakLeaners <- function(y, X, 
                              kValiFolds, 
                              posWeightsTrainVali, 
                              weakLearnerPool,
                              lassoSkipNonSkipIDs, 
                              targetRecall,
                              bParallel)
{
  valiFolds <- StratifyEasyDifficultPositives(y, posWeightsTrainVali, kValiFolds)
  
  # 
  
  if (bParallel)
  {
    predsAllLearners <- 
      foreach(
        iLearner=(1:length(weakLearnerPool)), .combine="cbind", 
        .maxcombine=1e5,
        .export=c("TrainAWeakLearner", "CON_WEAK_LEARNER_TYPES", "Swap2MakeFirstPositive",
                  "Train_LR_LASSOs", "Train_A_SVM_LIN", "Train_A_SVM_RAD", 
                  "Train_A_RF_BREIMAN", "Train_A_RF_CI", "PredictWithAWeakLearner", 
                  "Predict_LR_LASSOs", "Predict_SVM", "Predict_RF_BREIMAN", "Predict_RF_CI"),
        .packages=c("glmnet", "e1071", "randomForest", "party", 
                    "pROC", "pROC")
        ) %dopar%
        {
          # if LASSO and this round is the representative of this posNegRatio, 
          # then compute for all hyper parameters with this posNegRatio
          if (iLearner %in% lassoSkipNonSkipIDs[1, ]) # first row contains nonSkipIDs
          {
            firstLearnerSignature <- weakLearnerPool[[iLearner]]
            
            learnerIDsThisPNRatio <- 
              lassoSkipNonSkipIDs[, lassoSkipNonSkipIDs[1,]==iLearner]
            nLambdas <- nrow(lassoSkipNonSkipIDs)
            logLambdasUnsorted <- 
              GetLogLambdasUnsorted(weakLearnerPool, learnerIDsThisPNRatio)
            
            # the last row contains the learner IDs
            predsAllDataLASSOs <- matrix(1e5, nrow=nrow(X)+1, ncol=nLambdas)
            
            for (iFold in 1:kValiFolds)
            {
              trainIDs <- valiFolds[[iFold]]
              valiIDs <- which(!((1:length(y)) %in% trainIDs))
              yTrain <- y[trainIDs]
              XTrain <- X[trainIDs,]
              # validation data extracted before subsampling training
              XVali <- X[-trainIDs,]
              yVali <- y[-trainIDs]
              
              # sub-sample a fraction of the negatives
              
              trainIDsPos <- trainIDs[yTrain == 1]
              trainIDsNeg <- trainIDs[yTrain == 0]
              nNegs2Sample <- 
                ceiling(length(trainIDsPos) / firstLearnerSignature$posNegRatio)
              if (nNegs2Sample < length(trainIDsNeg))
              {
                trainIDsNegSubSampled <- 
                  sample(trainIDsNeg)[1:nNegs2Sample]
                trainIDs <- c(trainIDsPos, trainIDsNegSubSampled)
              }
              
              yTrain <- y[trainIDs]
              XTrain <- X[trainIDs, ]
              posWeightsTrain <- posWeightsTrainVali[trainIDs]
              
              # 
              model <- 
                Train_LR_LASSOs(y=yTrain, X=XTrain, 
                                posWeights=posWeightsTrain, 
                                logLambdasUnsorted=logLambdasUnsorted)
              
              predsAllDataLASSOs[valiIDs, ] <- 
                Predict_LR_LASSOs(model, XVali, logLambdasUnsorted)
            }
            
            # last element is the learner ID
            predsAllDataLASSOs[nrow(predsAllDataLASSOs), ] <- learnerIDsThisPNRatio
            
            return (predsAllDataLASSOs)
            
          } else if (iLearner %in% lassoSkipNonSkipIDs[2:nrow(lassoSkipNonSkipIDs), ]) 
          # if LASSO and this round is NOT the representative of this posNegRatio, 
          # don't compute anything (skip it)
          {
            return (NULL)
            
          } else # if not LASSO, compute one learner in every foreach round
          {
            learnerSignature <- weakLearnerPool[[iLearner]]
            
            # the last element contains the learner ID
            predsAllData <- rep(1e5, nrow(X)+1)
            
            for (iFold in 1:kValiFolds)
            {
              trainIDs <- valiFolds[[iFold]]
              valiIDs <- which(!((1:length(y)) %in% trainIDs))
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
                trainIDs <- c(trainIDsPos, trainIDsNegSubSampled)
              }
              
              yTrain <- y[trainIDs]
              XTrain <- X[trainIDs, ]
              posWeightsTrain <- posWeightsTrainVali[trainIDs]
              
              # 
              model <- TrainAWeakLearner(yTrain, XTrain, posWeightsTrain,
                                         learnerSignature)
              
              predsAllData[valiIDs] <- 
                PredictWithAWeakLearner(model, XVali, learnerSignature)
              # last element is the learner ID
              predsAllData[length(predsAllData)] <- iLearner
            }
            
            return (predsAllData)
          }
        }
  } else 
  {
    predsAllLearners <- 
      foreach(
        iLearner=(1:length(weakLearnerPool)), .combine="cbind", 
        .maxcombine=1e5,
        .packages=c("glmnet", "e1071", "randomForest", "party", 
                    "pROC", "pROC")
      ) %do%
      {
        # if LASSO and this round is the representative of this posNegRatio, 
        # then compute for all hyper parameters with this posNegRatio
        if (iLearner %in% lassoSkipNonSkipIDs[1, ]) # first row contains nonSkipIDs
        {
          firstLearnerSignature <- weakLearnerPool[[iLearner]]
          
          learnerIDsThisPNRatio <- 
            lassoSkipNonSkipIDs[, lassoSkipNonSkipIDs[1,]==iLearner]
          nLambdas <- nrow(lassoSkipNonSkipIDs)
          logLambdasUnsorted <- 
            GetLogLambdasUnsorted(weakLearnerPool, learnerIDsThisPNRatio)
          
          # the last row contains the learner IDs
          predsAllDataLASSOs <- matrix(1e5, nrow=nrow(X)+1, ncol=nLambdas)
          
          for (iFold in 1:kValiFolds)
          {
            trainIDs <- valiFolds[[iFold]]
            valiIDs <- which(!((1:length(y)) %in% trainIDs))
            yTrain <- y[trainIDs]
            XTrain <- X[trainIDs,]
            # validation data extracted before subsampling training
            XVali <- X[-trainIDs,]
            yVali <- y[-trainIDs]
            
            # sub-sample a fraction of the negatives
            
            trainIDsPos <- trainIDs[yTrain == 1]
            trainIDsNeg <- trainIDs[yTrain == 0]
            nNegs2Sample <- 
              ceiling(length(trainIDsPos) / firstLearnerSignature$posNegRatio)
            if (nNegs2Sample < length(trainIDsNeg))
            {
              trainIDsNegSubSampled <- 
                sample(trainIDsNeg)[1:nNegs2Sample]
              trainIDs <- c(trainIDsPos, trainIDsNegSubSampled)
            }
            
            yTrain <- y[trainIDs]
            XTrain <- X[trainIDs, ]
            posWeightsTrain <- posWeightsTrainVali[trainIDs]
            
            # 
            model <- 
              Train_LR_LASSOs(y=yTrain, X=XTrain, 
                              posWeights=posWeightsTrain, 
                              logLambdasUnsorted=logLambdasUnsorted)
            
            predsAllDataLASSOs[valiIDs, ] <- 
              Predict_LR_LASSOs(model, XVali, logLambdasUnsorted)
          }
          
          # last element is the learner ID
          predsAllDataLASSOs[nrow(predsAllDataLASSOs), ] <- learnerIDsThisPNRatio
          
          return (predsAllDataLASSOs)
          
        } else if (iLearner %in% lassoSkipNonSkipIDs[2:nrow(lassoSkipNonSkipIDs), ]) 
          # if LASSO and this round is NOT the representative of this posNegRatio, 
          # don't compute anything (skip it)
        {
          return (NULL)
          
        } else # if not LASSO, compute one learner in every foreach round
        {
          learnerSignature <- weakLearnerPool[[iLearner]]
          
          # the last element contains the learner ID
          predsAllData <- rep(1e5, nrow(X)+1)
          
          for (iFold in 1:kValiFolds)
          {
            trainIDs <- valiFolds[[iFold]]
            valiIDs <- which(!((1:length(y)) %in% trainIDs))
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
              trainIDs <- c(trainIDsPos, trainIDsNegSubSampled)
            }
            
            yTrain <- y[trainIDs]
            XTrain <- X[trainIDs, ]
            posWeightsTrain <- posWeightsTrainVali[trainIDs]
            
            # 
            model <- TrainAWeakLearner(yTrain, XTrain, posWeightsTrain,
                                       learnerSignature)
            
            predsAllData[valiIDs] <- 
              PredictWithAWeakLearner(model, XVali, learnerSignature)
            # last element is the learner ID
            predsAllData[length(predsAllData)] <- iLearner
          }
          
          return (predsAllData)
        }
      }
  }
  
  # sort the predictions from different learners according to their IDs
  predsAllLearners <- predsAllLearners[, order(predsAllLearners[nrow(predsAllLearners),])]
  # remove the last row learner IDs
  predsAllLearners <- predsAllLearners[1:(nrow(predsAllLearners)-1), ]
  
  # before returning, select the top 5% using accuracy + independence
  winnerPortion <- 0.05
  threshold <- 0.6
  winnerIndices <- SelectWinners(predsAllLearners, y, targetRecall, 
                                 winnerPortion, threshold)
  
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
        iWinner=(1:length(winnerIndices)), 
        .maxcombine=1e5,
        .export=c("TrainAWeakLearner", "CON_WEAK_LEARNER_TYPES", "Swap2MakeFirstPositive",
                  "Train_A_LR_LASSO", "Train_A_SVM_LIN", "Train_A_SVM_RAD", 
                  "Train_A_RF_BREIMAN", "Train_A_RF_CI"),
        .packages=c("glmnet", "e1071", "randomForest", "party", 
                    "pROC", "pROC")
        ) %dopar%
        {
          learnerSignature <- weakLearnerPool[[winnerIndices[iWinner]]]
          
          # sub-sample a fraction of the negatives
          
          trainIDs <- 1:length(y)
          trainIDsPos <- trainIDs[y == 1]
          trainIDsNeg <- trainIDs[y == 0]
          nNegs2Sample <- 
            ceiling(length(trainIDsPos) / learnerSignature$posNegRatio)
          if (nNegs2Sample < length(trainIDsNeg))
          {
            trainIDsNegSubSampled <- 
              sample(trainIDsNeg)[1:nNegs2Sample]
            trainIDs <- c(trainIDsPos, trainIDsNegSubSampled)
          }
            
          posWeightsTrain <- posWeightsTrainVali[trainIDs]
          
          model <- TrainAWeakLearner(y[trainIDs], X[trainIDs, ], 
                                     posWeightsTrain, learnerSignature)
          
          return (list(model=model, 
                       signature=learnerSignature))
        }
  } else 
  {
    learners <- 
      foreach(
        iWinner=(1:length(winnerIndices)), 
        .maxcombine=1e5,
        .packages=c("glmnet", "e1071", "randomForest", "party", 
                    "pROC", "pROC")
        ) %do%
        {
          learnerSignature <- weakLearnerPool[[winnerIndices[iWinner]]]
          
          # sub-sample a fraction of the negatives
          
          trainIDs <- 1:length(y)
          trainIDsPos <- trainIDs[y == 1]
          trainIDsNeg <- trainIDs[y == 0]
          nNegs2Sample <- 
            ceiling(length(trainIDsPos) / learnerSignature$posNegRatio)
          if (nNegs2Sample < length(trainIDsNeg))
          {
            trainIDsNegSubSampled <- 
              sample(trainIDsNeg)[1:nNegs2Sample]
            trainIDs <- c(trainIDsPos, trainIDsNegSubSampled)
          }
          
          posWeightsTrain <- posWeightsTrainVali[trainIDs]
          
          model <- TrainAWeakLearner(y[trainIDs], X[trainIDs, ], 
                                     posWeightsTrain, learnerSignature)
          
          return (list(model=model, 
                       signature=learnerSignature))
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
        iLearner=(1:length(weakLearners)), .combine="cbind", 
        .maxcombine=1e5, 
        .export=c("PredictWithAWeakLearner", "CON_WEAK_LEARNER_TYPES", "Predict_LR_LASSO",
                  "Predict_SVM", "Predict_RF_BREIMAN", "Predict_RF_CI"),
        .packages=c("glmnet", "e1071", "randomForest", "party", 
                    "pROC", "pROC")
        ) %dopar%
      {
        preds <- 
          PredictWithAWeakLearner(weakLearners[[iLearner]]$model, 
                                  XEval, weakLearners[[iLearner]]$signature)
        
        return (preds)
      }
  } else 
  {
    predsAllWeakLearners <- 
      foreach(
        iLearner=(1:length(weakLearners)), .combine="cbind", 
        .maxcombine=1e5,
        .packages=c("glmnet", "e1071", "randomForest", "party", 
                    "pROC", "pROC")
      ) %do%
      {
        preds <- 
          PredictWithAWeakLearner(weakLearners[[iLearner]]$model, 
                                  XEval, weakLearners[[iLearner]]$signature)
        
        return (preds)
      }
  }
  
  if (is.null(dim(predsAllWeakLearners)))
    preds <- predsAllWeakLearners
  else
    preds <- apply(predsAllWeakLearners, 1, mean)
  
  return (preds)
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
    writeLines(paste(winnerLearners[[iLearner]]$signature$type, 
                     ", ", sep=""), 
               sep="", fileSummary)
    paramNames <- names(winnerLearners[[iLearner]]$signature$hyperParams)
    paramVals <- winnerLearners[[iLearner]]$signature$hyperParams
    for (iParam in 1:length(paramNames))
    {
      writeLines(paste(paramNames[iParam],
                       ":", paramVals[[paramNames[iParam]]],
                       ", ", sep=""), sep="", fileSummary)
    }
    writeLines(paste("Positive-Negative Training Data Ratio:", 
                     winnerLearners[[iLearner]]$signature$posNegRatio), fileSummary)
    writeLines("", fileSummary)
    
    # the models and signatures
    filename <- paste(resultDir, "Winner", iLearner, "Of", n, 
                      "_EvalFold", iEvalFold, ".rds", sep="")
    saveRDS(winnerLearners[[iLearner]], filename)
  }
  
  close(fileSummary)
}

SaveEvalResult <- function(resultDir, preds, labels, targetRecall)
{
#   write.table(preds, sep=",", 
#               file=paste(resultDir, "preds.csv", sep=""), 
#               col.names=T, row.names=F)
  predsObj <- prediction(predictions=preds[,2], labels=labels)
  perfPR <- performance(predsObj, measure="prec", x.measure="rec")
  write.table(cbind(perfPR@x.values[[1]],perfPR@y.values[[1]]), sep=",", 
              file=paste(resultDir, "PR.csv", sep=""), 
              col.names=c("recall", "precision"),
              row.names=F)
  
  precisionTargetRecall <- 
    approx(perfPR@x.values[[1]], perfPR@y.values[[1]], xout=targetRecall)$y[1]
  write.table(precisionTargetRecall, sep=",", 
              file=paste(resultDir, "precisionTargetRecall.csv", sep=""), 
              col.names=F, row.names=F)
  
  rocObj <- 
    roc(response=as.vector(labels), 
        predictor=as.vector(preds[,2]),
        direction="<")
  auc <- rocObj$auc
  write.table(auc, sep=",", 
              file=paste(resultDir, "auc.csv", sep=""), 
              col.names=F, row.names=F)
  
  perfROC <- performance(predsObj, measure = "tpr", x.measure = "fpr") 
  write.table(cbind(perfROC@x.values[[1]],perfROC@y.values[[1]]), sep=",", 
              file=paste(resultDir, "ROC.csv", sep=""), 
              col.names=c("fpr", "tpr"),
              row.names=F)
}

# SelfEvalModel evaluates the trained ensemble on a subset of the input data
SelfEvalModel <- function(y, X, posWeights,
                          kEvalFolds, kValiFolds, 
                          weakLearnerSeed,
                          posNegRatios,
                          targetRecall,
                          obsIDs=NULL,
                          bParallel, 
                          resultDir)
{
  if (is.null(colnames(X)))
    stop("Error! X must have colnames.")
  
  if (length(unique(colnames(X))) != ncol(X))
    stop("Error! X has duplicated variable names.")
  
  #
  ## transform to a vector with the same length as y, for the ease of stratification
  
  posWeightsAllData <- rep(0, length(y))
  posWeightsAllData[y == 1] <- posWeights
  
  #
  ## stratify for ensemble evaluation 
  
  # a more sophisticated stratification: every fold has similar-sized
  # 'purer / easier' positive patients;
  evalFolds <- StratifyEasyDifficultPositives(y, posWeightsAllData, kEvalFolds)
  
  weakLearnerInventoryStruct <- 
    GenWeakLearnerPool(weakLearnerSeed, posNegRatios, resultDir)
  
  #
  ## ensemble evaluation
  
  predsAllData <- rep(1e5,length(y))
  
  for (iEvalFold in 1:length(evalFolds))
  {
    print(paste("eval fold:", iEvalFold))
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
    
    minMaxValMat <- apply(XTrainVali, 2, FindMinMaxOneVar)
    colnames(minMaxValMat) <- colnames(XTrainVali)
    XTrainVali <- MinMaxNormaliseAllVarsWithGivenMinMaxValues(XTrainVali, minMaxValMat)
    # save for normalisation in future tests
    fileMinMaxMat <- paste(resultDir, "minmax_EvalFold", iEvalFold, ".csv", sep="")
    write.table(minMaxValMat, sep=",", file=fileMinMaxMat, row.names=F)
    # standardise the evaluation data accordingly
    XEval <- MinMaxNormaliseAllVarsWithGivenMinMaxValues(XEval, minMaxValMat)

    # 
    
    posWeightsTrainVali <- posWeightsAllData[trainValiIDs]
    
    winnerIndices <- 
      CV_AllWeakLeaners(y=yTrainVali, X=XTrainVali, 
                        kValiFolds=kValiFolds, 
                        posWeightsTrainVali=posWeightsTrainVali, 
                        weakLearnerPool=weakLearnerInventoryStruct$pool,
                        lassoSkipNonSkipIDs=weakLearnerInventoryStruct$lassoSkipNonSkipIDs,
                        targetRecall=targetRecall,
                        bParallel=bParallel)
    
    # train the selected using all the trainVali data
    winnerLearners <- 
      TrainWinnerLearners(y=yTrainVali, X=XTrainVali, 
                          posWeightsTrainVali=posWeightsTrainVali, 
                          weakLearnerPool=weakLearnerInventoryStruct$pool,
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
  
  SaveEvalResult(resultDir, preds=predsAllData, labels=y, 
                 targetRecall=targetRecall)
}