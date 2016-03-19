library(glmnet)
library(e1071)
library(randomForest)
library(party)


CON_WEAK_LEARNER_TYPES <- list(LR_LASSO="LR_LASSO",
                               LR_ENET="LR_ENET",
                               SVM_LIN="SVM_LIN",
                               SVM_RAD="SVM_RAD",
                               RF_BREIMAN="RF_BREIMAN", 
                               RF_CI="RF_CI")


################################################################################
#
# training
#
################################################################################


Swap2MakeFirstPositive <- function(yTrain, trainIDs)
{
  firstPosLoc <- (which(yTrain==1))[1]
  tmp <- trainIDs[1]
  trainIDs[1] <- trainIDs[firstPosLoc]
  trainIDs[firstPosLoc] <- tmp
  
  return (trainIDs)
}

Train_LR_LASSOs <- function(y, X, posWeights, logLambdasUnsorted)
{
  posNegRatio <- sum(y==1) / (length(y)-sum(y==1))
  observationWeights <- rep(1, length(y))
  observationWeights[y==1] <- observationWeights[y==1] / posNegRatio * posWeights[posWeights>0]
  
  logLambdasSorted <- order(logLambdasUnsorted, decreasing=T)
  # logLambdaSeq <- c(10, (hyperParams$logLambda-3):(hyperParams$logLambda+3))
  lambdaSeq <- 10^(logLambdasSorted)
  
  model <- glmnet(X,y, family="binomial", 
                  weights=observationWeights,
                  alpha=1, lambda=lambdaSeq)
  
  return (model)
}

Train_A_LR_LASSO <- function(y, X, observationWeights, hyperParams)
{
  logLambdaSeq <- c((hyperParams$logLambda-3):(hyperParams$logLambda+3))
  lambdaSeq <- 10^(logLambdaSeq)
  
  model <- glmnet(X,y, family="binomial", 
                  weights=observationWeights,
                  alpha=1, lambda=lambdaSeq)
  
  return (model)
}

Train_A_SVM_LIN <- function(y, X, classWeights, hyperParams)
{
  trainIDs <- Swap2MakeFirstPositive(y, 1:length(y))
  y <- y[trainIDs]
  X <- X[trainIDs,]
  
  model <- svm(y=y, x=X, 
               scale=rep(F, length(y)),
               type="C-classification", kernel="linear",
               cost=10^(hyperParams$logC),
               class.weights=classWeights, probability=T)
  
  return (model)
}

Train_A_SVM_RAD <- function(y, X, classWeights, hyperParams)
{
  trainIDs <- Swap2MakeFirstPositive(y, 1:length(y))
  y <- y[trainIDs]
  X <- X[trainIDs,]
  
  model <- svm(y=y, x=X, 
               scale=rep(F, length(y)),
               type="C-classification", kernel="radial",
               cost=10^(hyperParams$logC), gamma=10^(hyperParams$logGamma),
               class.weights=classWeights, probability=T)
  
  return (model)
}

Train_A_RF_BREIMAN <- function(y, X, classWeights, hyperParams)
{
  model <- randomForest(y=as.factor(y), x=X, 
                        classwt=classWeights,
                        ntree=hyperParams$nTrees,
                        mtry=hyperParams$nVarsPerSplit,
                        nodesize=hyperParams$nodeSize)
  return (model)
}

Train_A_RF_CI <- function(y, X, observationWeights, hyperParams)
{
  dataTrain <- cbind(as.factor(y), as.data.frame(X))
  colnames(dataTrain)[1] <- "y"
  
  model <- cforest(y ~ ., data=dataTrain, weights=observationWeights,
                   controls = cforest_control(ntree=hyperParams$nTrees, 
                                              mtry=hyperParams$nVarsPerSplit, 
                                              fraction=0.99))
  
  return (model)
}

TrainAWeakLearner <- function(y, X, 
                              posWeights,
                              learnerSignature)
{
  if ((learnerSignature$type == CON_WEAK_LEARNER_TYPES$LR_LASSO) | 
      learnerSignature$type == CON_WEAK_LEARNER_TYPES$RF_CI)
    # supports different weights for individual observations
  {
    posNegRatio <- sum(y==1) / (length(y)-sum(y==1))
    weights <- rep(1, length(y))
    weights[y==1] <- weights[y==1] / posNegRatio * posWeights[posWeights>0]
  } else
    # supports only class weights
  {
    posIDs2Remove <- (y==1) & (posWeights <= 0.5)
    trainIDs <- (1:length(y))[!((1:length(y)) %in% posIDs2Remove)]
    y <- y[trainIDs]
    X <- X[trainIDs,]
    weights <- 1/table(y)
  }
  
  
  if (learnerSignature$type == CON_WEAK_LEARNER_TYPES$LR_LASSO)
  {
    model <- Train_A_LR_LASSO(y=y, X=X, observationWeights=weights, 
                              hyperParams=learnerSignature$hyperParams)
    
  } else if (learnerSignature$type == CON_WEAK_LEARNER_TYPES$SVM_LIN)
  {
    model <- Train_A_SVM_LIN(y=y, X=X, classWeights=weights, 
                             hyperParams=learnerSignature$hyperParams)
    
  } else if (learnerSignature$type == CON_WEAK_LEARNER_TYPES$SVM_RAD)
  {
    model <- Train_A_SVM_RAD(y=y, X=X, classWeights=weights, 
                             hyperParams=learnerSignature$hyperParams)
    
  } else if (learnerSignature$type == CON_WEAK_LEARNER_TYPES$RF_BREIMAN)
  {
    model <- Train_A_RF_BREIMAN(y=y, X=X, classWeights=weights, 
                                hyperParams=learnerSignature$hyperParams)
    
  } else if (learnerSignature$type == CON_WEAK_LEARNER_TYPES$RF_CI)
  {
    model <- Train_A_RF_CI(y=y, X=X, observationWeights=weights, 
                           hyperParams=learnerSignature$hyperParams)
    
  } else
    stop(paste("Error! Weak learner type ", learnerSignature$type, " is not supported."))
  
  return (model)
}


################################################################################
#
# Prediction
#
################################################################################


Predict_LR_LASSOs <- function(model, X, logLambdasUnsorted)
{
  preds <- 
    predict(model, newx = X, type="response", s=10^logLambdasUnsorted)
  
  return (preds)
}

Predict_LR_LASSO <- function(model, X, lambda)
{
  preds <- 
    predict(model, newx = X, type="response", s=lambda)
  
  return (preds)
}

Predict_SVM <- function(model, X)
{
  rawPreds <- predict(model, newdata=X, probability=T)
  probs <- (attr(rawPreds, "probabilities", exact=T))[,1]
  
  return (probs)
}

Predict_RF_BREIMAN <- function(model, X)
{
  rawPreds <- predict(model, newdata=X, type='prob')
  probs <- rawPreds[, 2]
  
  return (probs)
}

Predict_RF_CI <- function(model, X)
{
  rawPreds <- predict(model, newdata=as.data.frame(X), type='prob')
  probs <- do.call(rbind, rawPreds)[,2]
  
  return (probs)
}

PredictWithAWeakLearner <- function(model, X, learnerSignature)
{
  if (learnerSignature$type == CON_WEAK_LEARNER_TYPES$LR_LASSO)
  {
    preds <- Predict_LR_LASSO(model=model, X=X, 
                              lambda=10^(learnerSignature$hyperParams$logLambda))
    
  } else if ((learnerSignature$type == CON_WEAK_LEARNER_TYPES$SVM_LIN) | 
             (learnerSignature$type == CON_WEAK_LEARNER_TYPES$SVM_RAD))
  {
    preds <- Predict_SVM(model=model, X=X)
    
  } else if (learnerSignature$type == CON_WEAK_LEARNER_TYPES$RF_BREIMAN)
  {
    preds <- Predict_RF_BREIMAN(model=model, X=X)
    
  } else if (learnerSignature$type == CON_WEAK_LEARNER_TYPES$RF_CI)
  {
    preds <- Predict_RF_CI(model=model, X=X)
    
  } else
    stop(paste("Error! Weak learner type ", learnerSignature$type, " is not supported."))
  
  return (preds)
}