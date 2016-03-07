library(glmnet)
library(e1071)
library(randomForest)
library(party)


CON_WEAK_LEARNER_TYPES <- list(LR_LASSO=1,
                               LR_ENET=2,
                               SVM_LIN=3,
                               SVM_RAD=4,
                               RF_BREIMAN=5, 
                               RF_CI=6)


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

Train_A_LR_LASSO <- function(y, X, observationWeights, hyperParams)
{
  logLambdaSeq <- c(hyperParams$logLambda-1, 
                    hyperParams$logLambda, 
                    hyperParams$logLambda+1)
  lambdaSeq <- exp(logLambdaSeq)
  
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
               cost=exp(hyperParams$logC),
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
               cost=exp(hyperParams$logC), gamma=exp(hyperParams$logGamma),
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
  data <- cbind(as.factor(y), X)

  model <- cforest(y ~ ., data=data, weights=observationWeights,
                   controls = cforest_control(savesplitstats = FALSE,
                                              ntree=hyperParams$nTrees, 
                                              mtry=hyperParams$nVarsPerSplit, 
                                              fraction=1))
  
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
    trainIDs <- (1:length(y))[!(1:length(y)) %in% posIDs2Remove]
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
    stop(paste("Error! Weak learner type ", learnerSignature, " is not supported."))
  
  return (model)
}


################################################################################
#
# Prediction
#
################################################################################


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
  rawPreds <- predict(model, newdata=XTest, type='prob')
  probs <- do.call(rbind, rawPreds)[,2]
}

PredictWithAWeakLearner <- function(model, X, learnerSignature)
{
  if (learnerSignature$type == CON_WEAK_LEARNER_TYPES$LR_LASSO)
  {
    preds <- Predict_LR_LASSO(model=model, X=X, 
                              lambda=exp(learnerSignature$hyperParams$logLambda))
    
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
    stop(paste("Error! Weak learner type ", learnerSignature, " is not supported."))
  
  return (preds)
}