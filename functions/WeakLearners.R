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

TRAIN_A_LR_LASSO <- function(y, X, observationWeights, hyperParams)
{
  logLambdaSeq <- c(hyperParams$logLambda-1, 
                    hyperParams$logLambda, 
                    hyperParams$logLambda+1)
  lambdaSeq <- exp(logLambdaSeq)
  
  model <- glmnet(X,y, family="binomial", 
                  weights=weightVec,
                  alpha=1, lambda=lambdaSeq)
  
  return (model)
}

TRAIN_A_SVM_LIN <- function(y, X, classWeights, hyperParams)
{
  model <- svm(y=yTrain, x=XTrain, 
               scale=rep(F, length(y)),
               type="C-classification", kernel="linear",
               cost=exp(hyperParams$logC),
               class.weights=classWeights, probability=F)
  
  return (model)
}

TRAIN_A_SVM_RAD <- function(y, X, classWeights, hyperParams)
{
  model <- svm(y=yTrain, x=XTrain, 
               scale=rep(F, length(y)),
               type="C-classification", kernel="radial",
               cost=exp(hyperParams$logC), gamma=exp(hyperParams$logGamma),
               class.weights=classWeights, probability=F)
  
  return (model)
}

TRAIN_A_RF_BREIMAN <- function(y, X, classWeights, hyperParams)
{
  model <- randomForest(x=y,y=X,
                        ntree=hyperParams$nTrees,
                        mtry=hyperParams$nVarsPerSplit,
                        nodesize=hyperParams$nodeSize)
  
  return (model)
}

TRAIN_A_RF_CI <- function(y, X, observationWeights, hyperParams)
{
  data <- cbind(y, X)

  model <- cforest(y ~ ., data=data, weights=weightVec,
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
    yTmp <- y[trainIDs]
    trainIDs <- Swap2MakeFirstPositive(yTmp, (!(1:length(y)) %in% posIDs2Remove))
    y <- y[trainIDs]
    X <- X[trainIDs,]
    weights <- 1/table(y)
  }
  
  
  if (learnerSignature$type == CON_WEAK_LEARNER_TYPES$LR_LASSO)
  {
    model <- TRAIN_A_LR_LASSO(y=y, X=X, observationWeights=weights, 
                              hyperParams=learnerSignature$hyperParams)
    
  } else if (learnerSignature$type == CON_WEAK_LEARNER_TYPES$SVM_LIN)
  {
    model <- TRAIN_A_SVM_LIN(y=y, X=X, classWeights=weights, 
                             hyperParams=learnerSignature$hyperParams)
    
  } else if (learnerSignature$type == CON_WEAK_LEARNER_TYPES$SVM_RAD)
  {
    model <- TRAIN_A_SVM_RAD(y=y, X=X, classWeights=weights, 
                             hyperParams=learnerSignature$hyperParams)
    
  } else if (learnerSignature$type == CON_WEAK_LEARNER_TYPES$RF_BREIMAN)
  {
    model <- TRAIN_A_RF_BREIMAN(y=y, X=X, classWeights=weights, 
                        hyperParams=learnerSignature$hyperParams)
    
  } else if (learnerSignature$type == CON_WEAK_LEARNER_TYPES$RF_CI)
  {
    model <- TRAIN_A_RF_CI(y=y, X=X, observationWeights=weights, 
                        hyperParams=learnerSignature$hyperParams)
    
  } else
    stop(paste("Error! Weak learner type ", learnerSignature, " is not supported."))
  
  return (model)
}