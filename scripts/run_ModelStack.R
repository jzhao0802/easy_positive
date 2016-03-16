rm(list=ls())

source("functions/ModelStack.R")

main.arglist <- list()

main.arglist$runType <- "ModelStack"

main.arglist$dataDir <- "../../../Results/2016-03-08/2016-03-08 15.57.03/"

main.arglist$bSelfEval <- T
main.arglist$selfEval.kEvalFolds <- 2
main.arglist$kValiFolds <- 3

main.arglist$posWeightMethod <- CON_POS_WEIGHT_METHOD$KNN
main.arglist$similarityScoreFile <- 
  "../../../Results/2016-03-09/2016-03-09 12.32.26/SimilarityScoreResult.csv"

# hyper-parameter grid

main.arglist$bParallel <- F

main.arglist$bBadPosWeighting <- T

# main.arglist$weakLearnerSeed <- 
#   list(LR_LASSO=list(logLambda=(-4:4)),
#        SVM_LIN=list(logC=(-4:2)),
#        SVM_RAD=list(logC=(-4:2), 
#                     logGamma=seq(from=-4, to=1, by=1)),
#        RF_BREIMAN=list(nTrees=c(50,100,200),
#                        nVarsPerSplit=c(5, 10, 20),
#                        nodeSize=c(1,2,4,8)),
#        RF_CI=list(nTrees=c(50,100,200),
#                   nVarsPerSplit=c(5, 10, 20)))
main.arglist$weakLearnerSeed <- 
  list(LR_LASSO=list(logLambda=(-1:1)))
main.arglist$posNegRatios <- seq(from=0.8, to=1, by=0.2)

main.arglist$targetRecall <- 0.05

ModelStack(main.arglist)