rm(list=ls())

source("functions/ModelStack.R")

main.arglist <- list()
main.arglist$dataDir <- "../../../../Results/2016-02-26/2016-02-26 17.33.20/"

main.arglist$bSelfEval <- T
main.arglist$selfEval.kEvalFolds <- 2
main.arglist$kValiFolds <- 3

main.arglist$posWeightMethod <- CON_POS_WEIGHT_METHOD$SIMILARITY_SCORE
main.arglist$similarityScoreFile <- 
  "../../../../Results/2016-03-03/2016-03-03 16.58.02/SimilarityScoreResult.csv"

# hyper-parameter grid

main.arglist$bParallel <- F

main.arglist$bBadPosWeighting <- T

main.arglist$weakLearnerSeed <- 
  list(LR_LASSO=list(logLambda=(-4:4)),
       SVM_LIN=list(logC=(-4:2)),
       SVM_RAD=list(logC=(-4:2), 
                    logGamma=seq(from=-4, to=1, by=1)),
       RF_BREIMAN=list(nTrees=c(50,100,200),
                       nVarsPerSplit=c(5, 10, 20),
                       nodeSize=c(1,2,4,8)),
       RF_CI=list(nTrees=c(50,100,200),
                  nVarsPerSplit=c(5, 10, 20)))
main.arglist$posNegRatios <- seq(from=0.5, to=5, by=0.5)

ModelStack(main.arglist)