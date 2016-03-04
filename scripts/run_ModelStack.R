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

ModelStack(main.arglist)