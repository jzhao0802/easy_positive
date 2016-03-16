rm(list=ls())

source("functions/ApplyEnsemble.R")

main.arglist <- list()

main.arglist$runType <- "ApplyEnsemble"

main.arglist$dataDir <- "../../../Results/2016-03-08/2016-03-08 15.57.03/"

main.arglist$modelDir <- "Results/2016-03-16 17.45.37/"

# hyper-parameter grid

main.arglist$bParallel <- F

main.arglist$targetRecall <- 0.05

ApplyEnsemble(main.arglist)