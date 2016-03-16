library(doParallel)
library(parallel)

source("functions/Ensemble.R")
source("functions/MinMaxNormalise.R")
source("functions/SplitData.R")

ReadWeakLearnersOneFold <- function(modelDir, iFold)
{
  nLearners <- 
    length(list.files(path=modelDir, 
                      pattern=paste("_EvalFold", iFold, ".rds", sep="")))
  
  learners <- list()
  for (iLearner in 1:nLearners)
  {
    filename <- paste(modelDir, "Winner", iLearner, "Of", nLearners, "_EvalFold", iFold, ".rds", 
                      sep="")
    learners[[iLearner]] <- readRDS(filename)
  }
  
  return (learners)
}

ApplyEnsemble <- function(arglist)
{
  #
  ## parse
  
  bParallel <- arglist$bParallel
  if (bParallel)
  {
    if (is.null(arglist$nCores2Use))
      nCores2Use <- detectCores() - 1
    else
      nCores2Use <- arglist$nCores2Use
    
    cl <- makeCluster(nCores2Use)
    registerDoParallel(cl, cores = nCores2Use)
  }
  
  dataDir <- arglist$dataDir
  modelDir <- arglist$modelDir
  
  if (is.null(arglist$targetRecall))
    stop("Error! The target recall (at which precision will be maximised) must be specified. ")
  targetRecall <- arglist$targetRecall
  
  #
  
  ptm <- proc.time()
  if (is.null(arglist$resultDir))
  {
    timeStamp <- as.character(Sys.time())
    timeStamp <- gsub(":", ".", timeStamp)  # replace ":" by "."
    resultDir <- paste("./Results/", timeStamp, "/", sep = '')
    dir.create(resultDir, showWarnings = TRUE, recursive = TRUE, mode = "0777")
  } else
    resultDir <- arglist$resultDir
  
  
  #
  ## save all the input arguments
  
  fileArgList <- file(paste(resultDir, "arglist.txt", sep=""), "w")
  for (argName in names(arglist))
  {
    writeLines(paste(argName,": ", arglist[[argName]], "\n", sep=""), fileArgList)
  }
  close(fileArgList)
  
  #
  ## read and prepare data
  
  dataset <- read.csv(paste(dataDir, "CleanDataB4Similarity.csv", sep=""), 
                      header=TRUE, sep=",", check.names=FALSE)
  #   if (bBadPosWeighting)
  #   {
  #     if ((posWeightMethod == CON_POS_WEIGHT_METHOD$SIMILARITY_SCORE) & 
  #       (!any(colnames(dataset) %in% c("PATIENT_ID"))))
  #       stop(paste("Error! To use the similarity score to weight positives, the ", 
  #            "input positive patients must have IDs.", sep=""))
  #   }
  # y <- as.factor(dataset$HAE)
  y <- dataset$HAE
  X <- data.matrix(dataset[, (colnames(dataset) != "HAE")])
  
  # a little cleaning
  sums <- apply(X, 1, sum)
  X <- X[(sums != 0), ] # remove all-zero patients
  
  if (any(colnames(X) == "PATIENT_ID"))
  {
    patientIDs <- X[, "PATIENT_ID"]
    X <- X[, (colnames(X) != "PATIENT_ID")]
  } else
    patientIDs <- NULL
  
  X <- X[, colnames(X) != "LOOKBACK_DAYS"]
  
  
  #
  ## identify how many evluation folds were there
  
  kFolds <- length(list.files(path=modelDir, pattern="minmax_"))
  print(paste("kFolds:", kFolds))
  
  # split the test folds accordingly so that every datum doesn't need to be predicted multipe times
  
  folds <- ManualSampleFolds(nrow(X), kFolds)
  
  #
  ## for every evaluation folds in the training, validation and testing stage 
  ## (because kEvalFolds ensembles were saved)
  
  predsAllData <- rep(1e5, nrow(X))
  
  for (iFold in 1:kFolds)
  {
    IDsThisFold <- which(!((1:nrow(X)) %in% folds[[iFold]]))
    XThisFold <- X[IDsThisFold, ]
    
    #
    ## read the models
    
    # normalisation according to the saved min / max values
    
    minMaxMat <- read.csv(paste(modelDir, "minmax_EvalFold", iFold, ".csv", sep=""), 
                          header=TRUE, sep=",", check.names=FALSE)
    XThisFold <- MinMaxNormaliseAllVarsWithGivenMinMaxValues(XThisFold, minMaxMat)
    
    # models
    
    weakLearnersThisFold <- ReadWeakLearnersOneFold(modelDir, iFold)
    
    #
    ## applying the ensemble
    
    predsAllData[IDsThisFold] <- 
      PredictWithAnEnsemble(weakLearners=weakLearnersThisFold, XEval=XThisFold, 
                            bParallel=bParallel)
  }
  
  # save the predictions
  
  predsAllData <- cbind(patientIDs, predsAllData)
  colnames(predsAllData) <- c("ObservationID", "Prediction")
  
  SaveEvalResult(resultDir, predsAllData, y, targetRecall)
  
  
  runtime <- proc.time() - ptm
  print(paste("Time elapsed:", round(runtime[3],1), "seconds."))
  fileRunTime <- file(paste(resultDir, "runtime.txt", sep=""), "w")
  writeLines(paste(round(runtime["elapsed"],1), " seconds.", "\n", sep=""), fileRunTime)
  close(fileRunTime)
  
  if (bParallel)
  {
    stopCluster(cl)
  }
}