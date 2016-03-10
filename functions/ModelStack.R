library(doParallel)
library(parallel)

source("functions/WeightPositives.R")
source("functions/Ensemble.R")

ModelStack <- function(arglist)
{
  #
  ## parse
  
  bBadPosWeighting <- arglist$bBadPosWeighting
  if (bBadPosWeighting)
    print("Warning! Bad positive patient weighting is used. The result may be incorrect!")
  
  weakLearnerSeed <- arglist$weakLearnerSeed
  posNegRatios <- arglist$posNegRatios
  
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
  
  bSelfEval <- arglist$bSelfEval
  if (bSelfEval)
  {
    if (is.null(arglist$selfEval.kEvalFolds))
      stop("Error! selfEval.kEvalFolds must be specified when bSelfEval is set. ")
    selfEval.kEvalFolds <- arglist$selfEval.kEvalFolds
  }
  
  if (is.null(arglist$kValiFolds))
    stop("Error! kValiFolds is missing. ")
  kValiFolds <- arglist$kValiFolds
  
  posWeightMethod <- arglist$posWeightMethod
  if (posWeightMethod == CON_POS_WEIGHT_METHOD$SIMILARITY_SCORE)
  {
    
    if (is.null(arglist$similarityScoreFile))
      stop(paste("Error! similarityScoreFile must be specified when ", 
                 "posWeightMethod is specified as SIMILARITY_SCORE.", sep=""))
    
    
    similarityScoreFile <- arglist$similarityScoreFile
  } else
  {
    similarityScoreFile <- NULL
  }
  
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
    if (argName == "posNegRatios")
    {
      n_candidates <- length(arglist[[argName]])
      start <- arglist[[argName]][1]
      end <- arglist[[argName]][n_candidates]
      writeLines(paste(argName, ": bounds [", start, ", ", end, "], nCandidates: ", 
                       n_candidates, "\n", sep=""), fileArgList)
    } else if (argName == "posWeightMethod")
    {
      value <- 
        names(CON_POS_WEIGHT_METHOD)[
          as.vector(CON_POS_WEIGHT_METHOD) == arglist[[argName]]
          ]
      writeLines(paste(argName,": ", value, "\n", sep=""), fileArgList)
    } else if (argName == "weakLearnerSeed")
    {
      writeLines(paste(argName,": ", sep=""), fileArgList)
      for (modelType in names(arglist[[argName]]))
      {
        writeLines(paste(modelType,"; ", sep=""), sep="", fileArgList)
        for (param in names(arglist[[argName]][[modelType]]))
        {
          n_candidates <- length(arglist[[argName]][[modelType]][[param]])
          start <- arglist[[argName]][[modelType]][[param]][1]
          end <- arglist[[argName]][[modelType]][[param]][n_candidates]
          writeLines(paste(param,": bounds [", start, ", ", end, "], nCandidates: ", 
                           n_candidates, "; ", sep=""), sep="", fileArgList)
        }
        writeLines("", fileArgList)
      }
      writeLines("", fileArgList)
    } else
    {
      writeLines(paste(argName,": ", arglist[[argName]], "\n", sep=""), fileArgList)
    }
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
  y <- as.factor(dataset$HAE)
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
  ## weight every positive
  
  posPatientIDs <- patientIDs[y==1]
  posWeights <- WeightPositives(y, X, posPatientIDsInData=posPatientIDs, 
                                posWeightMethod, 
                                similarityScoreFile, resultDir)
  
  #
  ## modelling
  
  if (bSelfEval)
  {
    print("Self-evaluation modelling..")
    SelfEvalModel(y=y, X=X, posWeights=posWeights, 
                  kEvalFolds=selfEval.kEvalFolds, kValiFolds=kValiFolds, 
                  weakLearnerSeed=weakLearnerSeed,
                  posNegRatios=posNegRatios, 
                  targetRecall=targetRecall, 
                  obsIDs=patientIDs,
                  bParallel=bParallel, 
                  resultDir)
    
  } else
  {
    print("Modelling without evaluation..")
    # ModelWithoutEval(y, X, posWeights)
  }
  
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