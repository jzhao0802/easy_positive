library(FNN)
library(dplyr)

CON_POS_WEIGHT_METHOD <- list(KNN=1,
                              SIMILARITY_SCORE=2)

#
# Elements in the output weight vector are in the same order as all the positives
# in the input input data
#
WeightPositives <- function(y, X, posPatientIDsInData, 
                            posWeightMethod, similarityScoreFile="", 
                            resultDir)
{
  if (posWeightMethod == CON_POS_WEIGHT_METHOD$KNN)
  {
    k <- 5
    kNNIDs <- (get.knnx(data=X, query=X[y==1,], k=k, algo="kd_tree"))$nn.index
    kNNIDs <- kNNIDs[, 2:ncol(kNNIDs)] # self
    
    weights <- rep(0, nrow(kNNIDs))
    for (iPos in 1:length(weights))
    {
      neighbours <- y[kNNIDs[iPos, ]]
      weights[iPos] <- sum(neighbours==1) / k
    }
    # normalise the weights so that max(weights) = 1
    if (max(weights) == 0)
      stop(paste("Error! Weights for positive data cannot be computed from KNN ", 
                 "because none of the positive patients has any positive neighbours."))
    weights <- weights / max(weights)
    weightDF <- as.data.frame(cbind(posPatientIDsInData, weights))
    colnames(weightDF) <- c("PATIENT_ID", "weight")
    
  } else if (posWeightMethod == CON_POS_WEIGHT_METHOD$SIMILARITY_SCORE)
  {
    if (is.null(posPatientIDsInData))
    {
      stop(paste("Error! In order to use SIMILARITY_SCORE for WeightPositives, ", 
                 "the input data must have the column of 'PATIENT_ID'."))
    }
    print("Warning! This is just a temporary solution for SIMILARITY_SCORE. ")
    print("A formal solution will need to merge positive patients via PATIENT_ID.")
    weights <- read.csv(similarityScoreFile, header=T, check.names=F, sep=",")
    
    # make sure that the order is the same as the positives in the input data
    posPatientIDsInData <- matrix(posPatientIDsInData, ncol=1)
    colnames(posPatientIDsInData) <- "PATIENT_ID"
    posPatientIDsInData <- as.data.frame(posPatientIDsInData)
    colnames(weights) <- c("PATIENT_ID", "weight")
    weights <- as.data.frame(weights)
    weightDF <- left_join(posPatientIDsInData, weights)
  } else
    stop("Error! Invalid posWeightMethod value!")
  
  # save for investigation
  
  # make sure there's no 0 weights, for the ease of identifying weights for
  # positive patients among all patients
  weightDF$weight[weightDF$weight==0] <- 1e-6
  
  write.table(weightDF, sep=",", row.names=F,
              file=paste(resultDir, "posWeights.csv", sep=""))
  
  return (weightDF$weight)
}