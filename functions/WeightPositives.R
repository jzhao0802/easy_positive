library(FNN)

CON_POS_WEIGHT_METHOD <- list(KNN=1,
                              SIMILARITY_SCORE=2)

WeightPositives <- function(y, X, posWeightMethod, similarityScoreFile="", 
                           resultDir)
{
  if (posWeightMethod == CON_POS_WEIGHT_METHOD$KNN)
  {
    k <- 5
    print(paste("nPoses:", sum(y==1)))
    print(paste("nTotal:", nrow(X)))
    kNNIDs <- (get.knnx(data=X, query=X[y==1,], k=k, algo="kd_tree"))$nn.index
    print(paste("class(kNNIDs):", class(kNNIDs)))
    print("dim(kNNIDs):")
    print(dim(kNNIDs))
    kNNIDs <- kNNIDs[, 2:ncol(kNNIDs)] # self
    
    weights <- rep(0, nrow(kNNIDs))
    for (iPos in 1:length(weights))
    {
      neighbours <- y[kNNIDs[iPos, ]]
      weights[iPos] <- sum(neighbours) / k
    }
    # normalise the weights so that max(weights) = 1
    if (max(weights) == 0)
      stop(paste("Error! Weights for positive data cannot be computed from KNN ", 
                 "because none of the positive patients has any positive neighbours."))
    weights <- weights / max(weights)
    
  } else if (posWeightMethod == CON_POS_WEIGHT_METHOD$SIMILARITY_SCORE)
  {
    print("Warning! This is just a temporary solution for SIMILARITY_SCORE. ")
    print("A formal solution will need to merge positive patients via PATIENT_ID.")
    weights <- read.csv(similarityScoreFile, header=T, check.names=F, sep=",")
    
  } else
    stop("Error! Invalid posWeightMethod value!")
  
  # save for investigation
  if (any(colnames(X) == "PATIENT_ID"))
  {
    weightMat <- cbind(X$PATIENT_ID, weights)
    colnames(weightMat) <- c("PATIENT_ID", "weight")
    
  } else
  {
    weightMat <- matrix(weights, ncol=1)
    colnames(weightMat) <- "weight"
  }
  
  # make sure there's no 0 weights, for the ease of identifying positive weights
  weightMat[weightMat==0] <- weightMat[weightMat==0] + 1e-6
  
  write.table(weightMat, sep=",", row.names=F,
              file=paste(resultDir, "posWeights.csv", sep=""))
  
  return (weights)
}