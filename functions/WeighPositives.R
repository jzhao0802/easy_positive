library(FNN)

CON_POS_WEIGHT_METHOD <- list(KNN=1,
                              SIMILARITY_SCORE=2)

WeighPositives <- function(y, X, posWeightMethod, similarityScoreFile="")
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
    return (weights)
  } else if (posWeightMethod == CON_POS_WEIGHT_METHOD$SIMILARITY_SCORE)
  {
    print("Warning! This is just a temporary solution for SIMILARITY_SCORE. ")
    print("A formal solution will need to merge positive patients via PATIENT_ID.")
    probs <- read.csv(similarityScoreFile, header=T, check.names=F, sep=",")
    return (probs)
  } else
    stop("Error! Invalid posWeightMethod value!")
}