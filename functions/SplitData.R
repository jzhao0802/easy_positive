ManualSampleFolds <- function(nData, kFolds)
{
  indices <- sample(1:nData)
  foldIDs <- indices %% kFolds + 1
  
  folds <- list()
  for (iFold in 1:kFolds)
  {
    idsThisFolds <- indices[foldIDs == iFold]
    idsOutThisFold <- indices[!(foldIDs %in% idsThisFolds)]
    folds[[iFold]] <- idsOutThisFold
  }
  
  return (folds)
}