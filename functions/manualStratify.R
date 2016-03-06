library(caret)
library(compiler)

stratifySmallSample <- cmpfun(function(y, k_folds)
{
  # get the positive and negative
  
  pos_indices <- which(y>0)
  neg_indices <- which(y<=0)
  pos_indices <- sample(pos_indices)
  neg_indices <- sample(neg_indices)
  
  # 
  
  av_n_pos <- length(pos_indices) / k_folds
  av_n_neg <- length(neg_indices) / k_folds
  
  pos_pointer <- 1
  neg_pointer <- 1
  pos_frac <- 0
  neg_frac <- 0
  
  folds <- list()
  
  for (i_fold in 1:k_folds)
  {
    if (i_fold != k_folds)
    {
      pos_frac <- pos_frac + av_n_pos - floor(av_n_pos)
      neg_frac <- neg_frac + av_n_neg - floor(av_n_neg)
      if (pos_frac >= 1)
      {
        pos_ids_thisfold <- pos_indices[pos_pointer:(pos_pointer+floor(av_n_pos))]
        pos_frac <- pos_frac-1
      } else
        pos_ids_thisfold <- pos_indices[pos_pointer:(pos_pointer+floor(av_n_pos)-1)]
      
      if (neg_frac >= 1)
      {
        neg_ids_thisfold <- neg_indices[neg_pointer:(neg_pointer+floor(av_n_neg))]
        neg_frac <- neg_frac-1
      } else
        neg_ids_thisfold <- neg_indices[neg_pointer:(neg_pointer+floor(av_n_neg)-1)]
      
      pos_pointer <- pos_pointer + length(pos_ids_thisfold)
      neg_pointer <- neg_pointer + length(neg_ids_thisfold)
      
    } else 
    {
      pos_ids_thisfold <- pos_indices[pos_pointer:length(pos_indices)]
      neg_ids_thisfold <- neg_indices[neg_pointer:length(neg_indices)]
    }
    
    ids_thisfold <- c(pos_ids_thisfold, neg_ids_thisfold)
    ids_out_thisfold <- (1:length(y))[which(!((1:length(y)) %in% ids_thisfold))]
    
    folds[[i_fold]] <- ids_out_thisfold
  }
  
  return (folds)
}, option=list(optimize=3))

manualStratify <- cmpfun(function(y, kFolds)
{
  # to prevent unstratified data subsets (due to the data size)
  # use a check
  bStratValid <- FALSE
  nPosTot <- sum(y == 1)
  nNegTot <- sum(y == 0)
  pAvPos <- nPosTot / (nPosTot + nNegTot)
  pAvNeg <- nNegTot / (nPosTot + nNegTot)
  pPosThreshL <- pAvPos * 0.8
  pNegThreshL <- pAvNeg * 0.8
  pPosThreshH <- pAvPos * 1.25
  pNegThreshH <- pAvNeg * 1.25
  nTrialsLimit <- 10
  iTrialsLimit <- 1
  while (bStratValid == FALSE)
  {
    folds <- createFolds(y, k=kFolds, returnTrain=TRUE)
    # check
    bLocalEvidence <- TRUE
    for (iFold in 1:kFolds)
    {
      rest_ids <- folds[[iFold]]
      test_ids <- (1:length(y))[-rest_ids]
      nPos <- sum(y[test_ids] == 1)
      nNeg <- sum(y[test_ids] == 0)
      pPos <- nPos / (nPos + nNeg)
      pNeg <- 1 - pPos
      
      bLocalEvidence <- bLocalEvidence & 
        ((pPos >= pPosThreshL) & (pNeg >= pNegThreshL) & (pPos <= pPosThreshH) & (pNeg <= pNegThreshH))
    }
    
    if (bLocalEvidence)
    {
      bStratValid <- TRUE
    }
    
    # 
    iTrialsLimit = iTrialsLimit + 1
    if (iTrialsLimit > nTrialsLimit)
    {
      folds <- stratifySmallSample(y, kFolds)
      
#       # for test
#       for (i_fold in 1:length(folds))
#       {
#         ids_thisfold <- (1:length(y))[which(!(1:length(y)) %in% folds[[i_fold]])]
#         cat("fold ", i_fold, sep="")
#         cat(", nPos: ", sum(y[ids_thisfold] > 0))
#         cat(", nNeg: ", sum(y[ids_thisfold] == 0))
#         cat("\n")
#       }
      
      
      break
    }
  }
  
  if (iTrialsLimit > nTrialsLimit)
  {
    cat(paste("Not able to stratify. \n"))
  }
  
  return (folds)
}, options=list(optimize=3))

stratifyFoldIDs <- function(y, k_folds)
{
  ids <- 1:k_folds
  ids_every_pos <- rep(ids, length.out=sum(y==1))
  ids_every_neg <- rep(ids, length.out=sum(y!=1))
  
  ids_every_datum <- rep(-1, length(y))
  ids_every_datum[which(y==1)] <- ids_every_pos
  ids_every_datum[which(y!=1)] <- ids_every_neg
  
  return (ids_every_datum)
}