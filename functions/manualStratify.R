library(caret)
library(compiler)

DivideIntoFolds <- function(IDs, kFolds)
{
  avNDataPerFold <- length(IDs) / kFolds
  pointer <- 1
  frac <- 0
  
  IDsAllFolds <- list()
  
  for (iFold in 1:kFolds)
  {
    if (iFold != kFolds)
    {
      frac <- frac + avNDataPerFold - floor(avNDataPerFold)
      if (frac >= 1)
      {
        IDsThisFold <- IDs[pointer:(pointer+floor(avNDataPerFold))]
        frac <- frac-1
      } else
        IDsThisFold <- IDs[pointer:(pointer+floor(avNDataPerFold)-1)]
      
      pointer <- pointer + length(IDsThisFold)
      
    } else 
    {
      IDsThisFold <- IDs[pointer:length(IDs)]
    }
    
    IDsAllFolds[[iFold]] <- IDsThisFold
  }
  
  return (IDsAllFolds)
}

StratifyEasyDifficultPositives <- function(y, posWeightsAllData, kFolds)
{
  if (!(all(levels(y) %in% c(0,1))))
  {
    stop(paste("Error! Invalid y-values for stratification. ", 
               "stratifySmallSample currently only supports one type ",
               "of y values: c(0, 1). "))
  }
  
  # the original IDs
  easyPosIDs <- which(posWeightsAllData > 0.5)
  if (length(easyPosIDs) < kFolds)
    stop("Error! Too few easy positives. StratifyEasyDifficultPositives cannot be used.")
  difficultPosIDs <- which((posWeightsAllData < 0.5) &
                             (posWeightsAllData > 0))
  if (length(easyPosIDs) < kFolds)
  {
    print(paste("Warning! Too few difficult positives. Automatically calling", 
                " stratifySmallSample instead. "))
    return (stratifySmallSample(y, kFolds))
  }
  negIDs <- which(y==0)
  if (length(negIDs) < kFolds)
    stop("Error! Too few negatives. StratifyEasyDifficultPositives failed.")
  
  # shuffle
  easyPosIDs <- sample(easyPosIDs, kFolds)
  difficultPosIDs <- sample(difficultPosIDs, kFolds)
  negIDs <- sample(negIDs, kFolds)
  
  # assign to folds
#   avNEasyPoses <- length(easyPosIDs) / kFolds
#   avNDifficultPoses <- length(difficultPosIDs) / kFolds
#   avNNegs <- length(negIDs) / kFolds
  
#   easyPosPointer <- 1
#   diffPosPointer <- 1
#   negPointer <- 1
#   easyPosFrac <- 0
#   diffPosFrac <- 0
#   negFrac <- 0
  
  easyPos_IDsAllFolds <- DivideIntoFolds(easyPosIDs, kFolds)
  difficultPos_IDsAllFolds <- DivideIntoFolds(difficultPosIDs, kFolds)
  neg_IDsAllFolds <- DivideIntoFolds(negIDs, kFolds)
  
  folds <- list()
  
  for (iFold in 1:kFolds)
  {
    IDsInThisFold <- c(easyPos_IDsAllFolds[[iFold]], 
                       difficultPos_IDsAllFolds[[iFold]], 
                       neg_IDsAllFolds[[iFold]])
    folds[[iFold]] <- 
      (1:length(y))[which(!((1:length(y)) %in% IDsInThisFold))]
  }
  
  return (folds)
}

stratifySmallSample <- cmpfun(function(y, k_folds)
{
  if (!(all(levels(y) %in% c(0,1))))
  {
    stop(paste("Error! Invalid y-values for stratification. ", 
               "stratifySmallSample currently only supports one type ",
               "of y values: c(0, 1). "))
  }
  
  
  # get the positive and negative
  
  pos_indices <- which(y==1)
  if (length(pos_indices) < k_folds)
    stop("Error! Too few positives. StratifyEasyDifficultPositives failed.")
  neg_indices <- which(y==0)
  if (length(neg_indices) < k_folds)
    stop("Error! Too few negatives. StratifyEasyDifficultPositives failed.")
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