library(caret)
library(compiler)

DivideIntoFolds <- function(IDs, kFolds)
{
  if (length(IDs) < kFolds)
    stop("Error! Number of data is smaller than kFolds!")
  
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
  if (length(difficultPosIDs) < kFolds)
  {
    warning(paste("Warning! Too few difficult positives. Automatically calling", 
                " stratifySmallSample instead. ", sep=""))
    return (stratifySmallSample(y, kFolds))
  }
  negIDs <- which(y==0)
  if (length(negIDs) < kFolds)
    stop("Error! Too few negatives. StratifyEasyDifficultPositives failed.")
  
  # shuffle
  easyPosIDs <- sample(easyPosIDs)
  difficultPosIDs <- sample(difficultPosIDs)
  negIDs <- sample(negIDs)
  
  # assign to folds
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
  
  pos_ids_allfolds <- DivideIntoFolds(pos_indices, k_folds)
  neg_ids_allfolds <- DivideIntoFolds(neg_indices, k_folds)
  
  folds <- list()
  
  for (i_fold in 1:k_folds)
  {
    IDsInThisFold <- c(pos_ids_allfolds[[i_fold]], 
                       neg_ids_allfolds[[i_fold]])
    folds[[i_fold]] <- 
      (1:length(y))[which(!((1:length(y)) %in% IDsInThisFold))]
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