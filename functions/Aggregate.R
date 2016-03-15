library(ROCR)

CalIndependence <- function(corrCoefs, threshold)
{
  indep <- sum(corrCoefs < threshold) / length(corrCoefs)
  
  return (indep)
}

CalPrecisionAtRecall <- function(preds, labels, recall)
{
  if (!any(labels > 0))
    stop("Error! There's no positive label. Precision calculation failed.")
  if ((recall > 1) & (recall < 0))
    stop("Error1 Target recall should be within the range of 0 and 1. ")
  predsObj <- prediction(predictions=preds, labels=labels)
  perf <- performance(predsObj, measure="prec", x.measure="rec")
  
  nanRemovedPrecisions <- perf@y.values[[1]]
  nanRemovedPrecisions[is.na(nanRemovedPrecisions)] <- 0
  precision <- 
    (approx(perf@x.values[[1]], nanRemovedPrecisions, xout=recall))$y[1]
  return (precision)
}

SelectWinners <- function(predsAllLearners, y, targetRecall,
                          winnerPortion, threshold)
{
  #
  ##
  
  nLearners <- ncol(predsAllLearners)
  nWinners2Select <- ceiling(winnerPortion*nLearners)
  
  if (nWinners2Select >= ncol(predsAllLearners))
    return (1:ncol(predsAllLearners))
  
  #
  ## accuracies
  colnames(predsAllLearners) <- 1:ncol(predsAllLearners)
  
  accuraciesAllLearners <- 
    apply(predsAllLearners, 2, CalPrecisionAtRecall, labels=y, recall=targetRecall)
  accuraciesRemaining <- accuraciesAllLearners
  
  predsWinLearners <- NULL
  remaining <- predsAllLearners
  
  iRound <- 1
  nWinnersSelected <- 0
  while (nWinnersSelected < nWinners2Select)
  {
    if (nWinnersSelected == 0)
    {
      bestLearnID <- which.max(accuraciesAllLearners)
      predsWinLearners <- 
        predsAllLearners[, bestLearnID]
      predsWinLearners <- matrix(predsWinLearners, ncol=1)
      colnames(predsWinLearners) <- bestLearnID
      remaining <- 
        remaining[, !(colnames(remaining) %in% colnames(predsWinLearners))]
      accuraciesRemaining <- 
        accuraciesRemaining[!(names(accuraciesRemaining) %in% colnames(predsWinLearners))]
      
      nWinnersSelected <- nWinnersSelected+1
    } else
    {
      corrs <- cor(predsWinLearners, remaining)
      independences <- apply(corrs, 2, CalIndependence, threshold=threshold)
      scores <- 
        accuraciesRemaining / accuraciesAllLearners[names(accuraciesAllLearners) == bestLearnID] + 
        independences
      winnerIDThisRound <- names(sort(scores, decreasing=T))[1]
      
      predsWinLearners <- 
        cbind(predsWinLearners, predsAllLearners[colnames(predsAllLearners) == winnerIDThisRound])
      colnames(predsWinLearners)[ncol(predsWinLearners)] <- winnerIDThisRound
      remaining <- remaining[, (colnames(remaining) != winnerIDThisRound)]
      accuraciesRemaining <- 
        accuraciesRemaining[names(accuraciesRemaining) != winnerIDThisRound]
      
      nWinnersSelected <- nWinnersSelected+1
    }
    iRound <- iRound + 1
  }
  
  return (as.numeric(colnames(predsWinLearners)))
}