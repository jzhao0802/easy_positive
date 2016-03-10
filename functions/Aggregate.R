library(ROCR)

CalIndependence <- function(corrCoefs, threshold)
{
  indep <- sum(corrCoefs < threshold) / length(corrCoefs)
  
  return (indep)
}

CalPrecisionAtRecall <- function(preds, labels, recall)
{
  predsObj <- prediction(predictions=preds, labels=labels)
  perf <- performance(predsObj, measure="prec", x.measure="rec")
  
  precision <- approx(perf@x.values[[1]], perf@y.values[[1]], xout=0.05)
  return (CalPrecisionAtRecall)
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
  
  while (length(predsWinLearners) < nWinners2Select)
  {
    if (length(predsWinLearners) == 0)
    {
      bestLearnID <- which.max(accuraciesAllLearners)
      predsWinLearners <- 
        predsAllLearners[, bestLearnID]
      remaining <- 
        remaining[, !(colnames(remaining) %in% colnames(predsWinLearners))]
      accuraciesRemaining <- 
        accuraciesRemaining[!(names(accuraciesRemaining) %in% colnames(predsWinLearners))]
    } else
    {
      corrs <- cor(predsWinLearners, remaining)
      independences <- apply(corrs, 2, CalIndependence, threshold=threshold)
      scores <- accuraciesRemaining / accuraciesAllLearners[bestLearnID] + independences
      winnerIDThisRound <- colnames(sort(scores))[1]
      
      predsWinLearners <- cbind(predsWinLearners, predsAllLearners[winnerIDThisRound])
      remaining <- remaining[, (colnames(remaining) != winnerIDThisRound)]
      accuraciesRemaining <- 
        accuraciesRemaining[names(accuraciesRemaining) != winnerIDThisRound]
    }
  }
  
  return (as.numeric(colnames(predsWinLearners)))
}