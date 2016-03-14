rm(list=ls())

context("Testing functions in Aggregate.R")
source("../../functions/Aggregate.R")
# source("functions/Aggregate.R")

#
## CalIndependence

context("CalIndependence()")

# 1. 

IS_INDEP_CORRECT <- function()
{
  nPreds <- 100
  threshold <- 0.6
  
  newPreds <- runif(nPreds)
  
  predMat <- matrix(-1, ncol=4, nrow=nPreds)
  predMat[, 1] <- runif(nPreds)
  predMat[, 2] <- newPreds
  predMat[1:50, 2] <- runif(50)
  predMat[, 3] <- newPreds
  predMat[1, 3] <- predMat[1, 3] / 2
  predMat[, 4] <- -newPreds
  
  corr <- cor(newPreds, predMat)
  
  indep <- CalIndependence(corr[1, ], threshold)
  
  if (indep == sum(corr[1, ] < threshold) / ncol(predMat))
  {
    return (T)
  } else
  {
    return (F)
  }
    
}

test_that("Correlation score is calculated correctly.",{
  expect_true(IS_INDEP_CORRECT())
  expect_true(IS_INDEP_CORRECT())
  expect_true(IS_INDEP_CORRECT())
})

#
## CalPrecisionAtRecall()

context("CalPrecisionAtRecall()")
library(ROCR)

GenLabels <- function(nData)
{
  nPoses <- round(runif(1)*nData)
  if (nPoses < 1)
    nPoses <- 1
  if (nPoses > nData-1) 
    nPoses <- nData - 1
  
  y <- rep(0, nData)
  y[1:nPoses] <- 1
  
  return (y)
}

IS_PRECISION_CORRECT <- function(targetRecall)
{
  nData <- 1000
  y <- GenLabels(nData)
  
  preds <- runif(nData)
  
  # 
  predsObj <- prediction(predictions=preds, labels=y)
  perf <- performance(predsObj, measure="prec", x.measure="rec")
  recalls <- perf@x.values[[1]]
  precisions <- perf@y.values[[1]]
  precisions[is.na(precisions)] <- 0
  
  secondID <- NULL
  for (i in 1:length(recalls))
  {
    if (recalls[i] >= targetRecall)
    {
      secondID <- i
      break
    }
  }
  if (secondID > 1)
    firstID <- secondID-1
  else
    firstID <- NULL
  
  precisionFromFunctionCall <- 
    CalPrecisionAtRecall(preds, y, targetRecall)
  
  if (is.null(firstID))
  {
    if ((precisionFromFunctionCall <= precisions[secondID]) & 
      (precisionFromFunctionCall >= 0))
      return (T)
    else
      return (F)
  } else
  {
    if ((precisionFromFunctionCall >= 0) &
        (precisionFromFunctionCall <= precisions[secondID]) & 
        (precisionFromFunctionCall >= precisions[firstID]))
      return (T)
    else
      return (F)
  }
}

test_that("Interpolated precision at the specific recall is correct.",{
  IS_PRECISION_CORRECT(0.5)
  IS_PRECISION_CORRECT(1)
  IS_PRECISION_CORRECT(0)
  IS_PRECISION_CORRECT(0.1)
  IS_PRECISION_CORRECT(0.9)
})

#
## SelectWinners()

context("SelectWinners()")

GenPredMat <- function(nData, nWeakLearners)
{
  mat <- matrix(runif(nData*nWeakLearners), nrow=nData)
  return (mat)
}

# test_that(paste("If more than the number of candidates are to be selected, ", 
#                 "return everyone; Otherwise the number indicated by portion ", sep=""), {
#   nData <- 1000
#   nWeakLearners <- 100
#   labels <- GenLabels(nData)
#   predMat <- GenPredMat(nData, nWeakLearners)
#   threshold <- 0.6
#   winnerPortion <- 1
#   targetRecall <- 0.05
# #   
# #   write.table(labels, sep=",", 
# #               file=paste("labels", 
# #                          ".csv", sep=""), col.names=F, row.names=F)
# #   write.table(predMat, sep=",", 
# #               file=paste("predMat", 
# #                          ".csv", sep=""), col.names=F, row.names=F)
#                   
#   # labels <- read.csv("tests/testthat/labels.csv", header=F, sep=",", check.names=FALSE)
#   # predMat <- read.csv("tests/testthat/predMat.csv", header=F, sep=",", check.names=FALSE)
# #   labels <- read.csv("labels.csv", header=F, sep=",", check.names=FALSE)
# #   predMat <- read.csv("predMat.csv", header=F, sep=",", check.names=FALSE)
#   
#   winnerIDs <- SelectWinners(predMat, labels, targetRecall,
#                              winnerPortion, threshold)
#   
#   expect_equal(length(winnerIDs), nWeakLearners)
#   expect_true(all(winnerIDs %in% (1:nWeakLearners)))
#   expect_true(all((1:nWeakLearners) %in% winnerIDs))
#   
#   #
#   winnerPortion0.1 <- 0.1
#   winnerIDs0.1 <- SelectWinners(predMat, labels, targetRecall,
#                              winnerPortion0.1, threshold)
#   expect_equal(length(winnerIDs0.1), ceiling(nWeakLearners*winnerPortion0.1))
#   expect_equal(unique(winnerIDs0.1), winnerIDs0.1)
# })
# 
# IS_1STWINNER_MOSTACCURATE <- function(nData, nWeakLearners, winnerPortion, 
#                                       threshold, targetRecall)
# {
#   labels <- GenLabels(nData)
#   predMat <- GenPredMat(nData, nWeakLearners)
#   
#   if (ceiling(winnerPortion*nWeakLearners) >= nWeakLearners)
#     stop("Error! 1st winner is only most accuracte if nWinners2Select < nWeakLearners.")
#   
#   winnerIDs <- SelectWinners(predMat, labels, targetRecall,
#                              winnerPortion, threshold)
#   
#   # 
#   accuracies <- apply(predMat, 2, CalPrecisionAtRecall, 
#                       labels=labels, recall=targetRecall)
#   
#   if (accuracies[winnerIDs[1]] == max(accuracies))
#     return (T)
#   else
#     return (F)
# }
# 
# test_that("The first winner has the highest accuracy.", {
#   expect_true(IS_1STWINNER_MOSTACCURATE(nData=1000, nWeakLearners=100, 
#                                         winnerPortion=0.05, 
#                                         threshold=0.6, targetRecall=0.05))
#   expect_true(IS_1STWINNER_MOSTACCURATE(nData=1000, nWeakLearners=100, 
#                                         winnerPortion=0.1, 
#                                         threshold=0.7, targetRecall=0.05))
#   expect_true(IS_1STWINNER_MOSTACCURATE(nData=1000, nWeakLearners=100, 
#                                         winnerPortion=0.4, 
#                                         threshold=0.5, targetRecall=0.15))
# })

CalIndependence4Winners <- function(corrCoefs, threshold, winnerRank)
{
  corrCoefs <- corrCoefs[1:(winnerRank-1)]
  indep <- sum(corrCoefs < threshold) / length(corrCoefs)
  return (indep)
}

IS_PREV_WINNER_BETTER <- function(nData, nWeakLearners, winnerPortion, 
                                  threshold, targetRecall)
{
  labels <- GenLabels(nData)
  predMat <- GenPredMat(nData, nWeakLearners)
  colnames(predMat) <- 1:ncol(predMat)
  
  if (ceiling(winnerPortion*nWeakLearners) >= nWeakLearners)
    stop("Error! Previous winners are only better when nWinners2Select < nWeakLearners.")
  
  if (ceiling(winnerPortion*nWeakLearners <= 1))
    return (T)
  
  winnerIDs <- SelectWinners(predMat, labels, targetRecall,
                             winnerPortion, threshold)
  predsWinLearners <- predMat[, colnames(predMat) %in% winnerIDs]
  remaining <- predMat[, !(colnames(predMat) %in% winnerIDs)]
  
  
  # 
  accuraciesAllData <- apply(predMat, 2, CalPrecisionAtRecall, 
                             labels=labels, recall=targetRecall)
  
  corrMatWinners <- cor(predsWinLearners, predsWinLearners)
  corrMatRemain <- cor(predsWinLearners, remaining)
  indepsWinners <- rep(-1, length(winnerIDs))
  indepsWinners[1] <- 0
  names(indepsWinners) <- winnerIDs
  for (iWinner in 2:length(winnerIDs))
  {
    indepsWinners[iWinner] <- 
      CalIndependence4Winners(corrMatWinners[, iWinner], threshold, iWinner)
  }
  indepsRemain <- apply(corrMatRemain, 2, CalIndependence, threshold=threshold)
  
  scores <- rep(-1, nWeakLearners)
  names(scores)[1:length(winnerIDs)] <- winnerIDs
  names(scores)[(length(winnerIDs)+1):nWeakLearners] <- colnames(remaining)
  scores[1] <- 2
  
  
  for (iWinner in 2:length(winnerIDs))
  {
    ID <- winnerIDs[iWinner]
    score <- 
      accuraciesAllData[
        names(accuraciesAllData) == ID
      ] / accuraciesAllData[winnerIDs[1]] + 
      indepsWinners[names(indepsWinners) == ID]
    scores[names(scores)==ID] <- score
  }
  
  for (iRemain in 1:ncol(remaining))
  {
    ID <- colnames(remaining)[iRemain]
    score <- 
      accuraciesAllData[
        names(accuraciesAllData) == ID
      ] / accuraciesAllData[winnerIDs[1]] +
      indepsRemain[names(indepsRemain) == ID]
    scores[names(scores)==ID] <- score
  }
  
  for (iWinner in 1:length(winnerIDs))
  {
    if (scores[iWinner] == max(scores[iWinner:length(scores)]))
      next
    return (F)
  }
  return (T)
  
}

test_that(paste("Every other winner has a higher score then winners behind it ", 
                "and those not selected", sep=""), {
  expect_true(IS_PREV_WINNER_BETTER(nData=1000, nWeakLearners=100, 
                                    winnerPortion=0.05, 
                                    threshold=0.6, targetRecall=0.05))
  expect_true(IS_PREV_WINNER_BETTER(nData=500, nWeakLearners=100, 
                                    winnerPortion=0.1, 
                                    threshold=0.7, targetRecall=0.1))
  expect_true(IS_PREV_WINNER_BETTER(nData=1000, nWeakLearners=20, 
                                    winnerPortion=0.2, 
                                    threshold=0.4, targetRecall=0.1))
})