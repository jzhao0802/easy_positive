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

IS_PRECISION_CORRECT <- function(targetRecall)
{
  nData <- 1000
  nPoses <- round(runif(1)*nData)
  if (nPoses < 1)
    nPoses <- 1
  if (nPoses > nData-1) 
    nPoses <- nData - 1
  
  y <- rep(0, nData)
  y[1:nPoses] <- 1
  
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

test_that("Precision at the specific recall is correct.",{
  IS_PRECISION_CORRECT(0.5)
  IS_PRECISION_CORRECT(1)
  IS_PRECISION_CORRECT(0)
  IS_PRECISION_CORRECT(0.1)
  IS_PRECISION_CORRECT(0.9)
})
