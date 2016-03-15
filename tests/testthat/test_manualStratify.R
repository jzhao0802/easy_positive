rm(list=ls())

context("Testing some (but not all of the) functions in manualStratify.R")
source("../../functions/manualStratify.R")

#
## DivideIntoFolds

context("DivideIntoFolds()")

# 1. when the input vector is shorter than the number of folds, error
# 2. the difference between sample sizes in different folds cannot be > 1
# 3. After dividing the vec into folds, the combination of IDs in all folds
#   is identical to the original vec

MaxSizeDiff <- function(IDs, kFolds)
{
  if (kFolds == 1)
    return (0)
  
  folds <- DivideIntoFolds(IDs, kFolds)
  
  maxDiff <- 0
  for (iFold in 2:kFolds)
  {
    diff <- abs(length(folds[[iFold]]) - length(folds[[iFold-1]]))
    if (diff > maxDiff)
    {
      maxDiff <- diff
    }
  }
  return (maxDiff)
}

CombineAllFolds <- function(IDs, kFolds)
{
  folds <- DivideIntoFolds(IDs, kFolds)
  
  combinedIDs <- NULL
  
  for (iFold in 1:kFolds)
  {
    combinedIDs <- c(combinedIDs, folds[[iFold]])
  }
  return (combinedIDs)
}

test_that("Reports error when the data is fewer than the number of folds",{
  expect_error(DivideIntoFolds(IDs=sample(1:4), kFolds=5))
  expect_error(DivideIntoFolds(IDs=sample(1:200), kFolds=1000))
})

test_that("Sample size differences <= 1", {
  expect_true(MaxSizeDiff(IDs=sample(1:301), kFolds=11) <= 1)
  expect_true(MaxSizeDiff(IDs=sample(1:9), kFolds=5) <= 1)
  expect_true(MaxSizeDiff(IDs=sample(1:9), kFolds=9) <= 1)
  })

test_that("IDs in all folds together form the complete original set", {
  expect_true(all(CombineAllFolds(IDs=sample(1:301), kFolds=11) %in% (1:301)) &
                all((1:301) %in% CombineAllFolds(IDs=sample(1:301), kFolds=11)))
  expect_equal(length(CombineAllFolds(IDs=sample(1:301), kFolds=11)), 301)
  
  expect_true(all(CombineAllFolds(IDs=sample(1:11), kFolds=10) %in% (1:11)) &
                all((1:11) %in% CombineAllFolds(IDs=sample(1:11), kFolds=10)))
  expect_equal(length(CombineAllFolds(IDs=sample(1:11), kFolds=10)), 11)
  
  expect_true(all(CombineAllFolds(IDs=sample(1:11), kFolds=11) %in% (1:11)) &
                all((1:11) %in% CombineAllFolds(IDs=sample(1:11), kFolds=11)))
  expect_equal(length(CombineAllFolds(IDs=sample(1:11), kFolds=11)), 11)
})

#
## StratifyEasyDifficultPositives

context("StratifyEasyDifficultPositives()")

# 1. for each of the 3 classes (easy, difficult and negative), the differences 
#   among sample sizes in different folds are no more than 1
# 2. when the number of easy or negative is < kFolds, reports error; 
# 3. when the number of difficult is < kFolds, issues a warning and calls 
#   stratifySmallSample

IS_MAXDIFF_ALLCLASSES_LE_1 <- function(kFolds)
{
  if (kFolds == 1)
    return (T)
  
  minNTimes <- 1
  maxNTimes <- 10
  
  # randomly generate a y vector that
  # 1. has more positive elements than 2 x kFolds
  # 2. has more negative elements than kFolds
  
  nPositives <- round(runif(n=1, min=minNTimes, max=maxNTimes)) * 2 * kFolds
  nNegatives <- round(runif(n=1, min=minNTimes, max=maxNTimes)) * kFolds
  y <- sample(c(rep(1, nPositives), rep(0, nNegatives)))
  
  # randomly generate a weight vec with the same length of y, which
  # 1. has more than kFolds easy positives (weight > 0.5)
  # 2. has more than kFolds difficult positives (0< weight <= 0.5)
  # 3. has 0 weights for negatives
  
  nEasyPositives <- round(runif(n=1, min=kFolds, max=nPositives-kFolds))
  nDifficultPositives <- nPositives - nEasyPositives
  wEasyPositives <- runif(n=nEasyPositives, min=0.5+1e-6, max=1)
  wDifficultPositives <- runif(n=nDifficultPositives, min=1e-6, max=0.5)
  wPositives <- c(wEasyPositives, wDifficultPositives)
  weights <- rep(0, nPositives+nNegatives)
  weights[y==1] <- wPositives
  
  # compute the folds using StratifyEasyDifficultPositives
  
  folds <- 
    StratifyEasyDifficultPositives(y, weights, kFolds)
  
  # find the max size differences for all classes among folds 
  
  maxDiff_Easy <- 0
  maxDiff_Difficult <- 0
  maxDiff_Neg <- 0
  
  for (iFold in 2:kFolds)
  {
    yPrev <- y[folds[[iFold-1]]]
    weightsPrev <- weights[folds[[iFold-1]]]
    nPrevEasies <- sum((yPrev==1) & (weightsPrev>0.5))
    nPrevDifficults <- sum((yPrev==1) & (weightsPrev<=0.5))
    nPrevNegs <- sum(yPrev==0)
    
    yThis <- y[folds[[iFold]]]
    weightsThis <- weights[folds[[iFold]]]
    nThisEasies <- sum((yThis==1) & (weightsThis>0.5))
    nThisDifficults <- sum((yThis==1) & (weightsThis<=0.5))
    nThisNegs <- sum(yThis==0)
    
    diffEasy <- abs(nThisEasies - nPrevEasies)
    if (diffEasy > maxDiff_Easy)
      maxDiff_Easy <- diffEasy
    
    diffDifficult <- abs(nThisDifficults - nPrevDifficults)
    if (diffDifficult > maxDiff_Difficult)
      maxDiff_Difficult <- diffDifficult
    
    diffNeg <- abs(nThisNegs - nPrevNegs)
    if (diffNeg > maxDiff_Neg)
      maxDiff_Neg <- diffNeg
  }
  
  if ((maxDiff_Neg <= 1) & (maxDiff_Easy <= 1) & (maxDiff_Difficult <= 1))
    return (T)
  else 
    return (F)
}

test_that("Sample size differences in every class <= 1", {
  expect_true(IS_MAXDIFF_ALLCLASSES_LE_1(kFolds=5))
  expect_true(IS_MAXDIFF_ALLCLASSES_LE_1(kFolds=11))
  expect_true(IS_MAXDIFF_ALLCLASSES_LE_1(kFolds=101))
})

Call_StratifyEasyDifficultPositives_FewerEasy <- function(kFolds)
{
  minNTimes <- 1
  maxNTimes <- 10
  
  # randomly generate a y vector that
  # 1. has more positive elements than 2 x kFolds
  # 2. has more negative elements than kFolds
  
  nPositives <- round(runif(n=1, min=minNTimes, max=maxNTimes)) * 2 * kFolds
  nNegatives <- round(runif(n=1, min=minNTimes, max=maxNTimes)) * kFolds
  y <- sample(c(rep(1, nPositives), rep(0, nNegatives)))
  
  # randomly generate a weight vec with the same length of y, which
  # 1. has fewer than kFolds easy positives (weight > 0.5)
  # 2. has more than kFolds difficult positives (0< weight <= 0.5)
  # 3. has 0 weights for negatives
  
  nEasyPositives <- round(runif(n=1, min=0, max=(kFolds-1)))
  nDifficultPositives <- nPositives - nEasyPositives
  wEasyPositives <- runif(n=nEasyPositives, min=0.5+1e-6, max=1)
  wDifficultPositives <- runif(n=nDifficultPositives, min=1e-6, max=0.5)
  wPositives <- c(wEasyPositives, wDifficultPositives)
  weights <- rep(0, nPositives+nNegatives)
  weights[y==1] <- wPositives
  
  # compute the folds using StratifyEasyDifficultPositives
  
  folds <- 
    StratifyEasyDifficultPositives(y, weights, kFolds)
}

Call_StratifyEasyDifficultPositives_FewerNeg <- function(kFolds)
{
  minNTimes <- 1
  maxNTimes <- 10
  
  # randomly generate a y vector that
  # 1. has more positive elements than 2 x kFolds
  # 2. has fewer negative elements than kFolds
  
  nPositives <- round(runif(n=1, min=minNTimes, max=maxNTimes)) * 2 * kFolds
  nNegatives <- round(runif(n=1, min=0, max=kFolds-0.51))
  y <- sample(c(rep(1, nPositives), rep(0, nNegatives)))
  
  # randomly generate a weight vec with the same length of y, which
  # 1. has more than kFolds easy positives (weight > 0.5)
  # 2. has more than kFolds difficult positives (0< weight <= 0.5)
  # 3. has 0 weights for negatives
  
  nEasyPositives <- round(runif(n=1, min=kFolds, max=nPositives-kFolds))
  nDifficultPositives <- nPositives - nEasyPositives
  wEasyPositives <- runif(n=nEasyPositives, min=0.5+1e-6, max=1)
  wDifficultPositives <- runif(n=nDifficultPositives, min=1e-6, max=0.5)
  wPositives <- c(wEasyPositives, wDifficultPositives)
  weights <- rep(0, nPositives+nNegatives)
  weights[y==1] <- wPositives
  
  # compute the folds using StratifyEasyDifficultPositives
  
  folds <- 
    StratifyEasyDifficultPositives(y, weights, kFolds)
}

test_that("Error when the number of easy or negative < kFolds", {
  expect_error(
    Call_StratifyEasyDifficultPositives_FewerEasy(kFolds=5), 
    "Too few easy positives")
  expect_error(
    Call_StratifyEasyDifficultPositives_FewerNeg(kFolds=11),
    "Too few negatives")
})

Call_StratifyEasyDifficultPositives_FewerDifficult <- function(kFolds)
{
  minNTimes <- 1
  maxNTimes <- 10
  
  # randomly generate a y vector that
  # 1. has more positive elements than 2 x kFolds
  # 2. has more negative elements than kFolds
  
  nPositives <- round(runif(n=1, min=minNTimes, max=maxNTimes)) * 2 * kFolds
  nNegatives <- round(runif(n=1, min=minNTimes, max=maxNTimes)) * kFolds
  y <- sample(c(rep(1, nPositives), rep(0, nNegatives)))
  
  # randomly generate a weight vec with the same length of y, which
  # 1. has more than kFolds easy positives (weight > 0.5)
  # 2. has fewer than kFolds difficult positives (0< weight <= 0.5)
  # 3. has 0 weights for negatives
  
  nEasyPositives <- round(runif(n=1, min=nPositives-kFolds+1, max=nPositives))
  nDifficultPositives <- nPositives - nEasyPositives
  wEasyPositives <- runif(n=nEasyPositives, min=0.5+1e-6, max=1)
  wDifficultPositives <- runif(n=nDifficultPositives, min=1e-6, max=0.5)
  wPositives <- c(wEasyPositives, wDifficultPositives)
  weights <- rep(0, nPositives+nNegatives)
  weights[y==1] <- wPositives
  
  # compute the folds using StratifyEasyDifficultPositives
  
  folds <- 
    StratifyEasyDifficultPositives(y, weights, kFolds)
  
  return (T)
}

test_that("Error when the number of easy or negative < kFolds", {
  expect_warning(
    Call_StratifyEasyDifficultPositives_FewerDifficult(kFolds=5),
    paste("Warning! Too few difficult positives. Automatically calling", 
          " stratifySmallSample instead. ", sep="")
    )
  
  expect_true(
    Call_StratifyEasyDifficultPositives_FewerDifficult(kFolds=5)
  )
})

IS_COMBINED_STRATIFY_ORIGINAL <- function(kFolds)
{
  if (kFolds == 1)
    return (T)
  
  minNTimes <- 1
  maxNTimes <- 10
  
  # randomly generate a y vector that
  # 1. has more positive elements than 2 x kFolds
  # 2. has more negative elements than kFolds
  
  nPositives <- round(runif(n=1, min=minNTimes, max=maxNTimes)) * 2 * kFolds
  nNegatives <- round(runif(n=1, min=minNTimes, max=maxNTimes)) * kFolds
  y <- sample(c(rep(1, nPositives), rep(0, nNegatives)))
  
  # randomly generate a weight vec with the same length of y, which
  # 1. has more than kFolds easy positives (weight > 0.5)
  # 2. has more than kFolds difficult positives (0< weight <= 0.5)
  # 3. has 0 weights for negatives
  
  nEasyPositives <- round(runif(n=1, min=kFolds, max=nPositives-kFolds))
  nDifficultPositives <- nPositives - nEasyPositives
  wEasyPositives <- runif(n=nEasyPositives, min=0.5+1e-6, max=1)
  wDifficultPositives <- runif(n=nDifficultPositives, min=1e-6, max=0.5)
  wPositives <- c(wEasyPositives, wDifficultPositives)
  weights <- rep(0, nPositives+nNegatives)
  weights[y==1] <- wPositives
  
  # compute the folds using StratifyEasyDifficultPositives
  
  folds <- 
    StratifyEasyDifficultPositives(y, weights, kFolds)
  
  # check the combined left-out
  combinedSet <- NULL
  for (iFold in 1:length(folds))
  {
    IDsLeftOut <- (1:length(y))[!((1:length(y)) %in% folds[[iFold]])]
    combinedSet <- c(combinedSet, IDsLeftOut)
  }
  
  if (all(combinedSet %in% (1:length(y))) & 
      all((1:length(y)) %in% combinedSet))
    return (T)
  else
    return (F)
}

test_that("Left-out from Stratified folds combine to the original set.", {
  expect_true(IS_COMBINED_STRATIFY_ORIGINAL(kFolds=5))
  expect_true(IS_COMBINED_STRATIFY_ORIGINAL(kFolds=11))
})