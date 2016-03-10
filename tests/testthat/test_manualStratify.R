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

test_that("Sample size difference <= 1", {
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
