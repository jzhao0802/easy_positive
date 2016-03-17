rm(list=ls())

context("Testing MinMaxNormalise.R")
source("../../functions/MinMaxNormalise.R")

#
## FindMinMaxOneVar

context("FindMinMaxOneVar()")

IS_FIRST_MIN <- function(vec)
{
  minMax <- FindMinMaxOneVar(vec)
  if (min(vec) == minMax[1])
    return (T)
  else
    return (F)
}

test_that("No values in the vector is smaller than the first value of the found minMax.", {
  expect_true(IS_FIRST_MIN(runif(5)))
  expect_true(IS_FIRST_MIN(runif(1001)))
})

IS_SECOND_MAX <- function(vec)
{
  minMax <- FindMinMaxOneVar(vec)
  if (max(vec) == minMax[2])
    return (T)
  else
    return (F)
}

test_that("No values in the vector is larger than the second value of the found minMax.", {
  expect_true(IS_SECOND_MAX(runif(11)))
  expect_true(IS_SECOND_MAX(runif(100)))
})

#
## MinMaxNormaliseAllVarsWithGivenMinMaxValues

context("MinMaxNormaliseAllVarsWithGivenMinMaxValues()")

IS_ORIGINAL_AFTER_BACKTRANSFORM <- function()
{
  nRows <- round(runif(1, 2, 20))
  nCols <- round(runif(1, 3, 30))
  dataMat <- matrix(runif(nRows*nCols), nrow=nRows)
  
  colnames(dataMat) <- 1:ncol(dataMat)
  
  minMaxMat <- matrix(-1, nrow=2, ncol=nCols)
  minMaxMat[1, ] <- runif(nCols, min=0, max=0.5)
  minMaxMat[2, ] <- runif(nCols, min=0.5, max=1)
  minMaxMat[, 1] <- c(0.5, 0.5)
  colnames(minMaxMat) <- 1:ncol(minMaxMat)
  
  normalisedDataMat <-
    MinMaxNormaliseAllVarsWithGivenMinMaxValues(dataMat, minMaxMat)
  
  backTransformedMat <- normalisedDataMat
  for (iVar in 1:nCols)
  {
    if (minMaxMat[1,iVar] == minMaxMat[2, iVar])
      next
    backTransformedMat[, iVar] <- 
      backTransformedMat[, iVar] * (minMaxMat[2,iVar]-minMaxMat[1,iVar]) + minMaxMat[1,iVar]
  }
  
  if (all.equal(backTransformedMat, dataMat))
    return (T)
  else
    return (F)
}

test_that("Original dataMat can be obtained after back transformation.", {
  expect_true(IS_ORIGINAL_AFTER_BACKTRANSFORM())
  expect_true(IS_ORIGINAL_AFTER_BACKTRANSFORM())
})