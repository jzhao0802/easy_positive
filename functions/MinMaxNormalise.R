FindMinMaxOneVar <- function(vec)
{
  minMax <- rep(-1,2)
  minMax[1] <- min(vec)
  minMax[2] <- max(vec)
  
  return (minMax)
}

MinMaxNormaliseAllVarsWithGivenMinMaxValues <- function(XMat, minMaxValMat)
{
  if (ncol(XMat) != ncol(minMaxValMat))
    stop("Error! XMat and minMaxValMat don't have the same number of variables.")
  
  for (iVar in 1:ncol(XMat))
  {
    if (minMaxValMat[1, iVar] > minMaxValMat[2, iVar])
      stop(paste("Error! The min value is larger than the max value of the ", iVar, "th variable.", 
                 sep=""))
    if (minMaxValMat[1, iVar] != minMaxValMat[2, iVar])
    {
      XMat[, iVar] <- 
        (XMat[, iVar] - minMaxValMat[1, iVar] ) / (minMaxValMat[2, iVar] - minMaxValMat[1, iVar])
    }
  }
  
  return (XMat)
}