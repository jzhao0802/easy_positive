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
	
  if (is.null(colnames(XMat)))
    stop("Error! XMat must have col.names.")
  
  for (name in colnames(XMat))
  {
    if (!(name %in% colnames(minMaxValMat)))
      stop(paste("Error! Variable ", name, " doesn't exist in minMaxValMat."))
  }

  for (name in colnames(XMat))
  {
    if (minMaxValMat[1, name] > minMaxValMat[2, name])
      stop(paste("Error! The min value is larger than the max value of the ", name, "th variable.", 
                 sep=""))
    if (minMaxValMat[1, name] != minMaxValMat[2, name])
    {
      XMat[, name] <- 
        (XMat[, name] - minMaxValMat[1, name] ) / (minMaxValMat[2, name] - minMaxValMat[1, name])
    }
  }
  
  return (XMat)
}