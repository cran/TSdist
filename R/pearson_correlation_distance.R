
#This function calculates a distance based on Pearson's correlation
correlationDistance <- function(x, y, beta){
  
  cInitialCheck(x, y, beta)

  if (missing(beta)){
  #The distance is directly calculated.
    d <- 2 * (1 - cor(x, y))
  } else {
    d <- ((1 - cor(x, y)) / (1 + cor(x, y))) ^ beta
  }
  
  d  
}


# This function checks for possible initial errors: 
cInitialCheck <- function(x, y, beta){
  
  if (!is.numeric(x) | !is.numeric(y)){
    stop('The series must be numeric', call.=FALSE)
  }
  if (length(x) <= 1 | length(y) <= 1){
    stop('The series must have a more than one point', call.=FALSE)
  }
  if (length(x) != length(y)){
    stop('The series must have the same length', call.=FALSE)
  }
  if (any(is.na(x)) | any(is.na(y))){
    stop('There are missing values in the series', call.=FALSE)
  } 
  if (!missing(beta)){
    if(beta <= 0){
      stop('beta must be positive', call.=FALSE)
    } 
  }
}

