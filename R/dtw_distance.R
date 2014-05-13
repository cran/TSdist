

#This function calculates the Dynamic Time Warping distance.
dtwDistance <- function(x, y, sigma){
  
  dtwInitialCheck(x, y, sigma)
    
  #The length of the series are defined
  tamx <- length(x)
  tamy <- length(y)

  #The local distance matrix is computed
  distMatrix <- as.vector(t(proxy::dist(x, y, method = "euclidean")))
  
  #The cost matrix is initialized.
  costMatrix <- c(1:((tamx) * (tamy))) * 0 + (max(distMatrix) * 
                                             length(distMatrix))

  #The case with no temporal constraint
  if (missing(sigma)){
    #The cost matrix is computed using dynammic programming.
    resultList <- .C("dtwnw", as.integer(tamx), as.integer(tamy),
                     as.double(costMatrix), as.double(distMatrix))
    costMatrix <- resultList[[3]]
    
  #The case with a temporal constraint
  } else {
    #The cost matrix is computed using dynammic programming.
    resultList <- .C("dtw", as.integer(tamx), as.integer(tamy), 
                     as.integer(sigma), as.double(costMatrix), 
                     as.double(distMatrix))
    costMatrix <- resultList[[4]]
  }

  #The last position of the cost matrix is returned as the distance between 
  #the series.
  d<-costMatrix[length(costMatrix)]
  return(d)
}


# This function checks for possible initial errors: 
dtwInitialCheck <- function(x, y, sigma){
  
  if (!is.numeric(x) | !is.numeric(y)){
    stop('The series must be numeric vectors', call.=FALSE)
  }
  if (length(x) < 1 | length(y) < 1){
    stop('The series must have at least one point', call.=FALSE)
  }
  if(!missing(sigma)){
    if((sigma)<=0){
      stop('The window size must be positive', call.=FALSE)
    }
    if((sigma + 1) > length(x)){
      stop('The window size exceeds the the length of the first series', call.=FALSE)
    }
    if((sigma + 1) > length(y)){
      stop('The window size exceeds the the length of the second series', call.=FALSE)
    }
    if(sigma < abs(length(x) - length(y))){
      stop('It is not possible to compare those two series with the defined window size', call.=FALSE)
    }
  }
  if (any(is.na(x)) | any(is.na(y))){
    stop('There are missing values in the series', call.=FALSE)
  } 
}