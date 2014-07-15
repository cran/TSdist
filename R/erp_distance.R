
erpDistance<-function(x, y, g, sigma){
  
  if (class(try(erpInitialCheck(x, y, g, sigma)))=="try-error"){
    return(NA)
  }else{

  #The length of the series are defined
  tamx <- length(x)
  tamy <- length(y)
  
  #The local distance matrix is defined by using the Euclidean distance.
  distMatrix <- as.vector(t(proxy::dist(x, y, method="euclidean")))

  #The cost matrix is initialized and converted into a vector
  costMatrix <- c(1:((tamx+1) * (tamy+1))) * 0 + (max(distMatrix) * 
                                                length(distMatrix))

  
  #The case with no temporal constraint
  if (missing(sigma)){
    #The cost matrix is computed using dynammic programming.
    resultList<-.C("erpnw", as.double(x), as.double(y), as.integer(tamx),
                   as.integer(tamy), as.double(costMatrix), 
                   as.double(distMatrix), as.double(g))
    costMatrix<-resultList[[5]]
    
    #The case with a temporal constraint
  } else {
    #The cost matrix is computed using dynammic programming.
    resultList<-.C("erp", as.double(x), as.double(y), as.integer(tamx),
                   as.integer(tamy), as.integer(sigma),as.double(costMatrix), 
                   as.double(distMatrix), as.double(g))
    costMatrix <- resultList[[6]]
  }

  #The last position of the cost matrix is returned as the distance between 
  #the series.
  d<-costMatrix[length(costMatrix)]
  return(d)
  }
}


# This function checks for possible initial errors: 
erpInitialCheck <- function(x, y, g, sigma){
  
  if (!is.numeric(x) | !is.numeric(y)){
    stop('The series must be numeric', call.=FALSE)
  }
  if (!is.vector(x) | !is.vector(y)){
    stop('The series must be univariate vectors', call.=FALSE)
  }
  if (!is.numeric(g)){
    stop('g must be numeric', call.=FALSE)
  }
  if (length(x) < 1 | length(y) < 1){
    stop('The series must have at least one point', call.=FALSE)
  }
  if (any(is.na(x)) | any(is.na(y))){
    stop('There are missing values in the series', call.=FALSE)
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
      stop('The window size can not be lower than the difference between the series lengths', call.=FALSE)
    }
  }
}
