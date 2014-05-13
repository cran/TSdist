
#Wrapper function for all lp distances.
lpDistance<-function(x, y, method, ...){
  if (method == "euclidean"){
    d <- euclideanDistance(x, y)
  }
  if (method == "minkowski"){
    d <- minkowskiDistance(x, y, ...)
  }
  if (method == "infinitenorm"){
    d <- infiniteNormDistance(x, y)
  }
  if (method == "manhattan"){
    d <- manhattanDistance(x, y)
  }
  
  return(d)
}

#Euclidean distance
euclideanDistance <- function(x, y){
  lpInitialCheck(x, y)
  #The Euclidean distance between two series is computed.
  d <- sqrt(sum((x - y) ^ 2))
  return(d)
}

#Manhattan distance
manhattanDistance <- function(x, y){
  lpInitialCheck(x, y)
  #The Manhattan distance between two series is computed.
  d <- sum(abs(x - y))
  return(d)
}

#Infinite norm distance
infiniteNormDistance <- function(x, y){
  lpInitialCheck(x, y)
  #The supremum norm between two series is computed.
  d <- max(abs(x - y))
  return(d)
}

#Minkowski distance
minkowskiDistance <- function(x, y, p){
 lpInitialCheck(x, y, p)
  #The minkowsky distance with the chosen p value is computed.
  d <- (sum(abs(x - y) ^ p)) ^ (1 / p)
  return(d)
}


#This function checks for initial errors.
lpInitialCheck <- function(x, y, p){
  
  if (!is.numeric(x) | !is.numeric(y)){
    stop('The series must be numeric.', call.=FALSE)
  }
  if (length(x) < 1 | length(y) < 1){
    stop('The series must have at least one point.', call.=FALSE)
  }
  if (length(x) != length(y)){
    stop('Both series must have the same length.', call.=FALSE)
  }
  if (any(is.na(x)) | any(is.na(y))){
    stop('There are missing values in the series.', call.=FALSE)
  } 
  if (!missing(p)){
    
    if (round(p)!=p){
      stop('p must be an integer value.', call.=FALSE)
    }
    if (p <= 0){
      stop('p must be positive.', call.=FALSE)
    }
  } 
}