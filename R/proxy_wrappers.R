
#Euclidean distance

euclideanDistance <- function(x, y){
  if (class(try(lpInitialCheck(x, y)))=="try-error"){
    return(NA)
  }else{
  d <- as.numeric(dist(rbind(x,y), "euclidean")) 
  return(d)
  }
}


#Manhattan distance
manhattanDistance <- function(x, y){
  if (class(try(lpInitialCheck(x, y)))=="try-error"){
    return(NA)
  }else{
    #The Manhattan distance between two series is computed.
    d <- as.numeric(dist(rbind(x,y), "manhattan")) 
    return(d)
  }
}

#Infinite norm distance
inf.normDistance <- function(x, y){
  if (class(try(lpInitialCheck(x, y)))=="try-error"){
    return(NA)
  }else{
    #The supremum norm between two series is computed.
    d <- as.numeric(dist(rbind(x,y), "supremum")) 
    return(d)
  }
}

#Minkowski distance
minkowskiDistance <- function(x, y, p){
  if (class(try(lpInitialCheck(x, y, p)))=="try-error"){
    return(NA)
  }else{
    #The minkowsky distance with the chosen p value is computed.
    d <- as.numeric(dist(rbind(x,y), "minkowski", p=p)) 
    return(d)
  }
}


  
  
  
