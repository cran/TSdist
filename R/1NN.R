oneNN <- function(train, trainc, test, testc, distance, ...){
  
  if(is.character(testc)){
    distance <- testc
    testc <- NULL
    }
  d <- as.matrix(tsDatabaseDistances(train, test, distance=distance, ...))
  #We select nearest neighbors
  nn <- apply(d, 2, function(x) {which(x==min(x))})
  #We select randomly if there are ties
  class <- trainc[as.numeric(lapply(nn, select))]
  if(!is.null(testc)){
    e <- sum(class!=testc)/length(testc)
    return(list(classes=class, error=e))
  }else{
    return(list(classes=class))  
  }
}

#function to select randomly if there are ties
select <- function(vector){
  if(length(vector)==1){return(vector[1])
  }else{return(sample(vector,1))}
}