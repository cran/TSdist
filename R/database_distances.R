
tsDatabaseDistances <- function(x, method, diag=FALSE, upper=FALSE, ...){

if (!is.numeric(x) & !is.mts(x) &  !is.zoo(x) & !is.xts(x) & !is.list(x)){
  stop('x must be a matrix, mts, zoo, xts or list object.')
}
  
#Distance computation for mts objects.
if(is.mts(x)){
  if(dim(x)[2] <= 1){
    stop('The database must contain more than one series.')
  }
  options(show.error.messages = FALSE)
  d<-dist(t(x), method = "tsDistances", distance = method, tx=as.numeric(time(x)), ty=as.numeric(time(x)), diag=diag, upper=upper, ...)
}

#Distance computation for zoo and xts objects.
else if(is.zoo(x) | is.xts(x)){
  if(dim(x)[2] <= 1){
    stop('The database must contain more than one series.')
  }
  options(show.error.messages = FALSE)
  d<-dist(t(x), method = "tsDistances", distance = method, tx=as.numeric(index(x)), ty=as.numeric(index(x)), diag=diag, upper=upper, ...)
}

#Distance computation for lists.
else if(is.list(x)){
  if(length(x) <= 1){
    stop('The database must contain more than one series.')
  }
  k<-length(x[[1]])
  for(i in c(1:length(x))){
    if(length(x[[i]]) != k){
      stop('Time series must be of the same length.')
    }
  }
  x<- matrix(unlist(x), ncol = k, byrow = TRUE)
  options(show.error.messages = FALSE)
  d<-dist(x, method = "tsDistances", distance = method, ...)
}

#Distance computation for numeric matrices.
else{
  
  if(dim(x)[1] <= 1){
    stop('The database must contain more than one series.')
  }
  options(show.error.messages = FALSE)
  d<-dist(x, method = "tsDistances", distance = method, diag=diag, upper=upper, ...)
}
  options(show.error.messages = TRUE)
  return(d)
}