
#This function calculates a distance based on the cross-correlation.
crossCorrelationDistance <- function(x, y, lag.max=(min(length(x), length(y))-1)){
 
  #Some initial errors are checked
  if(class(try(ccInitialCheck(x, y, lag.max)))=="try-error"){return(NA)}
  
  #Calculate the correlation between the two series up to the maximum lag, 
  # if there are no errors.
  err<-try(cc <- ccf(x, y, lag.max=lag.max, type="correlation", plot="FALSE"))
  
  if(class(err)=="try-error"){  
    return(NA)
  }else{
    #If there are no errors, calculate the distance measure using 
    #the correlations obtained previously
    d <- sqrt((1-round(cc$acf[, , 1][which(cc$lag==0)] ^ 2,digits=5))/
                mean(cc$acf[, , 1] ^ 2))
    return(d)
  }
}


ccInitialCheck <- function(x, y, lag.max){
  
  if (!is.numeric(x) | !is.numeric(y)){
    stop('The series must be numeric', call.=FALSE)
  }
  if (!is.vector(x) | !is.vector(y)){
    stop('The series must be univariate vectors', call.=FALSE)
  }
  if (length(x) <= 1 | length(y) <= 1){
    stop('The series must have more than one point', call.=FALSE)
  }
  if (lag.max < 0) {
    stop ('The maximum lag value must be positive', call.=FALSE)
  }
  if (lag.max >= length(x)) {
    stop ('The maximum lag value exceeds the length of the first series', call.=FALSE)
  }
  if (lag.max >= length(y)) {
    stop ('The maximum lag value exceeds the length of the second series', call.=FALSE)
  }
  if (any(is.na(x)) | any(is.na(y))){
    stop('There are missing values in the series', call.=FALSE)
  } 
}