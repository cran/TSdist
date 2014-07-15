tsDistances <- function(x, y, tx, ty, distance, ...){

#If x is given as a ts object, the values and the time index are extracted.
if(is.ts(x)){
  tx<-as.numeric(time(x))
  x<-as.numeric(x)
}  
  
#If y is given as a ts object, the values and the time index are extracted.
if(is.ts(y)){
  ty<-as.numeric(time(y))
  y<-as.numeric(y)
} 

#If x is given as a xts or a zoo object, the values and the time index are extracted.
if(is.zoo(x) | is.xts(x)){
  tx<-as.numeric(index(x))
  x<-as.numeric(x)
}  
  
#If y is given as a xts or a zoo object, the values and the time index are extracted.
if(is.zoo(y) | is.xts(y)){
  ty<-as.numeric(index(y))
  y<-as.numeric(y)
} 

#If x is given as a numerical vector but the time index is not provided
#a constant sampling rate is assumed.
if(is.numeric(x) & missing(tx)){
  tx <- c(1:length(x))
} 

#If y is given as a numerical vector but the time index is not provided
#a constant sampling rate is assumed.
if(is.numeric(x) & missing(ty)){
  ty <- c(1:length(y))
}

#The distance is calculated.
d<-switch(distance, 
       "euclidean" = euclideanDistance(x, y),
       "manhattan" = manhattanDistance(x, y),
       "minkowski" = minkowskiDistance(x, y, ...), 
       "infinitenorm" = infiniteNormDistance(x, y),
       "pearsoncorrelation" = correlationDistance(x, y, ...),
       "crosscorrelation" = crossCorrelationDistance(x, y, ...),
       "sts" = stsDistance(x, y, tx, ty),
       "dtw" = dtwDistance(x, y, ...),
       "lb_keogh" = lbKeoghDistance(x, y, ...),
       "edr" = edrDistance(x, y, ...),
       "erp" = erpDistance(x, y, ...),
       "lcss" = lcssDistance(x, y, ...),
       "fourier" = fourierDistance(x, y, ...),
       "tquest" = tquestDistance(x, y, ...),
       "dissim" = dissimDistance(x, y, ...),
       "dissimapprox" = dissimapproxDistance(x, y, tx, ty),
       )
  
return(d)

}


