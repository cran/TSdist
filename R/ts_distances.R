tsDistances <- function(x, y, tx=NULL, ty=NULL, distance, ...){

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

possible.distances <- c("euclidean", "manhattan", "minkowski", "infnorm", "ccor",
                        "sts", "dtw", "lb_keogh", "edr", "erp", "lcss", "fourier",
                        "tquest", "dissimfull", "dissimapprox", "acf", "pacf", 
                        "ar.lpc.ceps", "ar.mah", "ar.mah.statistic", "ar.mah.pvalue",
                        "ar.pic", "cdm", "cid", "cor", "cort", "wav", "int.per", 
                        "per", "mindist.sax", "ncd", "pred", "spec.glk", "spec.isd",
                        "spec.llr", "pdc", "frechet")
distance <- match.arg(distance, possible.distances)

#The distance is calculated.
d<-switch(distance, 
       "euclidean" = euclideanDistance(x, y),
       "manhattan" = manhattanDistance(x, y),
       "minkowski" = minkowskiDistance(x, y, ...), 
       "infnorm" = inf.normDistance(x, y),
       "ccor" = ccorDistance(x, y, ...),
       "sts" = stsDistance(x, y, tx, ty),
       "dtw" = dtwDistance(x, y, ...),
       "lb_keogh" = lb.keoghDistance(x, y, ...),
       "edr" = edrDistance(x, y, ...),
       "erp" = erpDistance(x, y, ...),
       "lcss" = lcssDistance(x, y, ...),
       "fourier" = fourierDistance(x, y, ...),
       "tquest" = tquestDistance(x, y, ...),
       "dissimfull" = dissimDistance(x, y, tx, ty),
       "dissimapprox" = dissimapproxDistance(x, y, tx, ty),
       "acf" = acfDistance(x, y, ...),
       "pacf" = pacfDistance(x, y, ...),
       "ar.lpc.ceps" = ar.lpc.cepsDistance(x, y, ...),
       "ar.mah" = ar.mahDistance(x, y, ...),
       "ar.mah.statistic" = ar.mah.statisticDistance(x, y, ...),
       "ar.mah.pvalue" = ar.mah.pvalueDistance(x, y, ...),
       "ar.pic" = ar.picDistance(x, y, ...),
       "cdm" = cdmDistance(x, y, ...),
       "cid" = cidDistance(x, y, ...),
       "cor" = corDistance(x, y, ...),
       "cort" = cortDistance(x, y, ...),
       "wav" = wavDistance(x, y, ...),
       "int.per" = int.perDistance(x, y, ...),
       "per" = perDistance(x, y, ...),
       "mindist.sax" = mindist.saxDistance(x, y, ...),
       "ncd" = ncdDistance(x, y, ...),
       "pred" = predDistance(x, y, ...),
       "spec.glk" = spec.glkDistance(x, y, ...),
       "spec.isd" = spec.isdDistance(x, y, ...),
       "spec.llr" = spec.llrDistance(x, y, ...),
       "pdc" = pdcDistance(x, y, ...),
       "frechet" = frechetDistance(x, y, tx, ty)
       )
  
return(d)

}


