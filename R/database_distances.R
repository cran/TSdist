
tsDatabaseDistances <- function(X, Y=NULL, distance=NULL, ...){
  
  possible.distances <- c("euclidean", "manhattan", "minkowski", "infnorm", "ccor",
                          "sts", "dtw", "lb_keogh", "edr", "erp", "lcss", "fourier",
                          "tquest", "dissimfull", "dissimapprox", "acf", "pacf", 
                          "ar.lpc.ceps", "ar.mah", "ar.mah.statistic", "ar.mah.pvalue",
                          "ar.pic", "cdm", "cid", "cor", "cort", "wav", "int.per", 
                          "per", "mindist.sax", "ncd", "pred", "spec.glk", "spec.isd",
                          "spec.llr", "pdc", "frechet")
  
  
  distance <- match.arg(distance, possible.distances)

  #If Y does not appear, redefine input parameters
  if (is.character(Y)) {
    distance <- Y
    Y <- NULL
  }
  #If no method is given, the euclidean distance is calculated.
  if (is.null(distance)){
    distance <- "euclidean"
  }
  
  #Initial checks
  if (!is.numeric(X) & !is.matrix(X) & !is.mts(X) &  !is.zoo(X) & !is.xts(X) & !is.list(X)){
    stop('X must be a matrix, mts, zoo, xts or list object.')
  }  
  if(is.mts(X)){
    X <- t(X)
    tx <- as.numeric(time(X))
  }
  if(is.zoo(X) | is.xts(X)){
    X <- t(X)
    tx <- as.numeric(index(X))
  }else{
    tx<-NULL
  }
  
  if(!is.list(X)){
    if(dim(X)[1] <= 1){
      stop('The database must contain more than one series.')}
  }else{
    if(length(X)<=1){
      stop('The database must contain more than one series.')}  
  }
  

  #Distance calculations for only one database
  if(is.null(Y)){
 
    #Calculate distance matrix
    #Special cases of TSclust (more efficient than using dist.)
    if(distance=="ar.mah"){
      d1 <- dist(X, method = "tsDistances", distance = "ar.mah.statistic")
      d2 <- dist(X, method = "tsDistances", distance = "ar.mah.pvalue")
      d <- list(statistic=d1, pvalue=d2)
    }else if(distance=="ar.pic"){
      d <- as.dist(pairwise.distances1(X, pairwise.ar.picDistance, ...))
    }else if(distance=="ar.lpc.ceps"){
      d <- as.dist(pairwise.distances1(X, pairwise.ar.lpc.cepsDistance, ...))
    }else if(distance=="pred"){
      d <- as.dist(pairwise.predDistance(X, Y=NULL, ...))
    }else if(distance=="spec.llr"){
      d <- as.dist(pairwise.spec.llrDistance(X, Y=NULL, ...))
    }else if(distance=="spec.glk"){
      d <- as.dist(pairwise.spec.glkDistance(X, Y=NULL, ...))
    }else if(distance=="spec.isd"){ 
      d <- as.dist(pairwise.spec.isdDistance(X, Y=NULL, ...))
    }else if(distance=="cdm"){ 
      d <- as.dist(pairwise.distances1(X, pairwise.cdmDistance, ...))
    }else if(distance=="ncd"){ 
      d <- as.dist(pairwise.distances1(X, pairwise.ncdDistance, ...))
    }else if(distance=="wav"){
      options(show.error.messages = FALSE)
      d <- diss.DWT(X)
    #For the PDC distance we use the original function from package pdc.
    #Faster than  using dist.
    }else if(distance=="pdc"){
      d <- pdcDist(t(X))
    }else if(distance=="frechet"){
      d <- as.dist(pairwise.distances1(X, pairwise.frechetDistance, ...))
      #For the rest of the cases: we use dist.
    }else{
      options(show.error.messages = FALSE)
      d <- dist(X, method = "tsDistances", distance = distance, tx=tx, ty=tx, ...)
    }
   
  #Calculate pairwise distances between series of 2 databases.
  #For TRAIN/TEST environments
  }else{
    
    #Initial checks.
    if (!is.numeric(Y) & !is.matrix(Y) & !is.mts(Y) &  !is.zoo(Y) & !is.xts(Y) & !is.list(Y)){
      stop('x must be a matrix, mts, zoo, xts or list object.')
    } 
    if(is.mts(Y)){
      Y <- t(Y)
      ty <- as.numeric(time(Y))
    }else if(is.zoo(Y) | is.xts(Y)){
      Y <- t(Y)
      ty <- as.numeric(index(Y))
    }else{
      ty <- NULL
    }
    
    if(!is.list(Y)){
      if(dim(Y)[1] <= 1){
        stop('The database must contain more than one series.')}
    }else{
      if(length(Y)<=1){
        stop('The database must contain more than one series.')}  
    }
    
    #Special cases of TSclust (more efficient than using dist.)
    if(distance=="ar.mah"){
      d1 <- dist(X, Y, method = "tsDistances", distance = "ar.mah.statistic")
      d2 <- dist(X, Y, method = "tsDistances", distance = "ar.mah.pvalue")
      d <- list(statistic=d1, pvalue=d2)
    }else if(distance=="ar.pic"){
      d <- pairwise.distances2(X, Y, pairwise.ar.picDistance, ...)
    }else if(distance=="ar.lpc.ceps"){
      d <- pairwise.distances2(X, Y, pairwise.ar.lpc.cepsDistance, ...)
    }else if(distance=="pred"){
      d <- pairwise.predDistance(X, Y, ...)
    }else if(distance=="spec.llr"){
      d <- pairwise.spec.llrDistance(X, Y, ...)
    }else if(distance=="spec.glk"){
      d <- pairwise.spec.glkDistance(X, Y, ...)
    }else if(distance=="spec.isd"){ 
      d <- pairwise.spec.isdDistance(X, Y, ...)
    }else if(distance=="cdm"){ 
      d <- pairwise.distances2(X, Y, pairwise.cdmDistance, ...)
    }else if(distance=="ncd"){ 
      d <- pairwise.distances2(X, Y, pairwise.ncdDistance, ...)
    }else if(distance=="frechet"){
      d <- pairwise.distances2(X, Y, pairwise.frechetDistance, ...)
      #For the rest of the cases: we use dist.
    }else if(distance=="pdc"){
      if(is.list(X)){X <- do.call(rbind, X)}
      d <- pdcDist2(t(X),t(Y))
    #Both the training and testing databases are used for feature extraction.
    }else if(distance=="wav"){
      if(is.matrix(X)){n <- dim(X)[1]}
      if(is.list(X)){n <- length(X)}
      if(is.matrix(Y)){m <- dim(Y)[1]}
      if(is.list(Y)){m <- length(Y)}
      d <- truncate(diss.DWT(rbind(X, Y)), n, m)
      #Rest of the cases: we use dist.
    }else{
      options(show.error.messages = FALSE)
      d <- dist(X, Y, method = "tsDistances", distance = distance, tx=tx, ty=ty, ...)
    }
  }
  options(show.error.messages = TRUE)
  return(d) 
}
 
   
  