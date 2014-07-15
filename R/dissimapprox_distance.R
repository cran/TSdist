

dissimapproxDistance <- function(x, y, tx, ty){
  
  if (class(try(dissimInitialCheck(x, y, tx, ty)))=="try-error"){
    return(NA)
  }else{
   
  #If both temporal indices are missing, equal sampling is assumed and both
  #series begin and end in the same timestamp.
  if (missing(tx) & missing(ty)){
    tx <- seq(0, 1, length.out=length(x))
    ty <- seq(0, 1, length.out=length(y))
  }
  
  #If the temporal index of one of the series is missing, an equal starting and 
  #ending point is assumed and the series is sampled constantly in that interval.
  if (missing(tx)){
    tx <- seq(ty[1], ty[length(ty)], length.out=length(x))
  }
  if (missing(ty)){
    ty <- seq(tx[1], tx[length(tx)], length.out=length(y))
  }
  
  #If the temporal indices are different a global index is calculated 
  #taking into account both indexes.
  if (length(tx) != length(ty)){
    ind <- indglobal(tx, ty)
  }else{
    #If the temporal indices are different a global index is calculated.
    if(any(tx != ty)){
      ind <- indglobal(tx, ty)
    }else{
      #If the temporal indices are equal the global index is equal to them.
      ind <- tx
    }
  }

  #These arrays tell us in which interval of linear piece each point of
  #the global index is situated.
  xglobal <- indpos(tx, ind)
  yglobal <- indpos(ty, ind)

  #The linear approximation for each interval is created for series x.
  coefx <- xfun(x, tx)
  
  #The value of the piecewise function is calculated in each point
  #of the global index, for function x.
  xval <- ind*coefx[[1]][xglobal] + coefx[[2]][xglobal]
  xval[length(xval)] <- x[length(x)]

  #The linear approximation for each interval is created for series y.
  coefy <- xfun(y, ty)
  
  #The value of the piecewise function is calculated in each point
  #of the global index, for function y.
  yval <- ind*coefy[[1]][yglobal] + coefy[[2]][yglobal]
  yval[length(yval)] <- y[length(y)]

  #The approximated distance for each interval is created
  D <- sqrt((xval - yval) ^2)
  D1 <- D[1:(length(D) - 1)]
  D2 <- D[2:length(D)]

  #The total DISSIM distance is calculated.
  d <- 1/2 * sum( (D1+D2) * diff(ind) )
  return(d)
  }
}


