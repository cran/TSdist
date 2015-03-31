frechetDistance <- function (x, y, tx=NULL, ty=NULL, ...) {
  if(is.null(tx)){
    tx <- c(1:length(x))
  }
  if(is.null(ty)){
    ty <- c(1:length(y))
  }
  tryCatch({
  distFrechet(tx, x, ty, y, ...)},
  error=function(e){print(e);NA})   
}



###Special function to calculate Frechet distance for databases. 
# This function does not work with dist.

pairwise.frechetDistance <- function(X, Y=NULL, i, j, ...){
  
  #If data is given as matrix, convert to list
  if (!is.list(X)) {X <- matrix.to.list(X)}
  if (!is.null(Y) && !is.list(Y)) {Y <- matrix.to.list(Y)}
  if(is.null(Y)){
    as.numeric(frechetDistance(X[[i]], X[[j]], ...))
  }else{
    as.numeric(frechetDistance(X[[i]], Y[[j]], ...))
  }
}