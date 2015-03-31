
pdcDistance <- function(x, y, ...){
  tryCatch({
  as.numeric(pdcDist(cbind(x,y), ...))},
  error=function(e){print(e);NA})   
}

###ADDITIONAL FUNCTIONS FOR TRAIN/TEST DISTANCE MATRIX CALCULATION###

divergenceMatrix2 <-
  function(codebooks1, codebooks2, divergence)
  {
    l1 <- dim(codebooks1)[1]
    l2 <- dim(codebooks2)[1]
    mt <- matrix(rep(0,l1*l2),l1,)
    for (i in 1:l1)
    {
      for (j in 1:l2)
      {
        mt[i,j] <- divergence(codebooks1[i,], codebooks2[j,])
      }
    }
    return(mt);
  }


pdcDist2 <-
  function(X, Y, m=NULL, t=NULL, divergence=symmetricAlphaDivergence)
  {
    if (is.null(t) | is.null(m)) {
      ent <- entropyHeuristic(cbind(X,Y))
      
      if (is.null(m)) {
        m <- ent$m;
      }
      
      if (is.null(t)) {
        t <- ent$t;
      }
    }
    
      codebooks1 <- convertMatrix(X,m,t);
      codebooks2 <- convertMatrix(Y,m,t);
      D <- divergenceMatrix2( codebooks1,  codebooks2, divergence );
      
    
        
    return(D);
  }

#Internal function used by pdc to calculate the permutation distributions from 
#the series
convertMatrix <-function(X, m, td){
  return( t(apply(X,2,codebook,m=m, t=td)) )
}