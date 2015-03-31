
#Calculate the k partitioning around medoids clustering for a given distance measure
#If the ground truth is available we can evaluate it using the F-measure.
k.medoids <- function(data, k,  ground.truth, distance, ...){
  if(is.character(ground.truth)){
    distance <- ground.truth
    ground.truth <- NULL
  }
  
  d <- as.matrix(tsDatabaseDistances(data, distance=distance, ...))
  clus <- pam(d, k=k, diss=TRUE)
  
  if(!is.null(ground.truth)){
    f <- F(clus$clustering, ground.truth)
    return(list(clustering=clus$clustering, F=f))
  }else{
    return(clus$clustering)
  }
}

#F-measure for evaluating clusterings
F<-function(clus,true){
  
  tab1<-2*table(clus,true)
  tab2<-outer(rowSums(tab1),colSums(tab1),`+`)
  result<-1/length(true)*sum(colSums(tab1)*apply((tab1/tab2),2,max))
  return(result)
  
}