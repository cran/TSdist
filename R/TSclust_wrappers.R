
####The distances in TSclust are calculated by means of the following 
####wrapper functions. The functions of the TSclust package are directly
####used.

#Autocorrelation based similarity

acfDistance <- function(x, y, ...){
  #If there is an error, NA is returned and the error message 
  #is printed. This enables executing in batch mode, without stops.
  tryCatch({
  as.numeric(diss.ACF(x, y, ...))}, 
  error=function(e){print(e);NA})  
}

#Partial autocorrelation based similarity

pacfDistance <- function(x, y, ...){
  #If there is an error, NA is returned and the error message 
  #is printed. This enables executing in batch mode, without stops.
  tryCatch({
  as.numeric(diss.PACF(x, y, ...))},
  error=function(e){print(e);NA})  
}

#Dissimilarity Based on LPC Cepstral Coefficients

ar.lpc.cepsDistance <- function(x, y, ...){
  #If there is an error, NA is returned and the error message 
  #is printed. This enables executing in batch mode, without stops.
  tryCatch({
  as.numeric(diss.AR.LPC.CEPS(x, y, ...))},
  error=function(e){print(e);NA})  
}

#Model-based Dissimilarity Proposed by Maharaj (1996, 2000)

ar.mahDistance <- function(x, y, ...){
  #If there is an error, NA is returned and the error message 
  #is printed. This enables executing in batch mode, without stops.
  tryCatch({
  diss.AR.MAH(x, y, ...)},
  error=function(e){print(e);NA})  
  }

ar.mah.statisticDistance <- function(x, y, ...){
  #If there is an error, NA is returned and the error message 
  #is printed. This enables executing in batch mode, without stops.
  tryCatch({
  as.numeric(diss.AR.MAH(x, y, ...)$statistic)},
  error=function(e){print(e);NA})   
}

ar.mah.pvalueDistance <- function(x, y, ...){
  #If there is an error, NA is returned and the error message 
  #is printed. This enables executing in batch mode, without stops.
  tryCatch({
  as.numeric(diss.AR.MAH(x, y, ...)$p_value)},
  error=function(e){print(e);NA})   
}

#Model-based Dissimilarity Measure Proposed by Piccolo (1990)

ar.picDistance <- function(x, y, ...){
  #If there is an error, NA is returned and the error message 
  #is printed. This enables executing in batch mode, without stops.
  tryCatch({
  as.numeric(diss.AR.PIC(x, y, ...))},
  error=function(e){print(e);NA})   
}

#Compression-based Dissimilarity measure

cdmDistance <- function(x, y, ...){
  #If there is an error, NA is returned and the error message 
  #is printed. This enables executing in batch mode, without stops.
  tryCatch({
  as.numeric(diss.CDM(x, y, ...))},
  error=function(e){print(e);NA})   
}

#Complexity-Invariant Distance Measure For Time Series

cidDistance <- function(x, y){
  #If there is an error, NA is returned and the error message 
  #is printed. This enables executing in batch mode, without stops.
  tryCatch({
  as.numeric(diss.CID(x, y))},
  error=function(e){print(e);NA})   
}

#Correlation-based Dissimilarity

corDistance <- function(x, y, ...){
  #If there is an error, NA is returned and the error message 
  #is printed. This enables executing in batch mode, without stops.
  tryCatch({
  as.numeric(diss.COR(x, y, ...))},
  error=function(e){print(e);NA})   
}

#Dissimilarity Index Combining Temporal Correlation and Raw Values
#Behaviors

cortDistance <- function(x, y, ...){
  #If there is an error, NA is returned and the error message 
  #is printed. This enables executing in batch mode, without stops.
  tryCatch({
  as.numeric(diss.CORT(x, y, ...))},
  error=function(e){print(e);NA})   
}

#Dissimilarity for Time Series Based on Wavelet Feature Extraction
wavDistance <- function(x, y, ...){
  #If there is an error, NA is returned and the error message 
  #is printed. This enables executing in batch mode, without stops.
  tryCatch({
  as.numeric(diss.DWT(rbind(x, y)))},
  error=function(e){print(e);NA})   
}


#Integrated Periodogram Based Dissimilarity

int.perDistance <- function(x, y, ...){
  #If there is an error, NA is returned and the error message 
  #is printed. This enables executing in batch mode, without stops.
  tryCatch({
  as.numeric(diss.INT.PER(x, y, ...))},
  error=function(e){print(e);NA})   
}

#Periodogram Based Dissimilarity

perDistance <- function(x, y, ...){
  #If there is an error, NA is returned and the error message 
  #is printed. This enables executing in batch mode, without stops.
  tryCatch({
  as.numeric(diss.PER(x, y, ...))},
  error=function(e){print(e);NA})   
}


#Symbolic Aggregate Aproximation related functions

mindist.saxDistance <- function(x, y, w, ...){
  #If there is an error, NA is returned and the error message 
  #is printed. This enables executing in batch mode, without stops.
  tryCatch({
  as.numeric(diss.MINDIST.SAX(x, y, w, ...))},
  error=function(e){print(e);NA})   
}

#Normalized Compression Distance

ncdDistance <- function(x, y, ...){
  #If there is an error, NA is returned and the error message 
  #is printed. This enables executing in batch mode, without stops.
  tryCatch({
  as.numeric(diss.NCD(x, y, ...))},
  error=function(e){print(e);NA})   
}

#Dissimilarity Measure Based on Nonparametric Forecast

predDistance <- function(x, y, h, ...){
  #If there is an error, NA is returned and the error message 
  #is printed. This enables executing in batch mode, without stops.
  tryCatch({
  as.numeric(diss.PRED(x, y, h, ...)$L1dist)},
  error=function(e){print(e);NA})   
}

#Dissimilarity based on the Generalized Likelihood Ratio Test

spec.glkDistance <- function(x, y, ...){
  #If there is an error, NA is returned and the error message 
  #is printed. This enables executing in batch mode, without stops.
  tryCatch({
  as.numeric(diss(rbind(x,y), "SPEC.GLK", ...))},
  error=function(e){print(e);NA})   
}

#Dissimilarity Based on the Integrated Squared Difference between the
#Log-Spectra

spec.isdDistance <- function(x, y, ...){
  #If there is an error, NA is returned and the error message 
  #is printed. This enables executing in batch mode, without stops.
  tryCatch({
  as.numeric(diss.SPEC.ISD(x, y, ...))},
  error=function(e){print(e);NA})   
}


#General Spectral Dissimilarity Measure Using Local-Linear Estima-
#tion of the Log-Spectra

spec.llrDistance <- function(x, y, ...){
  #If there is an error, NA is returned and the error message 
  #is printed. This enables executing in batch mode, without stops.
  tryCatch({
  as.numeric(diss.SPEC.LLR(x, y, ...))},
  error=function(e){print(e);NA})   
}

