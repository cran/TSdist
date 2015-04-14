\name{TSdist-package}
\alias{TSdist-package}
\alias{TSdist}
\docType{package}
\title{
 Distance Measures for Time Series in R. 
}
\description{
A complete set of distance measures specifically designed to deal with time series.
}
\details{
\tabular{ll}{
Package: \tab TSdist\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-05-10\cr
License: \tab GPL (>=2) \cr
}
This package provides a comprehensive set of time series distance measures  published in the literature and some additional functions which, although initially not designed for this purpose, can be used to measure the dissimilarity between time series. These measures can be used to perform clustering, classification or other data mining tasks which require the definition of a distance measure between time series. Some of the measures are specifically implemented for this package while other are originally hosted in other R packages. The measures included are: 

\itemize{
\item   Lp distances \code{\link{lpDistance}}
\item   Distance based on the cross-correlation \code{\link{ccorDistance}}
\item Short Time Series distance (STS) \code{\link{stsDistance}}
\item Dynamic Time Warping (DTW) \code{\link{dtwDistance}}
\item LB_Keogh lower bound for the Dynamic Time Warping distance \code{\link{lb.keoghDistance}}
\item Edit Distance for Real Sequences (EDR) \code{\link{edrDistance}}
\item Longest Common Subsequence distance for real sequences(LCSS)  \code{\link{lcssDistance}}
\item Edit Distance based on Real Penalty (ERP) \code{\link{erpDistance}}
\item Distance based on the Fourier Discrete Transform \code{\link{fourierDistance}}
\item TQuest distance \code{\link{tquestDistance}}
\item Dissim distance \code{\link{dissimDistance}}
\item Approximation of the Dissim distance \code{\link{dissimapproxDistance}}
\item Autocorrelation-based dissimilarity \code{\link{acfDistance}}. 
\item  Partial autocorrelation-based dissimilarity \code{\link{pacfDistance}}.
\item Dissimilarity based on LPC cepstral coefficients \code{\link{ar.lpc.cepsDistance}}. 
\item Model-based dissimilarity proposed by Maharaj (1996, 2000) \code{\link{ar.mahDistance}}.
\item Compression-based dissimilarity measure \code{\link{cdmDistance}}. 
\item Complexity-invariant distance measure \code{\link{cidDistance}}. 
\item Dissimilarities based on Pearson's correlation \code{\link{corDistance}}. 
\item Dissimilarity index which combines temporal correlation and raw value
behaviors \code{\link{cortDistance}}. 
\item Dissimilarity based on wavelet feature extraction \code{\link{wavDistance}}. 
\item Integrated periodogram based dissimilarity \code{\link{int.perDistance}}. 
\item Periodogram based dissimilarity \code{\link{perDistance}}. 
\item Symbolic Aggregate Aproximation based dissimilarity  \code{\link{mindist.saxDistance}}. 
\item Normalized compression based distance \code{\link{ncdDistance}}. 
\item Dissimilarity measure cased on nonparametric forecasts \code{\link{predDistance}}. 
\item Dissimilarity based on the integrated squared difference between the log-spectra \code{\link{spec.isdDistance}}. 
\item General spectral dissimilarity measure using local-linear estimation of the log-spectra \code{\link{spec.llrDistance}}. 
\item Permutation Distribution Distance \code{\link{pdcDistance}}. 
\item Frechet distance \code{\link{frechetDistance}}. 
}

All the measures are implemented in separate functions but can also be invoked by means of the wrapper function \code{\link{tsDistances}}. Moreover, this distance enables the use of time series objects of type \code{ts}, \code{zoo} and \code{xts}.

As an additional functionality of the package, pairwise distances between all the time series in a database can be easily computed by using the \code{\link[proxy]{dist}} function from the \pkg{proxy} package or the \code{\link{tsDatabaseDistances}} function included in the \pkg{TSdist} package.
}
\author{
Usue Mori, Alexander Mendiburu, Jose A. Lozano.
Maintainer:  <usue.mori@ehu.es>
}

\references{
Esling, P., & Agon, C. (2012). \emph{Time-series data mining.} ACM Computing Surveys, 45(1), 1-34.

Liao, T. W. (2005). \emph{Clustering of time series data-a survey}. Pattern Recognition, 38(11), 1857-1874.

Wang, X., Mueen, A., Ding, H., Trajcevski, G., Scheuermann, P., & Keogh, E. (2012). \emph{Experimental comparison of representation methods and distance measures for time series data.} Data Mining and Knowledge Discovery, 26(2), 275-309.

David Meyer and Christian Buchta (2013). proxy: Distance and Similarity Measures. R package version 0.4-10. http://CRAN.R-project.org/package=proxy
}

\examples{
 library(TSdist);
}
