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
This package provides a comprehensive set of distance measures published in the literature
and specifically aimed at time series data. The distance measures implemented are: 

\itemize{
\item   Lp distances. 
\item   Distances based on Pearson's correlation.
\item   Distance based on the cross-correlation.
\item Short Time Series distance (STS).
\item Dynamic Time Warping (DTW).
\item Edit Distance for Real Sequences (EDR).
\item Longest Common Subsequence distance for real sequences(LCSS).
\item Edit Distance based on Real Penalty (ERP).
\item Fourier Coefficient based distance.
\item TQuest distance.
\item Dissim distance.
\item Approximation of the Dissim distance.
}

All the distance measures are implemented in separate functions but can also be invoked by means of the wrapper function \code{\link{tsDistances}}. Moreover, this distance enables the use of time series objects of type \code{ts}, \code{zoo} and \code{xts}.

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
