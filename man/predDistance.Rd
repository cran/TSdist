\name{predDistance}
\alias{predDistance}

\title{
Dissimilarity Measure Based on Nonparametric Forecasts
}
\description{
The dissimilarity of two numerical series of the same length is calculated based on the 
L1 distance between the kernel estimators of their forecast densities at a given
time horizon.
}

\usage{
predDistance(x, y, h, ...)
}

\arguments{
  \item{x}{
  Numeric vector containing the first time series.
}

  \item{y}{
  Numeric vector containing the second time series.
}

  \item{h}{
  Integer value representing the prediction horizon.
}

  \item{...}{
   Additional parameters for the function. See  \code{\link[TSclust]{diss.PRED}} for more 
  information.
}
}

\details{
This is simply a wrapper for the \code{\link[TSclust]{diss.PRED}} function of package \pkg{TSclust}. As such, all the functionalities of the \code{\link[TSclust]{diss.PRED}} function are also available when using this function. 

}

\value{
\item{d}{
  The computed distance between the pair of series.
}
}

\references{
Pablo Montero, José A. Vilar (2014). TSclust: An R Package for Time Series
  Clustering. Journal of Statistical Software, 62(1), 1-43. URL
  http://www.jstatsoft.org/v62/i01/.
}

\author{
Usue Mori, Alexander Mendiburu, Jose A. Lozano.
}



\seealso{

To calculate this distance measure using \code{ts}, \code{zoo} or \code{xts} objects see \code{\link{tsDistances}}. To calculate distance matrices of time series databases using this measure see \code{\link{tsDatabaseDistances}}.

}

\examples{

#The objects example.series1 and example.series2 are two 
#numeric series of length 100. 

data(example.series1)
data(example.series2)

#For information on their generation and shape see 
#help page of example.series.

help(example.series)

#Calculate the prediction based distance between the two series using
#the default parameters. 

predDistance(example.series1, example.series2)

}
