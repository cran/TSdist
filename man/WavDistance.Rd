\name{WavDistance}
\alias{WavDistance}

\title{
Dissimilarity for Time Series Based on Wavelet Feature Extraction
}

\description{
Provides the dissimilarity between two numerical series of the same length by calculating
the Euclidean distance between the wavelet coefficients obtained from an orthogonal wavelet transform of the series. 
}

\usage{
WavDistance(x, y, ...)
}

\arguments{
  \item{x}{
  Numeric vector containing the first time series.
}

  \item{y}{
  Numeric vector containing the second time series.
}

  \item{...}{
   Additional parameters for the function. See  \code{\link[TSclust]{diss.DWT}} for more 
  information.
}
}

\details{
This is essentially a wrapper for the \code{\link[TSclust]{diss.DWT}} function of package \pkg{TSclust}. However, in this case, instead of introducing a matrix conformed of a set of time series, two numerical series of the same length are introduced.

\bold{Note:} This distance measure is based on the Euclidean distance. This means that it can be used directly within kernel machines, e.g. by inserting it in the Gaussian RBF kernel because it will provide positive definite Gram matrices. . 
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
To calculate this distance measure using \code{ts}, \code{zoo} or \code{xts} objects see \code{\link{TSDistances}}. To calculate distance matrices of time series databases using this measure see \code{\link{TSDatabaseDistances}}.
}

\examples{

# The objects example.series1 and example.series2 are two 
# numeric series of length 100.

data(example.series1)
data(example.series2)

# For information on their generation and shape see 
# help page of example.series.

help(example.series)

# Calculate the distance based on wavelet feature extraction between the series.

WavDistance(example.series1, example.series2)



}
