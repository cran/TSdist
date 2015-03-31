\name{ncdDistance}
\alias{ncdDistance}

\title{
Normalized Compression based distance
}
\description{
Calculates a normalized distance between two numerical series based on their compressed sizes.
}

\usage{
ncdDistance(x, y, ...)
}

\arguments{
  \item{x}{
  Numeric vector containing the first time series.
}

  \item{y}{
  Numeric vector containing the second time series.
}

  \item{...}{
   Additional parameters for the function. See  \code{\link[TSclust]{diss.NCD}} for more 
  information.
}
}

\details{
This is simply a wrapper for the \code{\link[TSclust]{diss.NCD}} function of package \pkg{TSclust}. As such, all the functionalities of the \code{\link[TSclust]{diss.NCD}} function are also available when using this function. 

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

#The objects example.series3 and example.series4 are two 
#numeric series of length 100 and 120 respectively. 

data(example.series3)
data(example.series4)

#For information on their generation and shape see 
#help page of example.series.

help(example.series)

#Calculate the normalized compression based distance between the two series 
#using default parameter.

ncdDistance(example.series3, example.series4)

}
