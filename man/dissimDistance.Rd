\name{dissimDistance}
\alias{dissimDistance}


\title{
The Dissim distance is calculated.
}
\description{
Computes the Dissim distance between a pair of numeric series.
}
\usage{
dissimDistance(x, y, tx, ty)
}
\arguments{
  \item{x}{
  Numeric vector containing the first time series.
}
  \item{y}{
  Numeric vector containing the second time series.
}
  \item{tx}{
  If not constant, a numeric vector that specifies the sampling index of series \code{x}.
}
  \item{ty}{
  If not constant, a numeric vector that specifies the sampling index of series \code{y}.
}
}
\details{
The Dissim distance is obtained by calculating the integral of the Euclidean distance between the two series. The series are assumed to be linear between sampling points.

The two series must start and end in the same interval but they may have different and non-constant sampling rates. These sampling rates must be positive and strictly increasing. For more information see reference below.


}

\value{
\item{d}{
  The computed distance between the pair of series.
}

}

\references{
Frentzos, E., Gratsias, K., & Theodoridis, Y. (2007). \emph{Index-based Most Similar Trajectory Search}. In Proceeding of the IEEE 23rd International Conference on Data Engineering (pp. 816-825).
}

\author{
Usue Mori, Alexander Mendiburu, Jose A. Lozano.
}


\seealso{

To calculate an approximation of the Dissim distance see \code{\link{dissimapproxDistance}}.

To calculate this distance measure using \code{ts}, \code{zoo} or \code{xts} objects see \code{\link{tsDistances}}. To calculate distance matrices of time series databases using this measure see \code{\link{tsDatabaseDistances}}.
}
\examples{
#The objects example.series1 and example.series2 are two 
#numeric series of length 100 contained in the TSdist package. 

data(example.series1)
data(example.series2)

#For information on their generation and shape see help 
#page of example.series.

help(example.series)

#Calculate the Dissim distance assuming even sampling:

dissimDistance(example.series1, example.series2)

#Calculate the Dissim distance assuming uneven sampling:

tx<-unique(c(seq(2, 175, 2), seq(7, 175, 7)))
tx <- tx[order(tx)]
ty <- tx
dissimDistance(example.series1, example.series2, tx, ty)

}

