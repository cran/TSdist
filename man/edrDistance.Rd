\name{edrDistance}
\alias{edrDistance}

\title{
Edit Distance for Real Sequences (EDR). 
}
\description{
Computes the Edit Distance for Real Sequences  between a pair of 
numeric time series.
}

\usage{
edrDistance(x, y, epsilon, sigma)
}

\arguments{
  \item{x}{
  Numeric vector containing the first time series.
}
  \item{y}{
  Numeric vector containing the second time series.
}

\item{epsilon}{
  A positive threshold value that defines the distance.
}

\item{sigma}{
  If desired, a Sakoe-Chiba windowing contraint can be added by specifying 
  a positive integer representing the window size.
}

}
\details{
The basic Edit Distance for Real Sequences between two numeric series is calculated. The idea is to count the number of edit operations (insert, delete, replace) that are necessary to transform one series into the other. 

For that, if the Euclidean distance between two points \eqn{x_i} and \eqn{y_i} is smaller that \code{epsilon} they will be considered equal (\eqn{d=0}) and if they are farther apart, they will be considered different (\eqn{d=1}).
As a last detail, this distance permits gaps or sequences of points that are not matched with any other point.

The length of series \code{x} and \code{y} may be different. Furthermore, if desired, a temporal constraint may be added to the EDR 
distance. In this package, only the most basic windowing function, introduced 
by H.Sakoe and S.Chiba (1978), is implemented. This function sets a band around the main diagonal of the distance matrix and avoids the matching of the points that are farther in time than a specified \eqn{\sigma}. 

The size of the window must be a positive integer value, not larger than the size of both of the series. Furthermore, the following condition must be fulfilled:

\deqn{|length(x)-length(y)| < sigma}


}

\value{
\item{d}{
  The computed distance between the pair of series.
}
}

\references{
Chen, L., Ozsu, M. T., & Oria, V. (2005). \emph{Robust and Fast Similarity Search for Moving Object Trajectories}. In Proceedings of the 2005 ACM SIGMOD International Conference on Management of Data (pp. 491-502).
}

\author{
Usue Mori, Alexander Mendiburu, Jose A. Lozano.
}

\seealso{
To calculate this distance measure using \code{ts}, \code{zoo} or \code{xts} objects see \code{\link{tsDistances}}. To calculate distance matrices of time series databases using this measure see \code{\link{tsDatabaseDistances}}.
}

\examples{

#The objects example.series3 and example.series4 are two 
#numeric series of length 100 and 120 contained in the TSdist
#package. 

data(example.series3)
data(example.series4)

#For information on their generation and shape see 
#help page of example.series.

help(example.series)

#Calculate the EDR distance for two series of different length
#with no windowing constraint:

edrDistance(example.series3, example.series4, epsilon=0.1)

#Calculate the EDR distance for two series of different length
#with a window of size 30:

edrDistance(example.series3, example.series4, epsilon=0.1, sigma=30)

}
