\name{lcssDistance}
\alias{lcssDistance}

\title{
Longest Common Subsequence distance for Real Sequences. 
}
\description{
Computes the Longest Common Subsequence distance between a pair of 
numeric time series.
}

\usage{
lcssDistance(x, y, epsilon, sigma)
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
The Longest Common Subsequence for two real sequences is computed. 

For this purpose, 
the distances between the points of \code{x} and \code{y} are reduced to 0 or 1. If the Euclidean distance between two points \eqn{x_i} and \eqn{y_j} is smaller than \code{epsilon} they are considered
equal and their distance is reduced to 0. In the opposite case, 
the distance between them is represented with a value of 1. 

Once the distance matrix is defined in this manner, the maximum common subsequence is seeked. Of course, as in other Edit Based Distances, gaps or unmatched regions are permitted and they are penalized with a value proportional to their length.

Based on its definition, the length of series \code{x} and \code{y} may be 
different.

If desired, a temporal constraint may be added to the LCSS 
distance. In this package, only the most basic windowing function, introduced 
by H.Sakoe and S.Chiba (1978), is implemented. This function sets a band around the 
main diagonal of the distance matrix and avoids the matching of the points that are farther in time than a specified \eqn{\sigma}. 

The size of the window must be a positive integer value, not larger than the size of both of the series. Furthermore, the following condition must be fulfilled:

\deqn{|length(x)-length(y)| < sigma}


}

\value{
\item{d}{
  The computed distance between the pair of series.
}

}

\references{
Vlachos, M., Kollios, G., & Gunopulos, D. (2002). \emph{Discovering similar multidimensional trajectories}. In Proceedings 18th International Conference on Data Engineering (pp. 673-684). IEEE Comput. Soc. doi:10.1109/ICDE.2002.994784
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

#Calculate the LCSS distance for two series of different length
#with no windowing constraint:

lcssDistance(example.series3, example.series4, epsilon=0.1)

#Calculate the LCSS distance for two series of different length
#with a window of size 30:

lcssDistance(example.series3, example.series4, epsilon=0.1, sigma=30)

}
