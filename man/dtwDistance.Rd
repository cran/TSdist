\name{dtwDistance}
\alias{dtwDistance}

\title{
Dynamic Time Warping distance. 
}
\description{
Computes the Dynamic Time Warping distance between a pair of
numeric time series.
}

\usage{
dtwDistance(x, y, sigma)
}

\arguments{
  \item{x}{
  Numeric vector containing the first time series.
}

  \item{y}{
  Numeric vector containing the second time series.
}

  \item{sigma}{
  If desired, a Sakoe-Chiba windowing contraint can be added by specifying 
  a positive integer representing the window size.
}
}

\details{
The basic Dynamic Time Warping distance between two numeric series is calculated.
This distance finds the minimal path in a distance matrix that defines a mapping between the two series that are being compared. This distance matrix is built by using the Euclidean distance and the path is sought by using dynammic programming.

Based on the definition of the distance measure, the length of series \code{x} and \code{y} may be 
different.

If desired, a temporal constraint may be added to the Dynamic Time Warping 
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
Berndt, D. J., & Clifford, J. (1994). \emph{Using Dynamic Time Warping to Find Patterns in Time Series}. In AAAI-94 Workshop on Knowledge Discovery in Databases. (pp. 359-370).


Sakoe, H., & Chiba, S. (1978). \emph{Dynamic programming algorithm optimization for spoken word recognition}. Acoustics, Speech, and Signal Processing [see Also {IEEE} Transactions on Signal Processing], {IEEE} Transactions on, 26(1), 43-49.
}

\author{
Usue Mori, Alexander Mendiburu, Jose A. Lozano.
}



\seealso{
To calculate a lower bound of the DTW distance see \code{\link{lbKeoghDistance}}.

To calculate this distance measure using \code{ts}, \code{zoo} or \code{xts} objects see \code{\link{tsDistances}}. To calculate distance matrices of time series databases using this measure see \code{\link{tsDatabaseDistances}}.
}
\examples{

#The objects example.series3 and example.series4 are two 
#numeric series of length 100 and 120 contained in the TSdist 
#package 


data(example.series3)
data(example.series4)

#For information on their generation and shape see 
#help page of example.series.

help(example.series)

#Calculate the DTW distance for two series of different length
#with no windowing constraint:

dtwDistance(example.series3, example.series4)

#Calculate the DTW distance for two series of different length
#with a window of size 30:

dtwDistance(example.series3, example.series4, sigma=30)

}
