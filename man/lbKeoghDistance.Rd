\name{lbKeoghDistance}
\alias{lbKeoghDistance}

\title{
LB_Keogh for DTW.
}
\description{
Computes the Keogh lower bound for the Dynamic Time Warping distance between a pair of numeric time series.
}

\usage{
lbKeoghDistance(x, y, window.size)
}

\arguments{
  \item{x}{
  Numeric vector containing the first time series (query time series).
}

  \item{y}{
  Numeric vector containing the second time series (reference time series).
}

  \item{window.size}{
  Window size that defines the upper and lower envelopes.
}
}

\details{

The lower bound introduced by Keogh and Ratanamahatana (2005) is calculated for the Dynamic Time Warping distance. Given \code{window.size}, the width of a Sakoe-Chiba band, an upper and lower envelope of the query time series is calculated in the following manner: 

\deqn{U[i] = max(x[i - window.size], x[i + window.size])}
\deqn{L[i] = min(x[i - window.size], x[i + window.size])}

Based on this, the Keogh_LB distance is calculated as the Euclidean distance between the points in the reference time series (\code{y}) that fall outside both the lower and upper envelopes, and their nearest point of the corresponding envelope.

The series must have the same length. Furthermore, the width of the window should be even in order to assure a symmetric band around the diagonal and should not exceed the length of the series.


}

\value{
\item{d}{
  The computed distance between the pair of series.
}

}

\references{
Keogh, E., & Ratanamahatana, C. A. (2004). Exact indexing of dynamic time warping. Knowledge and Information Systems, 7(3), 358-386.

Sakoe, H., & Chiba, S. (1978). \emph{Dynamic programming algorithm optimization for spoken word recognition}. {IEEE} Transactions on Acoustics, Speech, and Signal Processing, 26(1), 43-49.
}

\author{
Usue Mori, Alexander Mendiburu, Jose A. Lozano.
}



\seealso{
To calculate the full DTW distance see \code{\link{dtwDistance}}.

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

#Calculate the LB_Keogh distance measure for these two series
#with a window of band of width 11:

lbKeoghDistance(example.series1, example.series2, window.size=11)



}
