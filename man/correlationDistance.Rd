\name{correlationDistance}
\alias{correlationDistance}

\title{
Pearson correlation based distance. 
}
\description{
Computes two different distance measures based on Pearson's correlation between a pair of numeric time series.
}

\usage{
correlationDistance(x, y, beta)
}

\arguments{
  \item{x}{
  Numeric vector containing the first time series.
}

  \item{y}{
  Numeric vector containing the second time series.
}

  \item{beta}{
  Positive parameter. If defined, the second Pearson correlation based distance measure will be computed.
}
}
\details{
Two different distances based on Pearson's correlation are calculated between
two numeric time series. The series must be of the same length.

If \code{beta} is not specified the distance is calculated 
as follows:

 \deqn{D1=2 (1 - Cor(x, y))} 
 
 If \code{beta} is specified, it must be positive and, the following formula is used to compute the distance:

 \deqn{D2=( (1 - Cor(x, y)) / (1 + Cor(x, y)) ) ^ \beta} 
 
 where \eqn{Cor(x,y)} is Pearson's correlation between \eqn{x} and \eqn{y}.


}

\value{
\item{d}{
  The computed distance between the pair of series.
}

}

\references{
Golay, X., Kollias, S., Stoll, G., Meier, D., Valavanis, A., and Boesiger, P. (2005) \emph{A new correlation-based fuzzy logic clustering algorithm for FMRI}. Magnetic Resonance in Medicine, 40(2), 249--260.
}

\author{
Usue Mori, Alexander Mendiburu, Jose A. Lozano.
}


\seealso{

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

#Calculate the Pearson correlation based distance using the first method

correlationDistance(example.series1, example.series2)

#Calculate the Pearson correlation based distance using the second method 
#with beta=3.

correlationDistance(example.series1, example.series2, beta=3)

}

