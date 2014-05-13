\name{euclideanDistance}
\alias{euclideanDistance}



\title{
Euclidean distance. 
}
\description{
Computes the Euclidean distance between a pair of numeric vectors.
}

\usage{
euclideanDistance(x, y)
}

\arguments{
  \item{x}{
  Numeric vector containing the first time series.
}

  \item{y}{
  Numeric vector containing the second time series.
}
}

\details{
The Euclidean distance is computed between the two numeric series using the following formula: 

\deqn{D=\sqrt{(x_i-y_i)^2)}} 

The two series must have the same length. 


}

\value{
\item{d}{
 The computed distance between the pair of series.
}
}

\author{
Usue Mori, Alexander Mendiburu, Jose A. Lozano.
}


\seealso{
This function can also be invoked by the wrapper function \code{\link{lpDistance}}.

Furthermore, to calculate this distance measure using \code{ts}, \code{zoo} or \code{xts} objects see \code{\link{tsDistances}}. To calculate distance matrices of time series databases using this measure see \code{\link{tsDatabaseDistances}}.
}

\examples{

#The objects example.series1 and example.series2 are two 
#numeric series of length 100 contained in the TSdist package. 

data(example.series1)
data(example.series2)

#For information on their generation and shape see help 
#page of example.series.

help(example.series)

#Compute the Euclidean distance between them:

euclideanDistance(example.series1, example.series2)
}