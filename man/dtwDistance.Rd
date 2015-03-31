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
dtwDistance(x, y, ...)
}

\arguments{
  \item{x}{
  Numeric vector containing the first time series.
}

  \item{y}{
  Numeric vector containing the second time series.
}

  \item{...}{
  Additional parameters for the function. See  \code{\link[dtw]{dtw}} for more 
  information.
}
}

\details{
This is simply a wrapper for the \code{\link[dtw]{dtw}} function of package \pkg{dtw}. As such, all the functionalities of the \code{\link[dtw]{dtw}} function are also available when using this function. 

}

\value{
\item{d}{
  The computed distance between the pair of series.
}


}

\references{
Giorgino T (2009). \emph{Computing and Visualizing Dynamic Time Warping Alignments in R:
The dtw Package.} Journal of Statistical Software, 31(7), pp. 1-24. URL:http://www.jstatsoft.org/v31/i07/
}

\author{
Usue Mori, Alexander Mendiburu, Jose A. Lozano.
}



\seealso{
To calculate a lower bound of the DTW distance see \code{\link{lb.keoghDistance}}.

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

#Calculate the basic DTW distance for two series of different length.

dtwDistance(example.series3, example.series4)

#Calculate the DTW distance for two series of different length
#with a sakoechiba window of size 30:

dtwDistance(example.series3, example.series4, window.type="sakoechiba", window.size=30)

#Calculate the DTW distance for two series of different length
#with an assymetric step pattern

dtwDistance(example.series3, example.series4, step.pattern=asymmetric)

}
