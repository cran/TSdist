\name{frechetDistance}
\alias{frechetDistance}

\title{
Frechet distance
}

\description{
Computes the Frechet distance between two numerical trajectories. 
}

\usage{
frechetDistance(x, y, tx, ty, ...)
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
  \item{...}{
   Additional parameters for the function. See  \code{\link[longitudinalData]{distFrechet}} for more 
  information.
}
}

\details{
This is essentially a wrapper for the \code{\link[longitudinalData]{distFrechet}} function of package \pkg{longitudinalData}. As such, all the functionalities of the \code{\link[longitudinalData]{distFrechet}} function are also available when using this function. 
}

\value{
\item{d}{
  The computed distance between the pair of series.
}
}

\references{

Christophe Genolini (2014). longitudinalData: Longitudinal Data. R package version
2.2. http://CRAN.R-project.org/package=longitudinalData

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

#The objects example.serie3 and example.series4 are two 
#numeric series of length 100 and 120, respectively. 

data(example.series3)
data(example.series4)

#For information on their generation and shape see 
#help page of example.series.

help(example.series)

#Calculate the distance based on wavelet feature extraction between the series.

\dontrun{frechetDistance(example.series3, example.series4)}



}
