\name{tsDatabaseDistances}
\alias{tsDatabaseDistances}

\title{
TSdist distance matrix computation.
}
\description{
TSdist distance matrix computation for time series databases.
}
\usage{
tsDatabaseDistances(x, method, diag=FALSE, upper=FALSE, ...)
}
\arguments{
  \item{x}{
  Time series database saved in a numeric matrix, a list, an \code{mts} object, a \code{zoo} object  or \code{xts} object.
}
  
  \item{method}{
 Distance measure to be used. It must be one of: \code{"euclidean"}, \code{"manhattan"}, \code{"minkowski"}, \code{"infinitenorm"}, \code{"pearsoncorrelation"}, \code{"crosscorrelation"}, \code{"sts"}, \code{"dtw"}, \code{"keogh_lb"}, \code{"edr"}, \code{"erp"}, \code{"lcss"}, \code{"fourier"}, \code{"tquest"}, \code{"dissim"} or \code{"dissimapprox"}
}
  \item{diag}{
   logical value indicating if the diagonal of the distance matrix is to be printed.
}
\item{upper}{
   logical value indicating if the upper triangle of the distance matrix is to be printed.
}
\item{...}{
  Additional parameters required by the chosen distance measure.
}
}
\details{

The distance matrix of a time series database is calculated by providing the pair-wise distances between the series that conform it. \code{x} can be saved in a numeric matrix, a list or a \code{mts}, \code{zoo} or \code{xts} object. The following distance methods are supported: 

\itemize{
\item "euclidean": Euclidean distance. \code{\link{euclideanDistance}}
\item "manhattan": Manhattan distance. \code{\link{manhattanDistance}}
\item "minkowski": Minkowski distance. \code{\link{minkowskiDistance}}
\item "infinitenorm": Infinite norm distance. \code{\link{infiniteNormDistance}}
\item "pearsoncorrelation": Distances based on Pearson's correlation. \code{\link{correlationDistance}}
\item "crosscorrelation": Distances based on the cross-correlation. \code{\link{crossCorrelationDistance}}
\item "sts": Short time series distance. \code{\link{stsDistance}}
\item "dtw": Dynamic Time Warping distance. \code{\link{dtwDistance}}
\item "keogh_lb": LB_Keogh lower bound for the Dynamic Time Warping distance. \code{\link{lbKeoghDistance}}
\item "edr": Edit distance for real sequences. \code{\link{edrDistance}}
\item "erp": Edit distance with real penalty. \code{\link{erpDistance}}
\item "lcss": Longest Common Subsequence Matching. \code{\link{lcssDistance}}
\item "fourier": Distance based on the Fourier Discrete Transform. \code{\link{fourierDistance}}
\item "tquest": TQuest distance. \code{\link{tquestDistance}}
\item "dissim": Dissim distance. \code{\link{dissimDistance}}
\item "dissimapprox": Approximated Dissim distance. \code{\link{dissimapproxDistance}}
}

Some distance measures may require additional arguments. See the individual help pages (detailed above) for more information about each method.


}

\value{
\item{D}{
  The computed distance matrix of the time series database.
}

}


\author{
Usue Mori, Alexander Mendiburu, Jose A. Lozano.
}

\examples{

#The object example.database is a numeric matrix that saves
#6 ARIMA time series in a row-wise format. For more information
#see help page of example.databases:

help(example.database)
data(example.database)

#To calculate the distance matrix of this database: 

tsDatabaseDistances(example.database, method="manhattan")
tsDatabaseDistances(example.database, method="edr", epsilon=0.2)
tsDatabaseDistances(example.database, method="fourier", n=20)

#The object zoo.database is a zoo object that saves
#the same 6 ARIMA time series saved in example.database. 

data(zoo.database)

#To calculate the distance matrix of this database: 

tsDatabaseDistances(zoo.database, method="manhattan")
tsDatabaseDistances(zoo.database, method="edr", epsilon=0.2)
tsDatabaseDistances(zoo.database, method="fourier", n=20)

}
