\name{tsDistances}
\alias{tsDistances}

\title{
TSdist distance computation.
}
\description{
TSdist distance calculation between two time series.

}
\usage{
tsDistances(x, y, tx, ty, distance, ...)
}
\arguments{
  \item{x}{
  Numeric vector or \code{ts}, \code{zoo} or \code{xts} object containing the first time series.
}
  \item{y}{
  Numeric vector or \code{ts}, \code{zoo} or \code{xts} object containing the second time series.
}
  \item{tx}{
  Optional temporal index of series \code{x}. Only necessary if \code{x} is a numeric vector and the sampling index is not constant.
}
  \item{ty}{
   Optional temporal index of series \code{y}. Only necessary if \code{y} is a numeric vector and the sampling index is not constant.
}
\item{distance}{
   Distance measure to be used. It must be one of: \code{"euclidean"}, \code{"manhattan"}, \code{"minkowski"}, \code{"infinitenorm"}, \code{"pearsoncorrelation"}, \code{"crosscorrelation"}, \code{"sts"}, \code{"dtw"}, \code{"keogh_lb"}, \code{"edr"}, \code{"erp"}, \code{"lcss"}, \code{"fourier"}, \code{"tquest"}, \code{"dissim"} or \code{"dissimapprox"}
}
\item{...}{
  Additional parameters required by the distance method.
}
}
\details{

The distance between the two time series \code{x} and \code{y} is calculated. \code{x} and \code{y} can be saved in a numeric vector or a \code{ts}, \code{zoo} or \code{xts} object. The following distance methods are supported: 

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
\item{d}{
  The computed distance between the pair of time series.
}

}


\author{
Usue Mori, Alexander Mendiburu, Jose A. Lozano.
}

\examples{

#The objects zoo.series1 and zoo.series2 are two 
#zoo objects that save two series of length 100. 

data(zoo.series1)
data(zoo.series2)

#For information on their generation and shape see 
#help page of example.series.

help(example.series)

#The distance calculation for these two series is done
#as follows:

tsDistances(zoo.series1, zoo.series2, distance="infinitenorm")
tsDistances(zoo.series1, zoo.series2, distance="pearsoncorrelation", beta=3)
tsDistances(zoo.series1, zoo.series2, distance="dtw", sigma=20)



}

