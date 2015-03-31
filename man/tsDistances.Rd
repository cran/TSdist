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
 Distance measure to be used. It must be one of: \code{"euclidean"}, \code{"manhattan"}, \code{"minkowski"}, \code{"infnorm"}, \code{"ccor"}, \code{"sts"}, \code{"dtw"}, \code{"keogh_lb"}, \code{"edr"}, \code{"erp"}, \code{"lcss"}, \code{"fourier"}, \code{"tquest"}, \code{"dissimfull"}, \code{"dissimapprox"}, \code{"acf"}, \code{"pacf"}, \code{"ar.lpc.ceps"}, \code{"ar.mah"}, \code{"ar.mah.statistic"}, \code{"ar.mah.pvalue"}, \code{"ar.pic"}, \code{"cdm"}, \code{"cid"}, \code{"cor"}, \code{"cort"}, \code{"wav"}, \code{"int.per"}, \code{"per"}, \code{"mindist.sax"}, \code{"ncd"}, \code{"pred"}, \code{"spec.glk"}, \code{"spec.isd"},
                        \code{"spec.llr"}, \code{"pdc"}, \code{"frechet"})
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
\item "infnorm": Infinite norm distance. \code{\link{inf.normDistance}}
\item "ccor": Distance based on the cross-correlation. \code{\link{ccorDistance}}
\item "sts": Short time series distance. \code{\link{stsDistance}}
\item "dtw": Dynamic Time Warping distance. \code{\link{dtwDistance}}. Uses the \pkg{dtw} package (see \code{\link[dtw]{dtw}}).
\item "lb_keogh": LB_Keogh lower bound for the Dynamic Time Warping distance. \code{\link{lb.keoghDistance}}
\item "edr": Edit distance for real sequences. \code{\link{edrDistance}}
\item "erp": Edit distance with real penalty. \code{\link{erpDistance}}
\item "lcss": Longest Common Subsequence Matching. \code{\link{lcssDistance}}
\item "fourier": Distance based on the Fourier Discrete Transform. \code{\link{fourierDistance}}
\item "tquest": TQuest distance. \code{\link{tquestDistance}}
\item "dissimfull": Dissim distance. \code{\link{dissimDistance}}
\item "dissimapprox": Approximated Dissim distance. \code{\link{dissimapproxDistance}}
\item "acf": Autocorrelation-based dissimilarity \code{\link{acfDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.ACF}}).
\item "pacf": Partial autocorrelation-based dissimilarity \code{\link{pacfDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.PACF}}).
\item "ar.lpc.ceps": Dissimilarity based on LPC cepstral coefficients \code{\link{ar.lpc.cepsDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.AR.LPC.CEPS}}).
\item "ar.mah": Model-based dissimilarity proposed by Maharaj (1996, 2000) \code{\link{ar.mahDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.AR.MAH}}).
\item "ar.pic": Model-based dissimilarity measure proposed by Piccolo (1990) \code{\link{ar.picDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.AR.PIC}}).
\item "cdm": Compression-based dissimilarity measure \code{\link{cdmDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.CDM}}).
\item "cid": Complexity-invariant distance measure \code{\link{cidDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.CID}}).
\item "cor": Dissimilarities based on Pearson's correlation \code{\link{corDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.COR}}).
\item "cort": Dissimilarity index which combines temporal correlation and raw value
behaviors \code{\link{cortDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.CORT}}).
\item "wav": Dissimilarity based on wavelet feature extraction \code{\link{wavDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.DWT}}).
\item "int.per": Integrated periodogram based dissimilarity \code{\link{int.perDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.INT.PER}}).
\item "per": Periodogram based dissimilarity \code{\link{perDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.PER}}).
\item "mindist.sax": Symbolic Aggregate Aproximation based dissimilarity  \code{\link{mindist.saxDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.MINDIST.SAX}}).
\item "ncd": Normalized compression based distance \code{\link{ncdDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.NCD}}).
\item "pred": Dissimilarity measure cased on nonparametric forecasts \code{\link{predDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.PRED}}).
\item "spec.glk": Dissimilarity based on the generalized likelihood ratio test \code{\link{spec.glkDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.SPEC.GLK}}).
\item "spec.isd": Dissimilarity based on the integrated squared difference between the log-spectra \code{\link{spec.isdDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.SPEC.ISD}}).
\item "spec.llr": General spectral dissimilarity measure using local-linear estimation of the log-spectra \code{\link{spec.llrDistance}}. Uses the \pkg{TSclust} package (see \code{\link[TSclust]{diss.SPEC.LLR}}).
\item "pdc": Permutation Distribution Distance \code{\link{pdcDistance}}. Uses the \pkg{pdc} package (see \code{\link[pdc]{pdcDist}}).
\item "frechet": Frechet distance \code{\link{frechetDistance}}. Uses the \pkg{longitudinalData} package (see \code{\link[longitudinalData]{distFrechet}}).
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

tsDistances(zoo.series1, zoo.series2, distance="infnorm")
tsDistances(zoo.series1, zoo.series2, distance="cor", beta=3)
tsDistances(zoo.series1, zoo.series2, distance="dtw", sigma=20)



}

