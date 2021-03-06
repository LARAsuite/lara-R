% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/importData.R
\name{pathlengthCorrectionFactor}
\alias{pathlengthCorrectionFactor}
\title{Pathlenght correction factor for microtiter plates}
\usage{
pathlengthCorrectionFactor(liquidVolume = 200, wellRadius = 0.34,
  miniscusFactor = 0.95)
}
\arguments{
\item{liquidVolume=200}{- in microliter (ul)}

\item{wellRadius=0.34}{(standard radius of a well in a 96 well MTP)}

\item{miniscusRactor=0.95}{(estimated reduction of height by well miniscus)}
}
\description{
A simple pathlenght correction factor is generated, based on volume and radius of a cylinder.
             More accurate pathlength correction is achieved by measureing the absorption of a solution 
             with know concentration with different per well
             volumes and recording of a calibration curve.
}
\details{
pathlengthCorrectionFactor
}
\examples{
    pl_factor <- pathlengthCorrectionFactor(liquidVolume=180, wellRadius=0.95)

}
\keyword{correction}
\keyword{pathlength}
\keyword{plate}
\keyword{readers,}
