% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/layoutReader.R
\name{addPlateLayout}
\alias{addPlateLayout}
\title{Adding Plate Layout Information to reader data frame}
\usage{
addPlateLayout(reader_df = NULL, barcode = "000000", useDBlayout = FALSE,
  padding = 4, concUnit = "uM", setValueNA = FALSE, setSlopeNA = FALSE)
}
\arguments{
\item{useDBlayout}{- retrieve layout from django database}

\item{padding}{- used for adding leading zeros to barcode - will later be changed to 6}

\item{reader_df=NULL}{(data.frame) - plate reader data frame from import reader file}

\item{barcode="000000"}{(string) - this is used for finding the layout file BC_plate_layout_*.csv
barcodes can have a non-numeric prefix, valid barcodes are e.g. TA1001, GA_20, Tbe.0003, XY_abc|0404, WZ.def.4432}

\item{dir="./"}{(string) - directory of layout file with leading '/'}

\item{concUnit="uM"}{- unit of concentration, valid values are 'uM' and 'mM'}

\item{setValueNA=TRUE}{(boolean) - set values to NA for empty plates of type '0'}

\item{setSlopeNA=FALSE}{(boolean) - set slope and intercept values to NA for empty plates of type '0'}
}
\description{
Convenient function to load plate layout from a layout file or database 
             and merging it with the reader data frame.
}
\details{
addPlateLayout
}
\note{
todo : check, if merging bug with multiple measurements per data frame (e.g. groth data) still exists
}
\keyword{layout}
\keyword{plate}
\keyword{readers,}
