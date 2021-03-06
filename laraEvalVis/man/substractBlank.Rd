% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataAggregation.R
\name{substractBlank}
\alias{substractBlank}
\title{converts time units}
\usage{
substractBlank(non_corr_df = NULL, groupingVar = "Substrate1Conc")
}
\arguments{
\item{layout_dir="./"}{(string) - plate layout file directory with leading /}

\item{use_db_layout=FALSE}{(boolean) - reading the layout from a database}

\item{PLC=FALSE}{(boolean) - automatically calculate pathlength correction 
under the assumptions of a 96 round well SBS layout (r=034 cm)
and 200ul liquid volume}
}
\description{
This function is the main dispatcher function for reading several file formats into dataframes.
}
\details{
substractBlank
}
\note{
todo -
}
\keyword{plate}
\keyword{readers}
