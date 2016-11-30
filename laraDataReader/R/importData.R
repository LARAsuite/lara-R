#_____________________________________________________________________________
#
# PROJECT: LARA
# CLASS: 
# FILENAME: importData.R
#
# CATEGORY:
#
# AUTHOR: mark doerr
# EMAIL: mark at ismeralda.org
#
# VERSION: 0.1.0
#
# CREATION_DATE: 2015/06/01
# LASTMODIFICATION_DATE: 161125
#
# BRIEF_DESCRIPTION: Library for reading
# DETAILED_DESCRIPTION: 
# HISTORY: 
#
# ____________________________________________________________________________
#
#   Copyright:
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#   This file is provided "AS IS" with NO WARRANTY OF ANY KIND,
#   INCLUDING THE WARRANTIES OF DESIGN, MERCHANTABILITY AND FITNESS FOR
#   A PARTICULAR PURPOSE.
#
#   For further Information see COPYING file that comes with this distribution.
#_______________________________________________________________________________

#' printDebug
#' 
#' @title printDebug 
#' @description printing debugging information - if debug mode is activated
#' @export 

printDebug <- function(module="",...)
{
  if( exists("debugging") ){ 
    if( isTRUE(debugging) )  cat("DEBUG:(",module,"): ", sprintf(...), '\n' ,sep='', file=stdout())
  } 
}

#' printWarning
#' 
#' @title printWarning
#' @description printing warning information - if debug mode is activated
#' @export 

printWarning <- function(module="", ...)
{
  if( exists("debugging") ){ 
    if( isTRUE(debugging) )  cat("WARNING:(",module,"): ", sprintf(...), '\n' ,sep='', file=stdout())
  } 
}

#' printError
#' 
#' @title printError 
#' @description printing errors and warnings to stderr - if debug mode is activated
#' @export 

printError <- function(module="", ...)
{
    cat("ERROR(",module, "): ", sprintf(...), '\n' ,sep='', file=stderr())
}

#' trim.leading
#' 
#' @title trim.leading 
#' @description removing leading whitespace from a string
#' @author f3lix at stackoverflow (big thanx !)
#' @return returns string w/o leading whitespace
#' @export 

trim.leading <- function(x)  sub("^\\s+", "", x)

#' trim.trailing
#' 
#' @title trim.trailing
#' @description removing trailing whitespace from a string
#' @author f3lix at stackoverflow (big thanx !)
#' @return returns string w/o trailing whitespace
#' @export 

trim.trailing <- function(x) sub("\\s+$", "", x)

#' trim.both
#' 
#' @title trim.both
#' @description removing whitespace from start and end of a string
#' @author f3lix at stackoverflow (big thanx !)
#' @return returns string w/o leading or trailing whitespace
#' @export 

trim.both <- function(x) gsub("^\\s+|\\s+$", "", x)

#' LA_ImportData
#'
#' @title Importing Data from Microtiter Plate Readers - generic method 
#' @description This function is the main dispatcher function for reading several file formats into dataframes.
#'              Currently supported devices:
#'                BMG Readers (e.g. Omega ) - device='omega'
#'                Thermo Scientific (e.g. Varioskan) - device='varioskan'
#'                TECAN (e.g. ) - device='tecan'
#'              Currently supported formats (method):
#'                SPabs : Single point absorption measurement (Omega, Varioskan),
#'                SPabsMatr: Single point absorption measurement in Matrix Layout format (Omega),
#'                SPfl:   Single point fluorescense measurement (Varioskan),
#'                KINabs: Kinetic absorption measurement (Varioskan) 
#' @param    filename/pattern (string)
#' @param    path (string)
#' @param    method (string) - one of the above described methods 
#' @param    device (string) - one of the above mentioned devices
#' @param    layoutGeometry - container layout geometry
#' @param    layout=TRUE (boolean) - automatically read plate layout file
#' @param    layoutDir="./" (string) - plate layout file directory with leading / 
#' @param    useDBlayout=FALSE (boolean) - reading the layout from a database
#' @param    concUnit="uM" - conentration unit of layout file
#' @param    PLC=FALSE  (boolean) - automatically calculate pathlength correction 
#'             under the assumptions of a 96 round well SBS layout (r=034 cm)
#'             and 200ul liquid volume
#' @param    wellRadius=0.34 (double) well radius for pathlength calculation in cm
#' @param    liquidVolume=0.2 (double) liquid volume for pathlength calculation in uL
#' @return   data frame with reader data
#' @keywords plate readers
#' @examples
#'           kin_df <- importReaderData("_KINabs_245_265_", method="KINabs", device='varioskan', layout=FALSE, PLC=TRUE)
#' 
#' @note     todo: debugging,  error handling, filename patterns 
#' @export 

LA_ImportData <- function ( filename="", data_path="./",
                           method='SPabs', defaultClass="SP",
                           barcode='000000', wavelength=0, wlStart=0, wlEnd=0, wlStepSize=1,
                           timeUnit="secs", numSamples = 96, sep=',', layoutGeometry=c(8,12),
                           layout=FALSE, useDBlayout=FALSE, layoutDir="./", concUnit="uM", 
                           PLC=FALSE, wellRadius=0.34, liquidVolume=0.2, ...)
{  
  current_data_path <- data_path
  
  current_filename <- filename
  current_method <- method
  default_class <- defaultClass
  current_barcode <- barcode
  current_wavelength <- wavelength
  wavelength_start <- wlStart
  wavelength_end <- wlEnd
  wavelength_step_size <- wlStepSize 
  current_time_unit <- timeUnit
  current_sample_num <- numSamples
  current_sep <- sep
  load_layout <- layout 
  current_layout_dir <- layoutDir
  current_layout_geometry <- layoutGeometry
  load_layout_from_db <- useDBlayout
  current_conc_unit <- concUnit
  pathlength_correction <- PLC
  current_well_radius <- wellRadius 
  current_liquid_volume <- liquidVolume 
  
  if(current_filename == "" ) {printError("ERROR (LA_ReaderData): no filename specified !"); return()}
  help_info <- "please type: ?importReaderData"
  
  filename_list <- list.files( pattern = current_filename, all.files = FALSE,
                               full.names = FALSE, recursive = FALSE,
                               ignore.case = FALSE, include.dirs = FALSE)
  
  if (length(filename_list) == 0L) {printError("ERROR (LA_ReaderData): No files found or wrong filname pattern (%s) specified !", current_filename); return(FALSE)}
  
  UseMethod("LA_ImportData")
}

#' LA_ImportData.default
#'
#' @title Importing Data from Microtiter Plate Readers - default method 
#' @description This function is the default function 
#' @export 

LA_ImportData.default <- function (filename, ...) {
  
  printDebug("ERROR(LA_ImportData): Unknown import class specified for filename. Please specify import reading class by class(%s) <- \"%s\", e.g. ", current_filename, current_method)
  #invisible(x)
}

#' pathlengthCorrectionFactor
#'
#' @title Pathlenght correction factor for microtiter plates
#' @description A simple pathlenght correction factor is generated, based on volume and radius of a cylinder.
#'              More accurate pathlength correction is achieved by measureing the absorption of a solution 
#'              with know concentration with different per well
#'              volumes and recording of a calibration curve.
#' @param liquidVolume=200 - in microliter (ul)
#' @param wellRadius=0.34 (standard radius of a well in a 96 well MTP)
#' @param miniscusRactor=0.95 (estimated reduction of height by well miniscus) 
#' @keywords plate readers, pathlength correction
#' @export 
#' @examples
#'     pl_factor <- pathlengthCorrectionFactor(liquidVolume=180, wellRadius=0.95)
#' 

pathlengthCorrectionFactor<-function(liquidVolume=200, wellRadius=0.34, miniscusFactor = 0.95 )
{
  # print(WARNING(pathlengthCorrectionFactor): This is a very simple pathlength correction based on the approximation of the well by a cylinder)
  # the miniscus factor highly depends on the liquid's surface tension, 5 % deviation is just a rough estimation
  optical_pathlength = liquidVolume * miniscusFactor / (1000 * pi * wellRadius^2) 
  pathlength_correction_factor = 1 / optical_pathlength  #signif((1/optical_pathlength),digits=3)
  
  return(pathlength_correction_factor)
}

#' genWellNumbers
#'
#' @title Well Number Generator
#' @description Generator for microtiter plate well coordinates with repeats
#' @param padding=2 (overall number of digits, leading digits are filled with zeros)  
#' @param nrows=8 (number of rows on plate)
#' @param ncols=12 (number of columns on plate)
#' @keywords plate readers, microtiter plates
#' @export 
#' @examples  genWellNumbers(padding=1)
#'              >  A1, A2, ...., H12
#'            genWellNumbers(padding=3)
#'              > A001, A002, ..., H012

genWellNumbers <- function(padding=2, nrows=8, ncols=12, repeats=1, byRows=FALSE, asVector=FALSE)
{
  if(byRows) {
    well_numbers <- rep(sapply(LETTERS[1:nrows], 
                           function(x) sprintf(paste("%s%0",padding,"d", sep=""), x, c(1:ncols)) ), 1, each=repeats)
  } else {
           well_numbers <- rep(sapply( c(1:ncols), function(x) sprintf(paste("%s%0",padding,"d", sep=""),  LETTERS[1:nrows], x) ), 1, each=repeats)
    }
  if(asVector) return(well_numbers) else
  return(as.factor(well_numbers)) 
}
