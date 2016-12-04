#_____________________________________________________________________________
#
# PROJECT: LARA
# CLASS: 
# FILENAME: csvReader.R
#
# CATEGORY:
#
# AUTHOR: mark doerr
# EMAIL: mark@ismeralda.org
#
# VERSION: 0.1.0
#
# CREATION_DATE: 2015/10/15
# LASTMODIFICATION_DATE: 161124
#
# BRIEF_DESCRIPTION: Library for reading generic CSV data
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


#' LA_ImportData.CSV
#' 
#' @title R script for reaing CSV data 
#' @description This function requires a table/list 
#'              'DiffTime', 'Value', 'Description', Wavelength'      
#' @param dec , decimal separator
#' @return data.frame
#' @keywords plate readers, CSV
#' @export 
#' 
#' @note todo - 

LA_ImportData.CSV <- function(filename, dec=".", sep=",", ...)
{
  printDebug("simple CSVReader v0.0.1")
  
  readAllData <- function(filename)
  {
    raw_tab_df <- read.table(filename,blank.lines.skip = TRUE, header=TRUE, stringsAsFactors=TRUE, dec=dec, sep=sep )
    
    raw_tab_df$NumReads <- as.factor(file_num)
    
    file_num <<- file_num + 1
    return(raw_tab_df)
  }
  file_num <- 1  # needs to be adjusted, if kinetics come from one single file
  output_df <- do.call(rbind, lapply(filename_list, readAllData ))
  
  if (is.null(output_df$Well) ) output_df$Well <- "A01"
  if (is.null(output_df$Type) ) output_df$Type <- "S"
  
  return(output_df)
}