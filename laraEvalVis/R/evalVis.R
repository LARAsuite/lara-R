#_____________________________________________________________________________
#
# PROJECT: LARA
# CLASS: 
# FILENAME: evalVis.R
#
# CATEGORY:
#
# AUTHOR: mark doerr
# EMAIL: mark@ismeralda.org
#
# VERSION: 0.0.9
#
# CREATION_DATE: 2015/07/29
# LASTMODIFICATION_DATE: 2015/07/29
#
# BRIEF_DESCRIPTION: Library/R package for evaluation and visualisation - functions used for all files
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
#' @description printing debugging information
#' @export 

printDebug <- function(...)
{
  if( exists("debugging") ){ 
    if( isTRUE(debugging) )  cat(sprintf(...), '\n' ,sep='', file=stdout())
  } 
}

debugging = TRUE

#' printError
#' 
#' @title printError 
#' @description printing errors and warnings to stderr
#' @export 

printError <- function(...) cat(sprintf(...), '\n' ,sep='', file=stderr())

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

#' as.numeric.factor
#' 
#' @title as.numeric.factor
#' @description converting factor to factor
#' @return numeric vector
#' @export 

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

#' readerDF2Array
#'
#' @title Converts a data frame to an easy plottable 3D array
#' @description This convenient function can be used to transform a plate reader dataframe into an 3D array
#'              in the format 12 x 8 x num.
#'              It is used to quickly iterate through kinetic data for each well. 
#' @param reader_data_df (data.frame) - source data frame
#' @param num=0 (integer) - number of measurements
#' @param wavelength=0
#' @param add_zero=TRUE
#' @param use_slope=FALSE
#' @return array (12 x 8 x num)
#' @keywords plate readers
#' @export 
#' @note todo - arbitrary number of cols and rows
#' 

readerDF2Array <- function(source_data_df=NULL, num=0, nrows=8, ncols=12, wavelength=0, 
                           groupingVar="Well", plotVar=c("Value", "RefTime"),  add_zero=TRUE)
{
  printDebug("INFO(readerDF2Array): v.0.0.9 l4, plotVar 1:%s 2:%s", plotVar[1], plotVar[2])
  printDebug("WARNING(readerDF2Array): please check, when wavelength is specified, if data contains a wavelength column")
  
 #source_data_df <- source_data_df[order(source_data_df[,groupingVar], source_data_df[,plotVar[2]]),]
 
 groupingVar="RefTime"
 source_data_df <- source_data_df[order(source_data_df[,groupingVar]),]
 
 #print(head(source_data_df,120))
  
  #if(plotVar[1] == 'Slope' & (num == 0) ) num <-length(value_vec)  else {
    if( wavelength > 0) value_vec <- source_data_df[source_data_df$Wavelength == wavelength,][,plotVar[1]] else {
      value_vec <- source_data_df[,plotVar[1]]
    }
  #}
  
  if (num == 0) {
    printDebug("INFO(readerDF2Array):auto determine size. vvn: %s", length(value_vec))
    num = length(value_vec) / (nrows*ncols) # nrows*ncols # length(value_vec) #/ (nrows*ncols)
  }
  
  #printDebug("INFO(readerDF2Array): nrows: %s, ncols:%s, num:%s", nrows, ncols, num)

  if (isTRUE(add_zero)) {
    #value_vec <- append(value_vec, rep(0, num), after=0)
    value_vec <- append(value_vec, rep(0, (nrows*ncols)), after=0)
    num <- num + 1
    printDebug("adding zero: num:%s", num)
  }
      
  #return(array(value_vec, dim=c(num, ncols, nrows)))
  #return(array(value_vec, dim=c(num, nrows, ncols)))
 
  # perm transformation for overcoming problem with filling by columns of array
  return(aperm(array(value_vec, dim=c( ncols, nrows, num)), perm=c(2,1,3)))
}

