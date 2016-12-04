#_____________________________________________________________________________
#
# PROJECT: LARA
# CLASS: 
# FILENAME: dataAggregation.R
#
# CATEGORY:
#
# AUTHOR: mark doerr
# EMAIL: mark@ismeralda.org
#
# VERSION: 0.0.9
#
# CREATION_DATE: 2015/07/07
# LASTMODIFICATION_DATE: 2015/07/29
#
# BRIEF_DESCRIPTION: Library for data aggregation, like  
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

printDebug <- function(...) cat(sprintf(...), '\n' ,sep='', file=stdout())


#' averageByGroups1Crit
#'
#' @title average by a grouping variable and add standard deviation and relative standard deviation (RDS or CV)
#' @description This function is the main dispatcher function for reading several file formats into dataframes.
#' @param    layout_dir="./" (string) - plate layout file directory with leading / 
#' @param    use_db_layout=FALSE (boolean) - reading the layout from a database
#' @param    PLC=FALSE  (boolean) - automatically calculate pathlength correction 
#'             under the assumptions of a 96 round well SBS layout (r=034 cm)
#'             and 200ul liquid volume
#' @keywords plate readers
#' @note     todo - 
#' @export 

averageByGroups1Crit <- function( non_corr_df=NULL, groupingVar="Description", newDF=TRUE ) 
{
  printDebug("WARNING(avarageByGroups1Crit): output columns name changed to ValueMean, ValueSd and ValueRsd")
  printDebug("Grouping var:%s", groupingVar)
  
  if ( is.null(non_corr_df$Value)) {print("ERROR(avarageByGroups1Crit): No Value column in data frame "); return(FALSE) }
  
  if ( isTRUE(newDF) ) non_corr_df <- subset(non_corr_df, select=c("Value", groupingVar) )
  
  aggr_m_df <- aggregate( non_corr_df$Value, list(groupingVar=non_corr_df[,groupingVar]), mean )
  names(aggr_m_df) <- c(groupingVar, "ValueMean")
  aggr_s_df <- aggregate( non_corr_df$Value, list(groupingVar=non_corr_df[,groupingVar]), sd )
  names(aggr_s_df) <- c(groupingVar, "ValueSd")
  ag_full_df <- merge( aggr_m_df, aggr_s_df, by=c(groupingVar))
  out_df <- merge( non_corr_df, ag_full_df, by=c(groupingVar))
  
  out_df$ValueRsd <- out_df$ValueSd / out_df$ValueMean 
  
  out_df$Value = NULL
  out_df$RawValue = NULL
  if( groupingVar != 'Well') out_df$Well = NULL
  out_df$RowNum = NULL
  out_df$ColNum = NULL
  out_df <- unique(out_df)
  
  return(out_df[order(out_df[,groupingVar]),] )
}

#' averageByGroups2Crit
#'
#' @title converts time units
#' @description This function is the main dispatcher function for reading several file formats into dataframes.
#' @param    layout_dir="./" (string) - plate layout file directory with leading / 
#' @param    use_db_layout=FALSE (boolean) - reading the layout from a database
#' @param    PLC=FALSE  (boolean) - automatically calculate pathlength correction 
#'             under the assumptions of a 96 round well SBS layout (r=034 cm)
#'             and 200ul liquid volume
#' @keywords plate readers
#' @note     todo - 
#' @export 
  
averageByGroups2Crit <- function( non_corr_df=NULL, groupingVar1="Description", groupingVar2="DiffTime" ) 
{
  printDebug("WARNING(avarageByGroups2Crit): mind slightly difference in DiffTime from reader files !!")
  printDebug("Grouping var1:%s var2:%s", groupingVar1, groupingVar2)
  
  tail(head(non_corr_df))
  
  aggr_m_df <- aggregate( non_corr_df$Value, list(groupingVar1=non_corr_df[,groupingVar1], groupingVar2=non_corr_df[,groupingVar2]), mean )
  names(aggr_m_df) <- c(groupingVar1, groupingVar2, "ValueMean")
  aggr_s_df <- aggregate( non_corr_df$Value, list(groupingVar1=non_corr_df[,groupingVar1], groupingVar2=non_corr_df[,groupingVar2]), sd )
  names(aggr_s_df) <- c(groupingVar1, groupingVar2, "ValueSd")
  ag_full_df <- merge( aggr_m_df, aggr_s_df, by=c(groupingVar1, groupingVar2))
  out_df <- merge( non_corr_df, ag_full_df, by=c(groupingVar1, groupingVar2))

  out_df$ValueRsd <- out_df$ValueSd / out_df$ValueMean
  
  out_df$Value = NULL
  out_df$RawValue = NULL
  if( groupingVar1 != 'Well') out_df$Well = NULL
  out_df$RowNum = NULL
  out_df$ColNum = NULL
  out_df <- unique(out_df)
  
  return(out_df[order(out_df[,groupingVar1], out_df[,groupingVar2]),] )
}


#' substractBlank
#'
#' @title converts time units
#' @description This function is the main dispatcher function for reading several file formats into dataframes.
#' @param    layout_dir="./" (string) - plate layout file directory with leading / 
#' @param    use_db_layout=FALSE (boolean) - reading the layout from a database
#' @param    PLC=FALSE  (boolean) - automatically calculate pathlength correction 
#'             under the assumptions of a 96 round well SBS layout (r=034 cm)
#'             and 200ul liquid volume
#' @keywords plate readers
#' @note     todo - 
#' @export 

substractBlankPointwise <- function( non_corr_df=NULL, groupingVar=c("Substr1Conc", "Enzyme"), 
                            ioColumns=c("ValueMean", "ValueMeanCorr"), removeBlank=FALSE ) 
{
  printDebug("WARNING(substractBlank): check new grouping and check handling of standard deviation !")
  non_corr_df$ValueMeanCorr <- non_corr_df$ValueMean
    
  out_corr_df = NULL
  iterSubstrate <- function( group_df )
  {
    blank_df = group_df[group_df$Type=="BL", ]
    
    correctBaseline <- function(by_enzyme_df){
      by_enzyme_df$ValueMeanCorr <- by_enzyme_df$ValueMeanCorr - blank_df$ValueMean
      out_corr_df <<- rbind(out_corr_df, by_enzyme_df)
    }
    by(group_df, group_df[,groupingVar[2]], correctBaseline )
  }
  invisible(by(non_corr_df, non_corr_df[,groupingVar[1]], iterSubstrate ))
  
  if (isTRUE(removeBlank)) return(out_corr_df[( out_corr_df$Enzyme != "blank"),]) else
    return(out_corr_df)
}


#' substractBlank
#'
#' @title converts time units
#' @description This function is the main dispatcher function for reading several file formats into dataframes.
#' @param    layout_dir="./" (string) - plate layout file directory with leading / 
#' @param    use_db_layout=FALSE (boolean) - reading the layout from a database
#' @param    PLC=FALSE  (boolean) - automatically calculate pathlength correction 
#'             under the assumptions of a 96 round well SBS layout (r=034 cm)
#'             and 200ul liquid volume
#' @keywords plate readers
#' @note     todo - 
#' @export 

substractBlank <- function( non_corr_df=NULL, groupingVar="Substrate1Conc" ) 
{
  printDebug("WARNING(substractBlank): check handling of standard deviation !")
  non_corr_df$ValueMeanCorr <- non_corr_df$ValueMean
  
  #out_corr_df <-  non_corr_df
  #out_corr_df$ValueMeanCorr <- out_corr_df$ValueMean 
  #head(out_corr_df)
  #tail(out_corr_df,120)
  
  out_corr_df = NULL
  iterSubstrate <- function( group_df )
  {
    print(head(group_df))
    blank_df = group_df[group_df$Type=="BL", ]
    
    print("----------- blank-----")
    print(blank_df)
    
    
    correctBaseline <- function(by_enzyme_df){
      by_enzyme_df$ValueMeanCorr <- by_enzyme_df$ValueMeanCorr - blank_df$ValueMean
      out_corr_df <<- rbind(out_corr_df, by_enzyme_df)
    }
    by(group_df, group_df$Enzyme, correctBaseline )
  }
  invisible(by(non_corr_df, non_corr_df[,groupingVar], iterSubstrate ))
  
  #out_corr_df[order(out_corr_df[,groupingVar], out_corr_df$DiffTime),]
  
  return(out_corr_df[( out_corr_df$Type != "BL"),])
}

#' addInfo
#'
#' @title adding additional info to data frame from info_df
#' @description This function is the main dispatcher function for reading several file formats into dataframes.
#' @param    layout_dir="./" (string) - plate layout file directory with leading / 
#' @param    use_db_layout=FALSE (boolean) - reading the layout from a database
#' @param    PLC=FALSE  (boolean) - automatically calculate pathlength correction 
#'             under the assumptions of a 96 round well SBS layout (r=034 cm)
#'             and 200ul liquid volume
#' @keywords plate readers
#' @note     todo - 
#' @export 

addInfo <- function( data_df=NULL, info_df=NULL) 
{
  data_df$Well = NULL
  data_df$Type = NULL
  data_df$Barcode = NULL
  
  # removing non-unique columns
  info_df$Slope = NULL
  info_df$Intercept = NULL
  info_df$BestLinLeft = NULL
  info_df$BestLinRight = NULL
  info_df$DateTime = NULL
  info_df$DiffTime = NULL
  info_df$Duration = NULL
  info_df$ValueMean = NULL
  info_df$ValueMeanCorr = NULL
  info_df$ValueSd = NULL
  info_df$ValueRsd = NULL
  info_df$Temperature = NULL
  
  info_df <- unique(info_df)
  
  return( merge(data_df, info_df))
}

