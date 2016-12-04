#_____________________________________________________________________________
#
# PROJECT: LARA
# CLASS: 
# FILENAME: HTSanalysis.R
#
# CATEGORY:
#
# AUTHOR: mark doerr
# EMAIL: mark@ismeralda.org
#
# VERSION: 0.0.3
#
# CREATION_DATE: 2015/08/01
# LASTMODIFICATION_DATE: 2015/08/27
#
# BRIEF_DESCRIPTION: Library for high-throughput screening (HTS) analysis
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

#' ZfactorAnalysis
#'
#' @title Z-factorAnalysis
#' @description This function determines the Z-factor.
#' @param    a
#' @keywords plate readers
#' @note     todo - 
#' @export 
  
ZfactorAnalysis <- function(assay_df=NULL, sampleTypes=c("S", "nCTRL")) 
{
  av_df = averageByGroups1Crit(non_corr_df=assay_df, groupingVar="Type" )
  
  Z_factor <- 1 - ( 3 * av_df[av_df$Type == sampleTypes[1],][1,]$ValueSd + 3 * av_df[av_df$Type == sampleTypes[2],][1,]$ValueSd / 
               abs(av_df[av_df$Type == sampleTypes[1],][1,]$ValueMean - av_df[av_df$Type == sampleTypes[2],][1,]$ValueMean ) )
  
  return(Z_factor)
}
