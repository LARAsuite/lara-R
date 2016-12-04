#_____________________________________________________________________________
#
# PROJECT: LARA
# CLASS: 
# FILENAME: unitConvert.R
#
# CATEGORY:
#
# AUTHOR: mark doerr
# EMAIL: mark@ismeralda.org
#
# VERSION: 0.0.9
#
# CREATION_DATE: 2015/06/18
# LASTMODIFICATION_DATE: 2015/07/15
#
# BRIEF_DESCRIPTION: Library for handling units
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


#' convertTimeUnit
#'
#' @title converts time units
#' @description This function converts the time
#' @param    layout_dir="./" (string) - plate layout file directory with leading / 
#' @keywords plate readers
#' @note     todo - 
#' @export 
  
convertTimeUnit <- function(unit="min") {
  
}

#' convertEnzymeUnit
#'
#' @title converts enzyme units
#' @description This function is the main dispatcher function for reading several file formats into dataframes.
#' @author  Anders Knight, Mark Doerr
#' @param    unit
#' @keywords plate readers
#' @note     todo - not working yet, just a concept
#' @export 

convertEnzymeUnit <- function(unit="U") {
  
  #CalcWellUnits currently not necessary, but used to make the WellIterVector so leaving it in at this point
  CalcWellUnits<- function(unitSlope){ #Unit slope is in absorbance/min
    wellRadius=0.34 #cm, measured by Mark
    sampleVolume=0.3#cm^3 (300uL)
    enzymeAdded=5 #uL, amount of enzyme added to sample vol
    extinctionCoefficient=6234 #1/(M*cm) for vanillic acid based on Nat Protocols 2006 from A Holt
    Units<- unitSlope*300e6/(extinctionCoefficient*(sampleVolume/(pi*wellRadius^2))*enzymeAdded) #gives umol/min/mL enzyme as a result
  }
}
