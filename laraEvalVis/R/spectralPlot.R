#_____________________________________________________________________________
#
# PROJECT: LARA
# CLASS: 
# FILENAME: spectralPlot.R
#
# CATEGORY:
#
# AUTHOR: mark doerr
# EMAIL: mark@ismeralda.org
#
# VERSION: 0.0.9
#
# CREATION_DATE: 2015/07/08
# LASTMODIFICATION_DATE: 2015/07/14
#
# BRIEF_DESCRIPTION: Library for plotting kinetic microtiter plate data
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

#' LA_Plot.spectrum
#'
#' @title kinetic Plot
#' @description This function is the main dispatcher function for reading several file formats into dataframes.
#' @param    layout_dir="./" (string) - plate layout file directory with leading / 
#' @param    use_db_layout=FALSE (boolean) - reading the layout from a database
#' @param    PLC=FALSE  (boolean) - automatically calculate pathlength correction 
#'             under the assumptions of a 96 round well SBS layout (r=034 cm)
#'             and 200ul liquid volume
#' @keywords plate readers
#' @note     todo - time conversion (h, min)
#' @export 

LA_Plot.spectrum <- function( plot_data_df=NULL, 
                                   plotVar=c("Wavelength", "Value"),
                                   groupingVar="Well",
                                   plot_type ="l",
                                   filename="_spectralPlot", ...)
{
  print("specplot 0.0.1")
  padding <- 4    # overall number of digits, leading digits are filled with zeros, used for formatting the barcode
  
  plot_char <- 1  # plotting character for the single point plots; 1=small circles
  num_col <- 10   # number of colours used for the colour range 
  # generating a colour palette for the circles
  #circle_colors <- colorRampPalette(c("blue","green","yellow","red"))(num_col+1) # rev(heat.colors(num_col))
  
  circle_colours <- colorRampPalette(c("blue","green","yellow","red"))(num_col+1) # rev(heat.colors(num_col))
  
  line_dot_colours <-  colorRampPalette(c("yellow","orange", "red"))(12) 
  # reverting original classes
  class(plot_data_df) <- "data.frame" 
  
  # automatically retrieving barcode if none is provided, e.g. for a unique filename
  if (current_barcode == "0000") current_barcode <- sprintf(paste("%0", padding, "d", sep=""), as.numeric(levels(plot_data_df$Barcode)[1])  )
  
  
  meas_type <- "S" #group_df$Type[1]
  meas_descr <- "spectrum" # group_df$Description[1]
  
  class(plot_data_df) <- "core"
  
  if(plot_xlab == "") plot_xlab <- "wavelength / nm"
  if(plot_ylab == "") plot_ylab <- "absorption / AU"
  LA_Plot(plot_data_df=plot_data_df, plotVar=c("Wavelength", "Value"), groupingVar=groupingVar, customCol=F,
               xlim=plot_xlim, ylim=plot_ylim,  xlab=plot_xlab, ylab=plot_ylab, plotType="l",  plotQuality=100,
               errorBars=plot_error_bars, wellNames=plot_well_names, description=plot_description, lineWidth=2,
               plotBestLinMod=FALSE, singlePlot=single_plot, overlay=plot_overlay, overview=plot_overview, filename=filename, outputFormat="eps")
}