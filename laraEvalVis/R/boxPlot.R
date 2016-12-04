#_____________________________________________________________________________
#
# PROJECT: LARA
# CLASS: 
# FILENAME: boxPlot.R
#
# CATEGORY:
#
# AUTHOR: mark doerr
# EMAIL: mark@ismeralda.org
#
# VERSION: 0.1.0
#
# CREATION_DATE: 2015/11/14
# LASTMODIFICATION_DATE: 2015/11/19
#
# BRIEF_DESCRIPTION: Library for plotting 2D barplots (barplots, histograms, top10 plots)
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


#' LA_Plot.boxplot
#'
#' @title boxplot plot
#' @description universal bar plot function with plotting mean and standard devaiation, if topN > 0 is provided, it orders the values in descending 
#'              order and plots the first N values
#' @author  Mark Doerr (mark.doerr at uni-greifswald.de)
#' @param    topN - integer to plot the n best values
#' @keywords plate readers
#' @note    todo
#' @export 

LA_Plot.boxplot <- function(plot_data_df=NULL,las=2, margin=0.1, ...) 
{
  printDebug("INFO(LA_Plot.histogram):plot var:%s", plot_var[1])
  # reverting original classes
  if ( ! is.null(plot_data_df)) class(plot_data_df) <- "data.frame" else return(FALSE)
  
  if(is.null(plot_var)) plot_var=c("Description", "Value")
  printDebug("INFO(LA_Plot.boxplot)  plot var: %s, %s", plot_var[1], plot_var[2])
  
  if (current_filename=="LA_Plot" ) current_filename <- paste(current_filename, "_boxplot", sep="" )
 
  fill_colours <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "grey", "darkgreen", "red", "white")
  #fill_colours <- c("white") 
  #fill_colours <-colorRampPalette(c("white", "lightgrey","darkgrey"))(8)
  
  plot_data_sel_df <- plot_data_df
  
  x_plot_data <- plot_data_df[,plot_var[1]]
  y_plot_data <- plot_data_df[,plot_var[2]]
  
  #y_plot_names <- plot_data_sel_df[,plot_var[2]]
  
  #if(is.null(plot_ylim)) plot_ylim <- c(0, y_plot_data_max * 1.1)
  
  switch( plot_output_format,
          png={ curr_plot_filename <- paste(current_barcode, '_', current_filename,".png", sep="" )
                png(curr_plot_filename,  units="px", width=plot_width, height=plot_height, res=120, bg=plot_bg) },
          jpeg={ curr_plot_filename <- paste(current_barcode,'_', current_filename,".jpg", sep="" )
                 jpeg(curr_plot_filename,  units="px", width=plot_width, height=plot_height, quality=plot_quality, res=120, bg=plot_bg) },
          svg={ curr_plot_filename <- paste(current_barcode,'_', current_filename,".svg", sep="" )
                svg(curr_plot_filename, antialias="default", width=plot_width, height=plot_height, bg=plot_bg) },
         { printError("ERROR(LA_Plot.topTen): unknown output format (%s) specified!", plot_output_format) }
  )
  
  #boxplot(Value~Description, data=plot_data_sel_df)
  # for longer names, use: par(mar = c(12, 5, 4, 2)+ 0.1) as parameter in boxplot
  # for grouping, use at: at=c(1,2,3,4, 6,7,8,9, 11,12,13,14)
  plot_mar <- par("mar") 
  par(mar = plot_mar + margin, cex.axis=char_scale, cex.lab=char_scale)
  
  boxplot(y_plot_data~x_plot_data, data=plot_data_sel_df, col=fill_colours, ylim=plot_ylim, xlab=plot_xlab, ylab=plot_ylab, 
          las=las )
  #barplot(y_plot_data,names.arg=y_plot_names, xlim=plot_xlim, ylim=plot_ylim, col=fill_colours, xlab=plot_xlab, ylab=plot_ylab, main=plot_title )
  
  if(plot_reference){
  
    y_plot_data_ref <- plot_data_sel_df[plot_data_sel_df$Type == 'R',plot_var[2]]
    
    y_plot_data_ref_mean <- mean( y_plot_data_ref, na.rm=TRUE)
    y_plot_data_ref_sd <- sd( y_plot_data_ref, na.rm=TRUE)
    y_plot_data_ref_min <- min( y_plot_data_ref, na.rm=TRUE)
    y_plot_data_ref_max <- max( y_plot_data_ref, na.rm=TRUE)
    
    abline(h=y_plot_data_ref_mean,  lty=1, lwd=line_width * 2, col=line_colour)
    abline(h=y_plot_data_ref_mean + y_plot_data_ref_sd, lty=2, lwd=line_width * 1.5, col=line_colour)
    abline(h=y_plot_data_ref_mean - y_plot_data_ref_sd, lty=2, lwd=line_width *1.5, col=line_colour)
   
    options(scipen=-10) # force to scientific notation
    axis(side=4, at=signif(y_plot_data_ref_mean, digits=3) ) # plots tick on the right side (=4)
    options(scipen=0)
    
    abline(h=y_plot_data_ref_min, lty=3, lwd=line_width, col="azure4")
    abline(h=y_plot_data_ref_max, lty=3, lwd=line_width, col="azure4")
  }

  dev.off()
}
