#_____________________________________________________________________________
#
# PROJECT: LARA
# CLASS: 
# FILENAME: wellPlot.R
#
# CATEGORY:
#
# AUTHOR: mark doerr
# EMAIL: mark@ismeralda.org
#
# VERSION: 0.0.9
#
# CREATION_DATE: 2015/07/07
# LASTMODIFICATION_DATE: 2015/09/02
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

#printDebug <- function(...) cat(sprintf(...), '\n' ,sep='', file=stdout())

#' LA_Plot.wells
#'
#' @title kinetic Plot
#' @description This function is the main dispatcher function for reading several file formats into dataframes.
#' @param    layout_dir="./" (string) - plate layout file directory with leading / 
#' @param    use_db_layout=FALSE (boolean) - reading the layout from a database
#' @param    PLC=FALSE  (boolean) - automatically calculate pathlength correction 
#'             under the assumptions of a 96 round well SBS layout (r=034 cm)
#'             and 200ul liquid volume
#' @keywords plate readers
#' @note     todo - 
#' @export 
  
LA_Plot.wells <- function(plot_data_df=NULL, colourScheme="custom",
                               plotAxis=TRUE, plotGrid=FALSE, plotBox=FALSE,
                               xlim=c(0,15.0), ylim=c(0.0,3.1),...)
{
  if(current_filename=="LA_Plot") current_filename <- "wellPlot"
  
  # reverting original classes
  if ( ! is.null(plot_data_df)) class(plot_data_df) <- "data.frame" else return(FALSE)
  
  num_rows <- panel_geom[1]
  num_cols <- panel_geom[2]
  
  switch( colourScheme,
          custom={  },
          phenolRed={ Col = c("#FFFF00","#FFA500" ,"#FF0000" ) },
          grey={ Col = grey(rank(plot_data_df$Value ) / nrow(plot_data_df))},
          green={ Col = hcl(h=120 , c = rank(plot_data_df$Value) , l=num_rows*num_cols)},
          { printError("ERROR(LA_Plot.wells): unknown colourScheme specified %s", colourScheme)}
  )

  # output format handling
  switch( plot_output_format,
        png={ curr_plot_filename <- paste(current_barcode,'_', current_filename,".png", sep="" )
              png(curr_plot_filename,  units="px", width=plot_width, height=plot_height, res=120, bg=plot_bg) },
        jpeg={ curr_plot_filename <- paste(current_barcode,'_', current_filename,".jpg", sep="" )
               jpeg(curr_plot_filename,  units="px", width=plot_width, height=plot_height, quality=plotQuality, res=120, bg=plot_bg) },
        svg={ curr_plot_filename <- paste(current_barcode,'_', current_filename,".svg", sep="" )
              svg(curr_plot_filename, antialias="default",width=plot_width, height=plot_height, bg=plot_bg) },
        { printError("ERROR(LA_Plot.wells): unknown output format (%s) specified!", plot_output_format) }
  )

  # plot with grey colour dictated by rank, no axes or labels
  with(plot_data_df, plot( x=as.numeric(ColNum), y= rev(as.numeric(RowNum)), 
                           pch=19, cex=4.5, 
                           col=Col , 
                           axes=F, xlab='', ylab ='', xlim=c(0.5, num_cols+0.5), ylim=c(0.5, num_rows+0.5) ))
  # add circular outline
  with(plot_data_df, points( x=as.numeric(ColNum), y=rev(as.numeric(RowNum)), pch=21, cex=4.5))

  # more elegant, but needs ggplot2
  # require(ggplot2)
  #  ggplot(plot_data_df, aes(y = factor(rown, rev(levels(Rown))), x = factor(Coln))) + 
  #  geom_point(aes(colour = Value), size =18)  +theme_bw() +
  #  labs(x=NULL, y = NULL)

  # add the axes
  if(isTRUE(plotAxis)) {
    axis(3, at=1:num_cols, labels=1:num_cols)
    axis(2, at=1:num_rows, labels=LETTERS[num_rows:1], las=2)
  }
  
  # the background grid
  if ( isTRUE(plotGrid)) grid()
  # and a box around the outside
  if ( isTRUE(plotBox)) box()

  dev.off()
}
