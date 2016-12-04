#'_____________________________________________________________________________
#
# PROJECT: LARA
# CLASS: 
# FILENAME: laraPlot.R
#
# CATEGORY:
#
# AUTHOR: mark doerr
# EMAIL: mark@ismeralda.org
#
# VERSION: 0.1.0
#
# CREATION_DATE: 2015/06/05
# LASTMODIFICATION_DATE: 2015/12/12
#
# BRIEF_DESCRIPTION: Library for plotting microtiter plate data
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

#' LA_Plot
#'
#' @title Plate Plot functions - generic function
#' @description This function is the main dispatcher function for reading several file formats into dataframes.
#' @param    layout_dir="./" (string) - plate layout file directory with leading / 
#' @param    use_db_layout=FALSE (boolean) - reading the layout from a database
#' @param    PLC=FALSE  (boolean) - automatically calculate pathlength correction 
#'             under the assumptions of a 96 round well SBS layout (r=034 cm)
#'             and 200ul liquid volume
#' @keywords plate readers
#' @note     todo - removing singlePlot concept - allowing only overview or singlePlot
#' @export 

LA_Plot <- function( plot_data_df=NULL, plotVar=NULL, groupingVar=NULL, barcode="0000", wavelength=0, 
                          overview=FALSE, singlePlot=TRUE, overlay=FALSE, preview=FALSE, errorBars=FALSE, fitting=FALSE, plotPeaks=FALSE,
                          wellNames=FALSE, description=FALSE, markSegments=TRUE,
                          markAverage=TRUE, markBest=FALSE, bestNegSlope=FALSE, thresholds=c(1.0, 1.0, 1.0), plotRef=FALSE, plotBestLinMod=FALSE,
                          plotType="b", plotChar=0, charScale=0, cex=0, lineType=1, lineWidth=1, lineCol='red', fillCol="lightgrey", numCol=20, customCol=FALSE,
                          panelGeometry=c(8,12), plotTitle="",  plotLegend=FALSE,
                          ylim=NULL,   xlim=NULL, plotAxes=TRUE, xlab="", ylab="", zlab="", plotGrid=FALSE,
                          plotBg="white", transp=1.0,
                          barWidth=0.5, barDistance=0.8, theta=-60, phi=15, scale=1.0,
                          plotWidth=1200, plotHeight=1024, plotQuality=70, filename = "LA_Plot", 
                          outputFormat='png', ... )                
{   
  plot_var <- plotVar 
  grouping_var <- groupingVar
  current_barcode <- barcode  
  current_wavelength <- wavelength
  plot_overview <- overview 
  plot_preview <- preview
  panel_geom <- panelGeometry
  plot_overlay <- overlay
  plot_error_bars <- errorBars
  plot_fitting <- fitting
  plot_peaks <- plotPeaks
  plot_description <- description
  plot_well_names <- wellNames
  plot_reference <- plotRef
  mark_best <- markBest  #  none, bestworst, bestaverage, bestbest,  
  mark_segments <- markSegments 
  mark_average <- markAverage
  plot_best_lin_mod <- plotBestLinMod 
  best_neg_slope <- bestNegSlope  
  plot_thresholds <- thresholds
  plot_type <- plotType
  #plot_char <- plotChar   # plotting character for the single point plots; 1=small circles
  if (plotChar == 0) assign("plot_char",  1, pos=-1) else assign("plot_char",  plotChar, pos=-1) # plotting character for the single point plots; 1=small circles
  
  line_type <- lineType   # default line type
  line_colour <- lineCol
  line_width <- lineWidth
  if ( charScale == 0 ) char_scale <- 1 else char_scale <- charScale
  fill_colours <- fillCol
  num_colours <- numCol
  plot_custom_colours <- customCol
  colour_saturation <- 1
  
  line_width <- lineWidth

  plot_title <- plotTitle
  plot_legend <- plotLegend
  plot_axes <- plotAxes
  plot_xlim <- xlim
  plot_ylim <- ylim
  plot_xlab <- xlab
  plot_ylab <- ylab
  plot_zlab <- zlab
  plot_grid <- plotGrid
  
  plot_width <- plotWidth 
  plot_height <- plotHeight
 
  plot_bg <- plotBg
  plot_transparence <- transp
  plot_bar_width <- barWidth
  plot_bar_distance <- barDistance
  plot_theta <- theta
  plot_phi <- phi
  plot_scale <- scale
  plot_output_format <- outputFormat
  
  if (cex == 0 ) cex_scale <- 1 else cex_scale <- cex
  
  plot_xaxt <- "s"
  plot_yaxt <- "s"
  plot_ann <- TRUE
  
  plot_col_index <- 1  # plotting character for the single point plots (first element of colour vector = black)
  
  if  ((plot_output_format == "svg") & (plotQuality < 600)) plot_quality <- 600 else plot_quality <- plotQuality 
  current_filename <- filename
  
  if (plot_overview) {    # plotting all data as multi panel overview
    single_plot <- FALSE
    if( plot_width < 7600 & ( ! isTRUE(plot_preview) )) { plot_width=7600;  plot_height=4400 }
    if( current_filename =="LA_Plot") current_filename <- paste(current_filename, "_plateView", sep="" )
  }  else  single_plot <- singlePlot
  
  if(isTRUE(plot_preview)){
    if (plot_width == 1200) {
      plot_width <- 1600
      plot_height <- 1024
    }
    colour_saturation <- 0.4
    plot_xaxt <- "n"
    plot_yaxt <- "n"
    plot_ann <-FALSE
    plot_description <- FALSE
    plot_error_bars <- FALSE
    #mark_best <- FALSE
    mark_average <- FALSE
    mark_segments <- FALSE
    plot_grid <- FALSE
    plot_col_index <- 5
    line_width <- lineWidth * 1.5
    if (cex == 0 ) cex_scale <- 0.7 else cex_scale <- cex
    if (plotChar == 0) assign("plot_char", 20, pos=-1) else assign("plot_char", plotChar, pos=-1)  # small filled circles
    if ( charScale == 0 ) char_scale <- 0.7 else char_scale <- charScale
    plot_xlab <- ""
    plot_ylab <- ""
    if( current_filename =="LA_Plot") current_filename <- paste(current_filename, "_preview", sep="")
  }
  
  if(plot_overlay) {
    plot_well_names <- FALSE 
    plot_col_index <- 2
  } 
  
  if ( ((plot_output_format == "svg" ) | (plot_output_format == "eps" )) & (plot_width==1200) ) 
  { 
    printDebug("eps svg plot")
    # default geometry
    plot_width <- 5.5 #  unit for svg is (out of a strange reason) in inch !
    plot_height <- 4.8  # unit for svg is (out of a strange reason) in inch !
  } else if ( (plot_output_format == "svg") & (plot_width != 1200) )  {
    plot_width <- plot_width / plot_quality  #  unit for svg is (out of a strange reason) in inch !
    plot_height <- plot_height / plot_quality  # unit for svg is (out of a strange reason) in inch !
  }
  
  if (length(plot_data_df) <= 0L) {print("ERROR (LA_Plot): No plot data provided"); return(FALSE)}
  
  UseMethod("LA_Plot")
}

#' LA_Plot.default
#'
#' @title Plate Plot functions - default function
#' @description Default function
#' @param plot_data_df
#' @export 
#' 

LA_Plot.default <- function( plot_data_df=NULL, ...)
{
  print("LARA platePlot 0.1.0")
  print("ERROR (LA_Plot.default): No plottable data provided for platePlot ! Please specify class of plot_data by class(plot_data) <- 'classname' ")
  return(FALSE)
}

#' LA_Plot.core
#'
#' @title core plate plot function
#' @description This function is the main dispatcher function for reading several file formats into dataframes.
#' @param    plot_data_df
#' @keywords plate readers
#' @note     todo - colour palette, scatterplot, single/overview fix, all_meta_data_df (slopes, fl)
#' @export 

LA_Plot.core <- function( plot_data_df=NULL, slopes_df=NULL, fl_df=NULL, fit_lst=NULL, norm_df=NULL, peaksIdx=NULL, num_col=10, ...)
{
  print("LA_Plot core 0.1.1i - mind plot var and grouping var  !!")
  printDebug("INFO(LA_Plot): plot_var:%s, grouping vars:%s, panel geom:%s", paste(plot_var, collapse=""),  grouping_var, paste(panel_geom, collapse="x"))
  printDebug("INFO(LA_Plot): plot params: overlay(%s), overview(%s), singleplot(%s)",
              plot_overlay, plot_overview, single_plot )
  
  padding <- 4    # overall number of digits, leading digits are filled with zeros, used for formatting the barcode
  
  # generating a colour palette for the circles with num_col number of colours
  #colorRampPalette(c("blue","green","yellow","red"))(num_col+1) # rev(heat.colours(num_col))
  
  # should be moved to entry function
  circle_colours <- rainbow(n=num_col+1, s=colour_saturation, v=1, start= 4/6, end=0, alpha=1)
  bg_colours <- rainbow(n=num_col+1, s=colour_saturation * 0.5, v=1, start=4/6, end=0, alpha=0.5)
  
  # colour palette for overlay plots
  #line_dot_colours <-  c("black", "darkgreen", "red", "blue", "violet", "brown", "black", "green", "pink", "white")
  # colour blind friendly palette (s. http://www.r-bloggers.com/creating-colorblind-friendly-figures-2/)
  # black, orange, sky blue, blueisch green, yellow, blue, vermillon, reddish purple
  line_dot_colours <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "grey", "darkgreen", "red", "white")
  
  # reverting original classes
  if ( ! is.null(plot_data_df)) class(plot_data_df) <- "data.frame" else return(FALSE)
  
  if ( ! is.null(slopes_df)) { slopes_vec <- slopes_df$Slope   #sapply(slopes_df, with, Slope)
                               # finding best slope as upper reference point for colour encoding
                               max_slope <- max(slopes_vec, na.rm=TRUE)
                               min_slope <- min(slopes_vec, na.rm=TRUE)
                               print("max slope" ); print(max_slope)
                               
                               # preparing the slope reference plots
                               # selecting the reference data, e.g. wild type activity
                               slope_ref_df <- slopes_df[slopes_df$Type == 'R',]
                               
                               slope_ref_wells <- slope_ref_df$Well
                               slopes_ref <- slope_ref_df$Slope
                               max_slope_ref <- max(slopes_ref, na.rm=TRUE)
                               min_slope_ref <- min(slopes_ref, na.rm=TRUE)
                               av_slope_ref <- mean(slopes_ref, na.rm=TRUE)
                               sd_slope_ref <- sd(slopes_ref, na.rm=TRUE)
  }
  
  if ( ! is.null(fl_df)) {
    max_fl <- max(fl_df$Value, rm.na = TRUE)
    
    ylim_fl=c(0.0, max_fl * 1.1 )
    
    ref_fl_df <- fl_df[fl_df$Type == 'R',]
    
    #ref_wells <- ref_fl_df$Well
    fl_ref = ref_fl_df$Value
    max_fl_ref <- max(fl_ref, rm.na = TRUE)
    av_fl_ref <- mean(fl_ref, rm.na = TRUE)
    
    if ( ! is.null(slopes_df)) {
      ratios_ref <- slopes_ref / fl_ref
      ratios_ref[which(fl_ref < plot_thresholds[2])] <- 0
      
      #print(slopes_ref)
      #print(fl_ref)
      #print(ratios_ref)
      
      av_sl_fl_ref_ratio = mean(ratios_ref, rm.na = TRUE)
      max_sl_fl_ref_ratio = max(ratios_ref, rm.na = TRUE)
      #printDebug(" max ref ratio: %s", max_sl_fl_ref_ratio )  
    }
  }
  
  # automatically retrieving barcode if none is provided, e.g. for a unique filename
  if (current_barcode == "0000") current_barcode <- sprintf(paste("%0", padding, "d", sep=""), as.numeric(levels(plot_data_df$Barcode)[1])  )
        
  if (plot_reference) {
    printDebug("INFO(LA_Plot): retrieving plotting references")
    
    ref_df <- plot_data_df[plot_data_df$Type == 'R',]
    
    ref_wells <- ref_df$Well
    ref_values <- ref_df$Value
  }
  
  # calulate interval sizes for more convenient placement of the labels in the panel plots
  x_interval <- plot_xlim[2]-plot_xlim[1]
  y_interval <- plot_ylim[2]-plot_ylim[1]
    
  # function for plotting each single well panel
  byItemPlot <- function(group_df)
  {
    #print("groupplot - name")
    # print(names(group_df))
    #  print(head(group_df,15))
    
    # *** praparation of plot data
    x_plot_data <- group_df[,plot_var[1]]
    y_plot_data <- group_df[,plot_var[2]]
  
    if (length(x_interval) == 0 ) y_interval <- max(x_plot_data, na.rm=T )
    if (length(y_interval) == 0 ) y_interval <- max(y_plot_data, na.rm=T )
    
    # colour handling
    if(plot_custom_colours) plot_colours <- line_dot_colours[group_df$Colour] else plot_colours <- line_dot_colours[plot_col_index]
    
    # retrieving current grouping and well name
    curr_grouping <- as.character.factor(group_df[,grouping_var][1])
    curr_well <- as.character.factor(group_df$Well[1])
    
    #printDebug("plot group: %s", curr_grouping)
    
    # !!! adjust to general name !!!
    meas_type <- group_df$Type[1]  # current type of value
    meas_descr <- group_df$Description[1]
    
    if ( ! is.null(slopes_df)) {
      # retrieving the corresponding slope data frame to the current well
      curr_slope_df <- slopes_df[slopes_df[,grouping_var] == curr_grouping,]
      
      #curr_slope_lst <- slopes_df[[meas_descr]]
      curr_slope <- curr_slope_df$Slope  #curr_slope_lst$Slope #curr_slope_df$Slope
      curr_intercept <- curr_slope_df$Intercept #curr_slope  #curr_slope_lst$Intercept
      
      #print("slope and intercept") ; print(curr_slope); print(curr_intercept)
    }
    
    if ( ! is.null(fl_df)) {
      curr_fl <- fl_df[(fl_df$Well == curr_well),]$Value
      
      if  (( ! is.null(slopes_df) ) &  (curr_fl > plot_thresholds[2]) ) curr_act_fl_ratio <- curr_slope / curr_fl
      else curr_act_fl_ratio = 0.0
        
      if(is.na(curr_fl) ) curr_fl = 0.0
    }
    
    # *** preparation of plot
    if(single_plot & (!plot_overlay) ) {
      if (grouping_var == "Well" ) fn_attr <- paste(curr_well, gsub("[ ,\\(,\\),/,\\.]", "_", meas_descr), sep="" ) else {
        fn_attr <- gsub("[ ,\\(,\\),/,\\.]", "_", meas_descr) }
      switch( plot_output_format,
              png={ curr_plot_filename <- paste(current_barcode, '_' , fn_attr, "_", current_filename,".png", sep="" )
                    png(curr_plot_filename,  units="px", width=plot_width, height=plot_height, res=120, bg=plot_bg) },
              jpeg={ curr_plot_filename <- paste(current_barcode,'_' , fn_attr,"_", current_filename,".jpg", sep="" )
                     jpeg(curr_plot_filename,  units="px", width=plot_width, height=plot_height, quality=plot_quality, res=120, bg=plot_bg) },
              svg={ curr_plot_filename <- paste(current_barcode,'_' , fn_attr, "_", current_filename,".svg", sep="" )
                    svg(curr_plot_filename, antialias="default",width=plot_width, height=plot_height, bg=plot_bg) },
              eps={ curr_plot_filename <- paste(current_barcode,'_' , fn_attr, "_", current_filename,".ps", sep="" )
                    setEPS()
                    postscript(curr_plot_filename, width=plot_width, height=plot_height, bg=plot_bg) },
                  { printError("ERROR(LA_Plot.wells): unknown output format (%s) specified! Supported formats: 'png','jpeg','svg','eps'", 
                               plot_output_format) }
      )  
    }
    
    # *** first plotting raw data points of measurement  
    # if ALL values are NA, then plot a box
    if ( all(is.na(y_plot_data)) )  {  
      plot(1, type="n", axes=FALSE, xlab="", ylab="", xlim=plot_xlim, ylim=plot_ylim)
      box(col="grey")
      text(plot_xlim[1] + x_interval * 0.45, plot_ylim[1] + y_interval * 0.50, "No data available", cex=2 * char_scale) 
    } else {
        if(plot_error_bars) {
        require(gplots)
        # main plot call for error bar plots
        plotCI( x_plot_data, y_plot_data, uiw=group_df$StdErr, pch=plot_char, type=plot_type, lwd=line_width, xlim=plot_xlim, ylim=plot_ylim,
                xlab=plot_xlab, ylab=plot_ylab, col=plot_colours, cex=cex_scale, cex.axis=char_scale, cex.lab=char_scale, )
        
        # workaround without gplots:
        #plot( plot_df$MeanAbs ~ plot_df$TimeDiff, type = "b" , main = plotTitle, xlab = "time [min]", ylab = "Absorption [AU]" )
        #arrows(plot_df$TimeDiff, plot_df$MeanAbs-plot_df$Sd, plot_df$TimeDiff, plot_df$MeanAbs+plot_df$Sd, length=0.08, angle=90, code=3)
        
        } else {
          # main plot call
          plot(y_plot_data~x_plot_data, pch=plot_char, type=plot_type, cex=cex_scale, cex.axis=char_scale, cex.lab=char_scale, lwd=line_width, 
               axes=plot_axes, xaxt=plot_xaxt, yaxt=plot_yaxt, ann=plot_ann, xlim=plot_xlim, ylim=plot_ylim,
               xlab=plot_xlab, ylab=plot_ylab, col=plot_colours )
        }
        
        # change background colour of a single panel
        if (isTRUE(plot_preview)) {
          if ( ! is.null(slopes_df))  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col=bg_colours[abs(curr_slope)/max_slope*10+1]) 
        }
        
        if(isTRUE(plot_grid)) grid(col='gray')
        
        if(plot_fitting) {
          curr_mm <- fit_lst[[as.character(group_df$Enzyme)[1]]]
          curr_fit = curr_mm$CurveFit
          
          x_min = min(x_plot_data, na.rm=TRUE)
          x_max = max(x_plot_data, na.rm=TRUE)
          
          printDebug("WARNING(LA_Plot): Auto range predition needs to be improved !!")
          #x_pred = seq(plot_xlim[1], plot_xlim[2], by=x_interval/50)
          x_pred = seq(x_min, x_max, by=(x_max-x_min)/50)
          new_dat_df <- data.frame(x_pred)
          names(new_dat_df) <- c(plot_var[1])
          # data.frame(pred_col_name=x_pred)
          #y_pred = predict(curr_mm$CurveFit, newdata=new_dat_df,  interval=c('none'))
          
          if(  ! is.null(curr_fit) ) {
            y_pred = predict(curr_fit, newdata=new_dat_df,  interval=c('none'))
            
            # col=line_dot_colours[plot_col_index]
            lines(x_pred, y_pred, lt=line_type, col=plot_colours, lwd=line_width)
          } 
        }
        
        if(plot_peaks){
          x_peak_data <- group_df[peaksIdx,plot_var[1]] 
          y_peak_data <- group_df[peaksIdx,plot_var[2]]
          if (length(y_interval) == 0 ) y_interval <- max(y_peak_data, na.rm=T )
          #printDebug(y_interval)
          text(x_peak_data, y_peak_data + (y_interval * 0.02), signif(x_peak_data, digits=3), cex=0.8 * char_scale, col='darkred')
        }
        
        ## plotting min and max, average +- std deviations of reference slope as faintly shaeded areas or faint dotted lines
        if(plot_reference) {
          if ( ! is.null(slopes_df) ){
       
            ref_triangle_x <- c(plot_xlim, rev(plot_xlim))
            ref_triangle_y <- c(curr_intercept, curr_intercept + max_slope_ref*plot_xlim[2], curr_intercept + min_slope_ref*plot_xlim[2], curr_intercept )
            polygon(x=ref_triangle_x, y=ref_triangle_y, col = "grey97", border = "grey95")
            
            ref_mean_triangle_x <- c(plot_xlim * c(1,0.98), rev(plot_xlim) * c(0.98,1))
            ref_mean_triangle_y <- c(curr_intercept, curr_intercept + av_slope_ref*ref_mean_triangle_x[2] + sd_slope_ref*ref_mean_triangle_x[2], 
                                     curr_intercept + av_slope_ref*ref_mean_triangle_x[3] - sd_slope_ref*ref_mean_triangle_x[3], curr_intercept )
            polygon(x=ref_mean_triangle_x, y=ref_mean_triangle_y, col = "lavenderblush", border = NA)
            
            av_slope_ref_line <- c(curr_intercept, av_slope_ref)
            abline( av_slope_ref_line, col='white', lty=1, lwd=line_width*1.5  )

#             printing dashed lines to mark reference region       
#             max_slope_ref_line <- c(curr_intercept, max_slope_ref)
#             min_slope_ref_line <- c(curr_intercept, min_slope_ref)            
#             abline( max_slope_ref_line, col='pink', lty=2, lwd=line_width  )
#             abline( min_slope_ref_line, col='pink', lty=2, lwd=line_width  )

          } else{
            x_plot_data_ref <- group_df[group_df$Type == 'R', plot_var[1]]
            y_plot_data_ref <- group_df[group_df$Type == 'R', plot_var[2]]
            
            y_plot_data_ref_mean <- mean( y_plot_data_ref, na.rm=TRUE)
            y_plot_data_ref_sd <- sd( y_plot_data_ref, na.rm=TRUE)
            y_plot_data_ref_min <- min( y_plot_data_ref, na.rm=TRUE)
            y_plot_data_ref_max <- max( y_plot_data_ref, na.rm=TRUE)
            
            abline(h=y_plot_data_ref_mean,  lty=1, lwd=line_width * 2, col="red")
            abline(h=y_plot_data_ref_mean + y_plot_data_sd, lty=2, lwd=line_width * 1.5, col="red")
            abline(h=y_plot_data_ref_mean - y_plot_data_sd, lty=2, lwd=line_width * 1.5, col="red")
            
            abline(h=y_plot_data_ref_min, lty=3, lwd=line_width, col="azure4")
            abline(h=y_plot_data_ref_max, lty=3, lwd=line_width, col="azure4")
          }
          if (curr_well %in% ref_wells) box( lwd=4.0) # mark reference wells
        }

        # *** plotting best linear model line
        if(isTRUE(plot_best_lin_mod)) {  
          xlim_best_lm = c(curr_slope_df$BestLinLeft, curr_slope_df$BestLinRight)
          
          if ( isTRUE(mark_segments) ){
            segments( xlim_best_lm[1], xlim_best_lm[1] * curr_slope + curr_intercept,
                      xlim_best_lm[2], xlim_best_lm[2] * curr_slope + curr_intercept , col=line_colour, lwd=line_width * 2.5, axes=plot_axes )
            abline( v=xlim_best_lm , col='darkgray', lwd = line_width * 0.7, lty="dotted", axes=plot_axes )  
            # *** adding full length linear model line
            if ( ! is.null(slopes_df)) abline( curr_intercept, curr_slope, col=line_colour, lwd = line_width * 0.2)
          } else {
            if ( ! is.null(slopes_df)) abline( curr_intercept, curr_slope, col=line_colour, lwd = line_width )
          }
          
          ticks = xlim_best_lm
         if ( ! isTRUE(plot_preview)) axis(side=3, at=ticks )
        }
        # *** plotting frames marking best slopes 
        if(mark_best) {
          #printDebug("INFO(LA_Plot): mark best") 
          if ( ! is.null(slopes_df)) {
            if ( ! is.null(fl_df)) {
              box_col <- circle_colours[abs(curr_act_fl_ratio)/max_fl*10+1]
              
              if (curr_act_fl_ratio > (av_sl_fl_ref_ratio  * plot_thresholds[3]) ) box(col="red", lwd=10.0 * char_scale)
              #if (curr_act_fl_ratio > (max_sl_fl_ref_ratio  * plot_thresholds[3]) ) box(col="red", lwd=10.0)
              #printDebug("curr_ratio: %s", curr_act_fl_ratio)
            } else {
              if ((curr_slope  > max_slope_ref) & ( ! isTRUE(plot_preview)) ) box(col="darkred", lwd=10.0 * char_scale) 
              
              box_col <- circle_colours[abs(curr_slope)/max_slope*10+1]
            }
            
            slope_col <- circle_colours[abs(curr_slope)/max_slope*10+1]
            symbols(plot_xlim[1] + x_interval * 0.0355, plot_ylim[1] + y_interval * 0.93, circles=c(1), fg=slope_col, bg=slope_col,
                    inches=0.2 * char_scale, add=T)
            if ( ! isTRUE(plot_preview) ) box(col=box_col, lwd=4.0 * char_scale) 
            
          } else {
            # drawing horizontal line with 
            y_max = max(y_plot_data, na.rm=TRUE)
            #printDebug("INFO(LA_Plot): mark best - h line at: %s", y_max) 
            abline(h = y_max, col='darkgray', lwd =line_width * 0.7, lty="dotted" )
            axis(side=4, at=signif(y_max, digits=3), tck=0.02, las=1, mgp=c(0, -2.4, 0) ) # plots ticks/value on the right side (=4)
          }
        }
        if(best_neg_slope) {
          slope_col <- circle_colours[abs(curr_slope)/abs(min_slope)*10+1]
          symbols(plot_xlim[1] + x_interval * 0.03, plot_ylim[1] +  y_interval * 0.93, circles=c(1), fg=slope_col, bg=slope_col,
                  inches=0.2, add=T)
          box(col=slope_col, lwd=4.0)  
          if (curr_slope  <= min_slope) box(col="orange", lwd=40.0)
        }
    } # end none empty plot
    
    # *** adding info into the graph (well number, slope, description)
    # plotting well labels
    
    if(plot_well_names) text(plot_xlim[1] + x_interval * 0.11, plot_ylim[1] + y_interval * 0.93, 
                             curr_grouping, cex= 2 * char_scale ) 
    if(plot_description) text(plot_xlim[1] + x_interval * 0.2, plot_ylim[1] + y_interval * 0.86, 
                              paste(meas_type, " : ", meas_descr, sep=" "), cex=0.8 * char_scale ) 
    
    # adding slope info to plot
    if ( ! is.null(slopes_df) & ! isTRUE(plot_preview) ) {
      # former pos: 0.2
      options(scipen=-10) # force to scientific notation
      text(plot_xlim[1] + x_interval * 0.3, plot_ylim[1] +  y_interval * 0.02, 
           paste("slope =", signif(curr_slope, digits=3)  , "AU / plot time unit"  ,  sep = " "),  cex=char_scale,  col='darkred')  
      options(scipen=0) # resetting scientific notation to default threshold
    }

    if ( ! is.null(fl_df)) {
      par(new = TRUE)
      if (length(curr_fl) > 1 )  {
         # Fluorecent kinetics plot
         plot(curr_fl~x_plot_data, col="green", col.lab="darkgreen", xlab="", ylab="",  pch=plot_char, type=plot_type, axes = FALSE, xlim=plot_xlim, ylim=ylim_fl)
         if ( ! isTRUE(plot_preview) ) {
           axis(side = 4, col="darkgreen", col.axis='darkgreen')
         } 
      } else {
        # Fluorescent bar plotting
        options(scipen=-10) # force to scientific notation
        if ( isTRUE(plot_preview) ) {
          text(plot_xlim[1] + x_interval * 0.7, plot_ylim[1] +  y_interval * 0.02, col="darkblue", 
               paste("", signif(curr_act_fl_ratio, digits=3),  sep = " "),  cex=char_scale )
        } else {
          text(plot_xlim[1] + x_interval * 0.7, plot_ylim[1] +  y_interval * 0.02, col="darkblue", 
               paste("ratio: slope/fl =", signif(curr_act_fl_ratio, digits=3),  cex=char_scale, sep = " "))
        }
        options(scipen=0) # resetting scientific notation to default threshold 
        
        par(new = TRUE)
        
        barplot(height=c(0,0,0,0,0,curr_fl), col = "green", axes = FALSE, xlim = c(0,7), ylim=ylim_fl)
        
        #mtext(side = 4, line = 3, "FL", cex=0.8, adj=1, padj=1)
        # * adding text at top of bars
        text(x = 6.7, y = curr_fl, label = signif(curr_fl, digits=3) , pos = 3, cex = 0.9 * char_scale, col = "darkgreen")
        if ( ! isTRUE(plot_preview) ) axis(side = 4)
        par(new = T)
        plot(0.0,0.0, xlim=c(0.0,1.0), ylim=c(0.0,1.0), axes=F, col='white'  ) # dummy plot to get rid of axes 
        if (curr_fl  > max_fl_ref) symbols(0.65,0.93, circles=c(0.03),fg='darkgreen', bg='green', inches=FALSE, add=T)
        if (curr_slope  > max_slope_ref * plot_thresholds[1]) symbols(0.72,0.93, circles=c(0.03),fg='darkred', bg='red', inches=FALSE, add=T)
        if (curr_act_fl_ratio  > max_sl_fl_ref_ratio ) {
          symbols(0.79,0.93, circles=c(0.03),fg='darkblue', bg='blue', inches=FALSE, add=T)
          # add text with % comp. to WT
        }
      }

    }

    # *** closing plot output devic/file  
    if(single_plot & (! plot_overlay)) {  
      dev.off() 
      printDebug("INFO(LA_Plot): Single plot device is now off")
    }
    if( plot_overlay) {
      printDebug("INFO(LA_Plot): plotchar %s, plot_col_index %s, lt: %s", plot_char, plot_col_index, line_type )
      assign("plot_char", plot_char + 1, inherits=T)
      assign("plot_col_index", plot_col_index + 1, inherits=T)
      assign("line_type", line_type + 1, inherits=T)
      par(new=T)
    }
  } # end if item plot function
 
  if( plot_overlay | plot_overview ) {
    switch( plot_output_format,
            png={ curr_plot_filename <- paste(current_barcode,'_', current_filename,".png", sep="" )
                  png(curr_plot_filename,  units="px", width=plot_width, height=plot_height, res=120, bg=plot_bg) },
            jpeg={ curr_plot_filename <- paste(current_barcode,'_', curr_well, current_filename,".jpg", sep="" )
                   jpeg(curr_plot_filename,  units="px", width=plot_width, height=plot_height, quality=plot_quality, res=120, bg=plot_bg) },
            svg={ curr_plot_filename <- paste(current_barcode,'_', current_filename,".svg", sep="" )
                  svg(curr_plot_filename, antialias="default",width=plot_width, height=plot_height, bg=plot_bg) },
            eps={ curr_plot_filename <- paste(current_barcode,'_', current_filename,".eps", sep="" )
                  setEPS()
                  postscript(curr_plot_filename, width=plot_width, height=plot_height, bg=plot_bg) },
                { printError("ERROR(LA_Plot.wells): unknown output format (%s) specified! Supported formats: 'png','jpeg','svg','eps'", plot_output_format) }
    )
    if (plot_overview) par(mfrow=panel_geom)  # overview panel layout with 8 x 12 panels as default, defined by panelGeom
    if(isTRUE(plot_preview)) par(mar = c(0, 0, 0, 0))
  }
  # main iteration loop over all groups
  invisible( by(plot_data_df, plot_data_df[,grouping_var], byItemPlot) )

  if(plot_overlay & plot_legend){
    printDebug("INFO(LA_Plot): plotting legend")
    #print(levels(plot_data_df[plot_data_df$Type!="0",grouping_var]))
    
#     print(head(plot_data_df[,grouping_var]))
#     plot_data_df[,grouping_var] <- as.character.factor(plot_data_df[,grouping_var])
#     print(head(plot_data_df[,grouping_var]))
    
    # mind the order !
    descr_text = plot_data_df[plot_data_df$Type!="0",grouping_var]
    legend_text_vec = union( levels(descr_text), descr_text ) #unique(plot_data_df[plot_data_df$Type!="0",grouping_var])  # unique( plot_data_df[,grouping_var])
    print(legend_text_vec)
    legend(plot_xlim[1] + x_interval * 0.73, plot_ylim[1] +  y_interval * 0.04 * length(legend_text_vec), 
           legend_text_vec , cex=char_scale, lty=(1:length(legend_text_vec)),
           col=line_dot_colours[(2:(length(legend_text_vec)+1))], pch=(1:length(legend_text_vec))) # +1 to compensate for starting at 2
  }
  if(plot_overview | plot_overlay) dev.off() # printDebug("INFO(LA_Plot): fl or overiew") ;   # finall switching plot device off
}

