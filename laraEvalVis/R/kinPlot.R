#_____________________________________________________________________________
#
# PROJECT: LARA
# CLASS: 
# FILENAME: kinPlot.R
#
# CATEGORY:
#
# AUTHOR: mark doerr
# EMAIL: mark@ismeralda.org
#
# VERSION: 0.1.0
#
# CREATION_DATE: 2015/06/08
# LASTMODIFICATION_DATE: 2015/12/12
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


#' LA_Plot.kinetics
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


LA_Plot.kinetics <- function( plot_data_df=NULL, slopes_df=NULL, fl_df=NULL, plotVar=c("DiffTime", "Value"), timeUnit="secs",
                              groupingVar="Well", plot_well_names=TRUE, description=TRUE, single_plot=FALSE, 
                              plot_overlay=FALSE, plot_overview=TRUE, filename="_kinPlot", ...)
{
  printDebug("kinetics 0.1.0_kin_b; time unit %s", timeUnit)
  
  padding <- 4  # barcode zero padding
  
  class(plot_data_df) <- "data.frame"  # class reverting to data.frame
  
  # automatically retrieving barcode if none is provided, e.g. for a unique filename
  if (current_barcode == "0000") current_barcode <- sprintf(paste("%0", padding, "d", sep=""), as.numeric(levels(plot_data_df$Barcode)[1])  )
  
  # select data of particular wavelength
  if (current_wavelength == 0 ) { 
    if ( ! is.null(plot_data_df$Wavelength) ) {
      print("Warning(LA_Plot.kinetics): no wavelength specified, plotting first available wavelength")
      current_wavelength <- plot_data_df$Wavelength[[1]]
      abs_wl_sel_df <- plot_data_df[plot_data_df$Wavelength == current_wavelength,]  
    } else abs_wl_sel_df <- plot_data_df
  } else abs_wl_sel_df <- plot_data_df[plot_data_df$Wavelength == current_wavelength,]
  
  if(timeUnit != "secs"){
    date_time_ref = min(abs_wl_sel_df$RefTime)
    #date_time_ref = min(abs_wl_sel_df$RefTime)
    abs_wl_sel_df$DiffTimeUnit <- difftime(abs_wl_sel_df$RefTime, date_time_ref , units=timeUnit)
    #abs_wl_sel_df$DiffTimeUnit <- as.difftime(abs_wl_sel_df$RefTime, units=timeUnit)
    print(head(abs_wl_sel_df))
    plotVar=c("DiffTimeUnit", plotVar[2])
  }
  class(abs_wl_sel_df) <- "core"
  if(plot_xlab =="") plot_xlab <- paste("time /", timeUnit)
  if(plot_ylab =="") plot_ylab <- "absorption / AU"
  LA_Plot(plot_data_df=abs_wl_sel_df, slopes_df=slopes_df, fl_df=fl_df, plotVar=plotVar, groupingVar=groupingVar, plotLegend=plot_legend,
               charScale=char_scale, plotChar=plot_char, barcode=current_barcode,  cex=char_scale,
               xlim=plot_xlim, ylim=plot_ylim, plotAxes=plot_axes,  xlab=plot_xlab, ylab=plot_ylab, plotType=plot_type, description=plot_description,
               plotBestLinMod=plot_best_lin_mod, markBest=mark_best, errorBars=plot_error_bars, bestNegSlope=best_neg_slope, thresholds=plot_thresholds, plotRef=plot_reference,
               wellNames=plot_well_names, singlePlot=single_plot, overlay=plot_overlay, overview=plot_overview,  preview=plot_preview, filename=filename, outputFormat=plot_output_format)
}

#' LA_Plot.pHProfile
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

LA_Plot.pHProfile <- function( plot_data_df=NULL, fit_lst=NULL, single_plot=TRUE, overlay=TRUE, plot_fitting=TRUE,  
                                     plot_type="p", plot_xlab = "pH", plot_ylab = "Kcat/Km  [1/M 1/s]",  plotVar=c("MediumpH", "KcatKm"),
                                     groupingVar="Enzyme", filename="_pHProfile_1", ...)
{
  print("MMkin plot 01")
  
  class(plot_data_df) <- "core"
  LA_Plot(plot_data_df=plot_data_df, fit_lst=fit_lst, xlim=plot_xlim, ylim=plot_ylim, errorBars=plot_error_bars, fitting=TRUE, plotType=plot_type,
               plotVar=plotVar, groupingVar=groupingVar, xlab=plot_xlab, ylab=plot_ylab,  singlePlot=T, overlay=T, overview=F, filename=filename)
  
}

#' LA_Plot.MMkinetics
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

LA_Plot.MMkinetics <- function( plot_data_df=NULL, fit_lst=NULL, single_plot=TRUE, overlay=TRUE, plot_fitting=TRUE,  
                                     plotType="p", plot_xlab = "Substr. Conc / M", plot_ylab = "velocity / M s-1", 
                                     groupingVar="Enzyme", plotVar=c("Substrate1Conc", "v0"), filename="MM_kinPlot", ...)
{
  print("MMkin plot 01a")
  
  class(plot_data_df) <- "core"
#   LA_Plot(plot_data_df=plot_data_df, fit_lst=fit_lst, xlim=plot_xlim, ylim=plot_ylim, errorBars=plot_error_bars, 
#           plotLegend=plot_legend, fitting=TRUE, plotType=plot_type,
#           plotVar=plotVar, groupingVar=groupingVar, xlab=plot_xlab, ylab=plot_ylab,
#           singlePlot=T, overlay=T, overview=F, filename=filename)
  
  LA_Plot(plot_data_df=plot_data_df, fit_lst=fit_lst,  plotVar=plotVar, groupingVar=groupingVar, customCol=F,
          xlim=plot_xlim, ylim=plot_ylim,  xlab=plot_xlab, ylab=plot_ylab, plotType=plotType,  plotChar=plot_char,
          plotLegend=plot_legend, fitting=TRUE,  barcode=current_barcode,
          errorBars=plot_error_bars, wellNames=plot_well_names, description=plot_description,
          plotBestLinMod=FALSE, markBest=mark_best, 
          singlePlot=T, overlay=plot_overlay, overview=FALSE, preview=plot_preview, filename=filename)
}

#' LA_Plot.growth
#'
#' @title Plotting bacterial growth and Expression Data
#' @description 
#'   Slopes need to be pre-calculated to also plot max and min slopes
#' @param filename
#' @keywords plate readers
#' @export 
#' @examples
#'    good parameters f. jpg: plotWidth=7600, plotHeight=4400, plotQuality=70
#'    single plot: plotWidth=1200, plotHeight=800, plotQuality=70
#' @note todo: connection line growth expression
#' 

LA_Plot.growth <- function( plot_data_df=NULL, wavelength=660, numGrowth=0, plot_type="b", timeUnit="hours", sampleType='S', averageGrowth=FALSE, 
                                 plotVar=c("DiffTimeUnit", "Value"), groupingVar="Well", plot_description=TRUE, plot_well_names=TRUE,
                                 plotTitle = "Av. Growth and Expression of Plate ",  filename = "_growthExprPlot", ... )
{  
  printDebug("growthExpression plot 0.1.0d")
  
  class(plot_data_df) <- "data.frame"  # class reverting to data.frame

  if(averageGrowth) growth_sel_df <- plot_data_df[(plot_data_df$Wavelength == wavelength & plot_data_df$Type == sampleType),] else {
    growth_sel_df <- plot_data_df[(plot_data_df$Wavelength == wavelength),]
  }
  if(current_barcode == '0000') current_barcode <- growth_sel_df$Barcode[1]  
  
  if(plotTitle != "") plotTitle <-  paste(plotTitle, current_barcode, " (", wavelength, "nm)", sep="" )
  
  growth_sel_df$Colour <- 1 # black
  
  grouped_df <- NULL
  assignColour <- function(grouped_df)
  {  
      grouped_df$Colour <- 7  # red
      grouped_df[(1:numGrowth),]$Colour <- 4 # dark green
      return(grouped_df)     
  }  
  if(numGrowth > 0) growth_sel_df <- do.call(rbind, as.list (by( growth_sel_df,growth_sel_df$Well, assignColour )) )
  
#   print(head(growth_sel_df))
  
  if(averageGrowth & (! plot_overview)) { 
    printDebug("av gr no overview")
    #av_growth_df <- averageByGroups1Crit( non_corr_df=growth_sel_df, groupingVar="Type", newDF=F )  
    av_growth_df <- averageByGroups2Crit( non_corr_df=growth_sel_df, groupingVar1="Description", groupingVar2="RefTime")
    single_plot <- TRUE
    plot_overview <- FALSE
    plot_overlay <- FALSE
    plot_well_names <- FALSE
    plot_description <- FALSE
    plotVar <- c("DiffTimeUnit", "ValueMean")
    groupingVar <- "Description"
    plot_error_bars <- TRUE
  } else {
    if(averageGrowth & plot_overview  ) { 
      av_growth_df <- averageByGroups2Crit( non_corr_df=growth_sel_df, groupingVar1="Description", groupingVar2="NumReads")
      plot_overlay <- FALSE
      plot_well_names <- FALSE
      plot_description <- TRUE
      plotVar=c("DiffTimeUnit", "ValueMean")
      groupingVar="Description"
      single_plot = FALSE
      plot_error_bars <- TRUE
    } else av_growth_df <- growth_sel_df
  }

  date_time_ref = min(av_growth_df$DateTime)
  av_growth_df$DiffTimeUnit <- difftime(av_growth_df$DateTime, date_time_ref , units=timeUnit)
  
  av_growth_df$StdErr <- av_growth_df$ValueSd #select correct error for error bars in plot
  
#   print(head(av_growth_df))
#   print(tail(av_growth_df))
#   
  #xlim = c(0.0, max(plot_expr_df$TimeDiff) * 1.05 )
  #ylim = c(0.0, max(plot_expr_df$MeanAbs) * 1.3 )

  #print(head(av_growth_df))
  #print(tail(av_growth_df))
  
  class(av_growth_df) <- "core"
  
  if(plot_xlab == "") plot_xlab <- paste("time /", timeUnit)
  if(plot_ylab == "") plot_ylab <- "absorption / AU"
  LA_Plot(plot_data_df=av_growth_df, plotVar=plotVar, groupingVar=groupingVar, customCol=TRUE,
          charScale=char_scale, plotChar=plot_char, barcode=current_barcode, cex=cex_scale, 
               xlim=plot_xlim, ylim=plot_ylim, plotAxes=plot_axes, xlab=plot_xlab, ylab=plot_ylab, plotType="b", 
               errorBars=plot_error_bars, wellNames=plot_well_names, description=plot_description,
               plotBestLinMod=FALSE, markBest=mark_best, 
               singlePlot=single_plot, overlay=plot_overlay, overview=plot_overview, preview=plot_preview, filename=filename)
}

#' LA_Plot.growthRibbon
#'
#' @title Plotting bacterial growth and Expression Data
#' @description 
#'   Slopes need to be pre-calculated to also plot max and min slopes
#' @author Johannes Kabisch (big thanx Johannes !)
#' @param filename
#' @keywords plate readers
#' @export 
#' @examples
#'    good parameters f. jpg: plotWidth=7600, plotHeight=4400, plotQuality=70
#'    single plot: plotWidth=1200, plotHeight=800, plotQuality=70
#' @note todo: not working yet - just a concept
#' 

LA_Plot.growthRibbon <- function( plot_data_df=NULL, wavelength=600, numGrowth=0, plot_type="b", timeUnit="mins", sampleType='S', averageGrowth=FALSE, 
                                 plotVar=c("DiffTimeUnit", "Value"), groupingVar="Well", plot_description=TRUE, plot_well_names=TRUE,
                                 plotTitle = "Av. Growth and Expression of Plate ",  filename = "_growthExprPlotRibbon", ... )
{  
  ## load ggplot2
  require(ggplot2)
  require(reshape2)
 
  ##Farben festlegen
  farbeace<-rgb(212, 94, 76, maxColorValue=255)
  farbeWT<-rgb(62, 130, 193, maxColorValue=255)
  farbeaceLt<-"#d45e4c40"
  farbeWT<-"#6e7e9740"
  
  #dat<-read.table("ggEnbase.csv",sep="\t",header=T,as.is=T, dec=",")
  min1<-OD-Stabw
  max1<-dat$OD+data$Stabw
  #min2<-dat$X6051-data$SW2
  #max2<-dat$X6051+data$SW2
  p<-ggplot(data = dat, aes(x =Zeit, y = OD))
  p<-p+geom_line(aes(y=OD,colour=Stamm))
  p<-p+geom_point(aes(y=OD,colour=Stamm))+coord_trans(y = "log")
  p<-p+facet_grid(~ Stamm)
  
  p<-p+geom_ribbon(aes(ymin=OD-Stabw, ymax=OD+Stabw)) +coord_trans(y = "log")
  p
  p<-p+geom_line(aes(y = X6051, colour="Blue"))+geom_ribbon(aes(x=Zeit,y = X6051, ymin=min2, ymax=max2)) + coord_trans(y = "log")
  p
  ggsave(filename)
}