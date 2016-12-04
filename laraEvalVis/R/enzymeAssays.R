#_____________________________________________________________________________
#
# PROJECT: LARA
# CLASS: 
# FILENAME: enzymeAssays.R
#
# CATEGORY:
#
# AUTHOR: mark doerr
# EMAIL: mark@ismeralda.org
#
# VERSION: 0.0.9
#
# CREATION_DATE: 2015/08/02
# LASTMODIFICATION_DATE: 2015/09/02
#
# BRIEF_DESCRIPTION: Library for handling enzyme assays
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

#' splitGFPAssay
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
  
splitGFPAssay <- function(assay_df=NULL, ouput=FALSE) 
{
  # colour handling
  col_yellow <- "#FFFF00"  # yellow   
  col_orange <- "#FFA500"  # orange
  col_red <- "#FF0000"     # red
  
  ## finding the yellow wells
  r_df <- assay_df[assay_df$Wavelength == 430, ]
  r_df$Value <- r_df$Value / assay_df[assay_df$Wavelength == 560, ]$Value
  
  r_df$Col <- col_red
  
  r_df$Col[r_df$Value > min(r_df[r_df$Well==c("C01","D01"),]$Value)*0.84 ] <- col_orange
  r_df$Col[r_df$Value > min(r_df[r_df$Well==c("C01","D01"),]$Value)*0.95 ] <- col_yellow
    
  ## evaluation of yellow wells
  yellow_wells_df <- r_df[r_df$Col==col_yellow, ]
  
  ydata <- kin_fl_df$Value
  xdata <-  as.numeric(kin_fl_df$DiffTime)
  
  x_start <- (1:length(ydata))
  
  fit_line_xdata = data.frame(xdata = seq(min(xdata),max(xdata), len=length(ydata)))
  
  poly_fit = lm(ydata ~  x_start + I(x_start^2) + I(x_start^3) )
  
  plotCI(y=ydata, x=xdata, uiw = kin_fl_df$Sd, pch = plot_char,   type="p", xlim=xlim, ylim=ylim,
         xlab="time / h", ylab="Fluorescence / RFU", col=lineColor)
  
  lines(fit_line_xdata$xdata, predict(poly_fit, newdata=fit_line_xdata), col='green')
  
  poly_fit = lm(ydata ~  x_start + I(x_start^2) + I(x_start^3) )
  
  filename <- "PhredAssay_yellow_wells_summary.csv"
  if(output) write.table(yellow_wells_df, file=filename,  append=TRUE, sep="," , row.names=FALSE)
  
  barcode <- r_df$Barcode[1]
  
  num_yellow <- nrow(yellow_wells_df)
  yellow_wells <- paste(yellow_wells_df$Well, collapse=";")
    
  plate_summary_df <- data.frame("Barcode"=barcode, "YellowWells"=yellow_wells, "YellowWellsNum"=num_yellow )
  filename <- paste(barcode, "_PhredAssay_per_plate_evaluation.csv", sep="")
  if(output) write.table(plate_summary_df, file=filename, sep=",", row.names=FALSE)
  
#   out_df <- NULL
#   calcRatios <- function(subset_df)
#   {
#     #ratio yellow
#     
#     print(subset_df)
#     # ratio 430 / 560
#     subset_df$WLRatio <- 1
#     
#     wl_ratio <- subset_df[subset_df$Wavelength==430,]$Value / subset_df[assay_df$Wavelength==480,]$Value 
#     
#     if ( wl_ratio[1] > 1.0) curr_col <- col_yellow else curr_col <- col_red
#     
#     temp_df <- data.frame("Well"=subset_df$Well[1], "WLRatio"=wl_ratio[1], "Col"=curr_col) 
#     
#     out_df <<- rbind(out_df, temp_df)
#     
#     #assay_df$Col[assay_df$Value > min(assay_df[c(49,61),]$Value)*0.84 ] <<- col_orange
#     #assay_df$Col[assay_df$Value > min(assay_df[c(49,61),]$Value)*0.95 ] <<- col_yellow
#   }
#   invisible(by(assay_df, assay_df$Well, calcRatios) )
  
  return(r_df)
}

#' phenolRedAssay
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

phenolRedAssay <- function(assay_df=NULL, intensityCoding=FALSE, limOrange=0.95, limYellow=0.8, output=FALSE) 
{
  # colour handling
  col_yellow <- "#FFFF00"  # yellow   
  col_orange <- "#FFA500"  # orange
  col_red <- "#FF0000"     # red
  
  ## finding the yellow wells
#   r_df <- assay_df[assay_df$Wavelength == 430, ]
#   r_df$Value <- r_df$Value / assay_df[assay_df$Wavelength == 560, ]$Value
#   
  # ratio  560 / 430
  r_df <- assay_df[assay_df$Wavelength == 560, ]
  r_df$Value <- r_df$Value / assay_df[assay_df$Wavelength == 430, ]$Value
  
  r_df$Col <- col_red

#   r_df$Col[r_df$Value > min(r_df[r_df$Well==c("C01","D01", "E01", "F01"),]$Value)*0.95 ] <- col_orange
#   r_df$Col[r_df$Value > max(r_df[r_df$Well==c("C01","D01", "E01", "F01"),]$Value)*1.00 ] <- col_yellow
#  
  r_df$Col[r_df$Value < limOrange ] <- col_orange
  r_df$Col[r_df$Value < limYellow  ] <- col_yellow
  
  ## evaluation of yellow wells
  yellow_wells_df <- r_df[r_df$Col==col_yellow, ]
  
  if(length(yellow_wells_df) > 0)
  {
    #fl_0533_ne_df  <- subset( fl_0533_df, ! ( fl_0533_df$Well  %in% ctrl_wells))

    yellow_wells_df <- yellow_wells_df[yellow_wells_df$Type != "pCTRL",]
    
    filename <- paste("../","PhRedAssay_yellow_wells_summary", limYellow,".csv", sep="")
    if(output) write.table(yellow_wells_df, file=filename,  append=TRUE, sep="," , row.names=FALSE)
    
    barcode <- r_df$Barcode[1]
    
    num_yellow <- nrow(yellow_wells_df)
    yellow_wells <- paste(yellow_wells_df$Well, collapse=";")
    
    plate_summary_df <- data.frame("Barcode"=barcode, "YellowWells"=yellow_wells, "YellowWellsNum"=num_yellow )
    #filename <- paste("../",barcode, "_PhredAssay_per_plate_evaluation.csv", sep="")
    filename <- paste("../","PhRedAssay_per_plate_evaluation", limYellow, ".csv", sep="")
    if(output) write.table(plate_summary_df, file=filename, sep=",",col.names=FALSE, row.names=FALSE, append=TRUE )
  }
   
  if (isTRUE(intensityCoding)) {
    num_col <- length(unique(r_df$Value))
    #printDebug("numcol: %s", num_col)
    #print("numcol:")
    #print(num_col)  
    intensity_colors <- colorRampPalette(c("yellow","orange", "darkorange", "orangered" ,"red"))(num_col)
    
    #print(intensity_colors)
    
    r_df$Col <- intensity_colors[floor(rank(r_df$Value))]
  }
    
  
  #   out_df <- NULL
  #   calcRatios <- function(subset_df)
  #   {
  #     #ratio yellow
  #     
  #     print(subset_df)
  #     # ratio 430 / 560
  #     subset_df$WLRatio <- 1
  #     
  #     wl_ratio <- subset_df[subset_df$Wavelength==430,]$Value / subset_df[assay_df$Wavelength==480,]$Value 
  #     
  #     if ( wl_ratio[1] > 1.0) curr_col <- col_yellow else curr_col <- col_red
  #     
  #     temp_df <- data.frame("Well"=subset_df$Well[1], "WLRatio"=wl_ratio[1], "Col"=curr_col) 
  #     
  #     out_df <<- rbind(out_df, temp_df)
  #     
  #     #assay_df$Col[assay_df$Value > min(assay_df[c(49,61),]$Value)*0.84 ] <<- col_orange
  #     #assay_df$Col[assay_df$Value > min(assay_df[c(49,61),]$Value)*0.95 ] <<- col_yellow
  #   }
  #   invisible(by(assay_df, assay_df$Well, calcRatios) )
  
  return(r_df)
}