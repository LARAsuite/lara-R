#_____________________________________________________________________________
#
# PROJECT: LARA
# CLASS: 
# FILENAME: plateReader.R
#
# CATEGORY:
#
# AUTHOR: mark doerr
# EMAIL: mark@ismeralda.org
#
# VERSION: 0.0.4
#
# CREATION_DATE: 2015/06/01
# LASTMODIFICATION_DATE: 2015/08/01
#
# BRIEF_DESCRIPTION: Library for reading liquid chromatography data
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


#' LA_ImportData.Hitachi
#'
#' @title R script for reaing HPLC data of Hitachi/VWR HPLC
#' @description This function requires a table/list Varioskan  output file starting 
#'              from the keyword "Photometric1" with the following columns:
#'              'Barcode', 'Well', 'Type', 'Description', 'SampleNo', 'Value', 'Time', 'Wavelength', 'Read'
#'              
#' @param filename (string) - single varioskan outputfile of kinetic data
#' @param barcode="0000" (string) - for defining a new barcode
#' @return data.frame
#' @keywords plate readers, Thermo Varioskan
#' @export 
#' 
#' @note todo - storing meta info 

LA_ImportData.Hitachi <- function(filename, ...)
{
  printDebug("LCReader - Hitachi HPLCabs: reading HPLCabs - table format")
  
  # positions of constants
  num_data_points_row <- 12-1
  x_axis_mulitplier_row <- 15-1
  y_axis_mulitplier_row <- 16-1
  table_start_row <- 16

  num_data_points  = read.table(filename,skip = num_data_points_row, nrows=1, sep=",")[[2]]
  x_axis_mulitplier = read.table(filename,skip = x_axis_mulitplier_row, nrows=1,  dec=",", sep=",")[[3]] * 1E-06
  y_axis_mulitplier = read.table(filename,skip = y_axis_mulitplier_row, nrows=1,  dec=",", sep=",")[[3]] * 1E-05
  
  data_l <- read.table(filename,skip = table_start_row, nrows=num_data_points, blank.lines.skip = TRUE)
  absorption_l = data_l[[1]] * y_axis_mulitplier
  absorption <- as.vector(unlist(absorption_l))
  
  # generating time vector with min as unit 
  time_f <- seq( 1, num_data_points ) / 300 # factor 1/300 for converting 200µs steps in min  
  time <- round(time_f, digits=2)
}

#' LA_ImportData.MerckLaChrom
#'
#' @title MerckLaChrom HPLC data import
#' @description This function requires a table/list Varioskan  output file starting 
#'              from the keyword "Photometric1" with the following columns:
#'              'Barcode', 'Well', 'Type', 'Description', 'SampleNo', 'Value', 'Time', 'Wavelength', 'Read'
#'              
#' @param filename (string) - single varioskan outputfile of kinetic data
#' @param barcode="0000" (string) - for defining a new barcode
#' @return data.frame
#' @keywords plate readers, Thermo Varioskan
#' @export 
#' 
#' @note todo - storing meta info 

LA_ImportData.MerckLaChrom <- function(filename, ...)
{
  printDebug("LCReader - MerckLaChrom HPLCabs: reading HPLCabs - table format")
  
  readAllData <- function(filename)
  {  
    table_offset = 0  # offset between the keyword 'Chromatic' and the begin of the data table
    #reading file only once  ?collapse=" ",
    data_file <- readLines(filename, encoding= "UTF-8")
    # reading time
    raw_date_pos <- grep("[Date Analyzed]",data_file, value=F)[1] + 1
    # %I hours in 1-12, %p AM/PM
    date_time <- Sys.time() # now strptime(raw_date, format="\"%m.%d.%y %I:%M\";", tz="CET")
    # reading barcode of plate
    
    #barcode <- sub("(ID1:\\s*)(\\d*)(\\s*ID2:\\s)([^ID3:]+)(ID3:)(.*)", "\\2", grep('ID1:.*ID2:', data_file, value=TRUE) )
    # finding wavelengths    
    #wavelengths <- as.numeric(sub("(\\s*\\d:\\s*)(\\d*)(nm\\s*)(.*)", "\\2",  grep('\\d{3}nm',data_file, value=TRUE) ))
    wavelengths = c(wavelength)
    # finding all data tables
    abs_table_start_rows <- grep('Chromatogram Data', data_file, value=F)
    print(abs_table_start_rows)
    
    readMultipleWavelengthData <- function(wavelength)
    {
      # reading absorption data
      table_start_row <- abs_table_start_rows[i] + table_offset

      print(table_start_row)
      col_names <- c('Time', 'Value', 'Empty')
      raw_tab <- read.table(textConnection(data_file), skip = table_start_row, header=FALSE, stringsAsFactors=TRUE,
                            col.names=col_names, nrows=current_sample_num, dec=",", sep=";")  
      
      raw_tab$Barcode <- as.factor(barcode)
      raw_tab$Num <- as.factor(file_num)
      raw_tab$DateTime <- date_time
      raw_tab$Type <- as.factor("S")
      raw_tab$Wavelength <- as.factor(wavelength) 
      
      i <<- i+1 
      full_abs_df <<- rbind(full_abs_df,raw_tab)
    }
    # iterating reading through all wavelength tables
    i <- 1  # "static" variable for iterating through table starts
    abs_data <- sapply(wavelengths, readMultipleWavelengthData )
    file_num <<- file_num + 1
  }
  
  # iterating through all files in the file list
  file_num <- 1
  full_abs_df <- NULL
  sapply(filename_list, readAllData )
  
  date_time_ref = min(full_abs_df$DateTime)
  #full_abs_df$DiffTime <-  full_abs_df$DateTime + full_abs_df$Time  # difftime(full_abs_df$DateTime, date_time_ref , units=current_unit)
  
  output_df <- full_abs_df #[order(full_abs_df$Well, full_abs_df$DiffTime),]
  
  return(output_df)
}
