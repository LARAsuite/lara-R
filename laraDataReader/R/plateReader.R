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
# VERSION: 0.1.0
#
# CREATION_DATE: 2015/04/27
# LASTMODIFICATION_DATE: 2015/10/10
#
# BRIEF_DESCRIPTION: Library for reading microtiter plate reader files 
# DETAILED_DESCRIPTION: currently supported devices BMG omega and Thermo Varioskan
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


#' LA_ImportData.varioskan
#'
#' @title Thermo Varioskan file reader
#' @description This function requires a table/list Varioskan  output file starting 
#'              from the keyword "Photometric1" with the following columns:
#'              'Barcode', 'Well', 'Type', 'Description', 'SampleNo', 'Value', 'Time', 'Wavelength', 'Read'.
#' @param filename (string) - single varioskan outputfile of kinetic data
#' @param barcode="000000" (string) - for defining a new barcode
#' @return data.frame
#' @keywords plate readers, Thermo Varioskan
#' @export 
#' 
#' @note todo - adding meta info (temp....), spectrum read: assigning start and end wavelength

LA_ImportData.varioskan <- function(filename="", deltaTime=32.0, addTime=TRUE, coords=FALSE, read=FALSE, ...)
{
  if(isTRUE(coords)) printDebug("LA_importData v.0.0.9: reading Varioskan %s file with coordinates: %s", current_method, filename_list[1]) else
    printDebug("LA_importData v.0.0.9a: reading Varioskan %s file: %s \nWarning: reading possibly multiple files", current_method, filename_list[1])
 
  table_offset = 2  # offset between the keyword 'Photometric1' and the begin of the data table
 
  switch(current_method,
         SPabs={  table_start_string <- 'Photometric1\t\t'
                  # column names for output data frame
                  if ( isTRUE(coords)) col_names <- c('Barcode', 'Well', 'Type', 'Description', 'SampleNo', 
                                                      'Value', 'Duration', 'Wavelength', 'C1', 'C2', 'C3') 
                  if (isTRUE(read))    col_names <- c('Barcode', 'Well', 'Type', 'Description', 'SampleNo', 
                                                               'Value', 'Duration', 'Wavelength', 'Read') else
                                       col_names <- c('Barcode', 'Well', 'Type', 'Description', 'SampleNo', 
                                                                                'Value', 'Duration', 'Wavelength')
         },
         SPECabs={  table_start_string <- 'Pho-Scanning1\t\t'
                  # column names for output data frame          
                    col_names <- c('Barcode', 'Well', 'Type', 'SampleNo', 'Sample', 'Value', 'Duration', 'Wavelength')
         },
         KINabs={ table_start_string <- 'Photometric1\t\t'
                  # column names for output data frame
                  if (isTRUE(coords)) col_names <- c('Barcode', 'Well', 'Type', 'Description', 'SampleNo', 
                                                    'Value', 'Duration', 'Wavelength', 'C1', 'C2', 'C3') else
                                      col_names <- c('Barcode', 'Well', 'Type', 'Description', 'SampleNo', 
                                                                'Value', 'Duration', 'Wavelength', 'Read')},
         KINabsMatr={ table_offset <- 14
                      inter_table_offset <- 22
                   table_start_string <- '^Photometric1\t\t'
                  # column names for output data frame
                  col_names <- c('Barcode', 'Well', 'Type', 'Description', 'SampleNo', 
                                                                      'Value', 'Duration', 'Wavelength', 'Read')},
         SPfl={table_start_string <- 'Fluorometric1\t\t'
               col_names <- c('Barcode', 'Well', 'Type', 'Description', 'SampleNo', 'Value', 'Duration', 'ExWL', 'Wavelength') },
         { cat(current_method, " - method not found Varioskan device\n"); return(help_info); }
  )
  
  readAllData <- function(filename)
  {  
    # reading input file only once
    data_file <- readLines(filename,encoding= "UTF-8")
    
    # retrieving date - mind the time zone information !
    raw_date <- sub("(\\s*Run started\\s*)(.*)([+]\\d*:\\d*)", "\\2", grep('Run started\\s*\\d*.*',data_file, value=TRUE))
    # %I hours in 1-12, %p AM/PM
    date_time <- strptime(raw_date, format = "%m/%d/%Y %H:%M:%S", tz = "CET")
    if(is.na(date_time)) {printError("ERROR (LA_ImportData.varioskan): wrong date or time format in file %s. Please use mm/dd/YYYY HH:MM:SS !", filename)}
    
    #temperature <-   grep('Actual instrument temperature',data_file, value=TRUE) 
    temperature <- as.numeric(sub("(\\s*Actual instrument temperature\\s*.MIN\\s*)(\\d*.\\d*)(\\s*)(\\d*)(\\s*)", "\\4",  grep('Actual instrument temperature',data_file, value=TRUE) ))

    if ( current_method == "SPECabs" ) {
      printError("WARNING (LA_ImportData.varioskan): assign start and end wavelengths from spectrum file")
      num_wavelengths <- ((wavelength_end - wavelength_start) / wavelength_step_size )
      num_readings <<- 1
      printDebug("spectral read varioskan num Wls: %s", num_wavelengths)
    } else {
      # detecting number of wavelengths
      num_wavelengths <<- length(grep("Wavelength \\[nm\\]", data_file))
      if(current_method=="SPfl") num_wavelengths <<- 1
      # detecting number of readings
      num_readings <<- as.numeric(sub("(\\s*Readings\\s*)(\\d*)(.*)", "\\2", grep('^\\s*Readings\\s*\\d*\\s*',data_file, value=TRUE) ))
      #print(num_readings)
    }
    if ( length(num_readings)==0 ) num_readings <<- 1
      
    # finding start of data
    table_start_row <- grep(table_start_string, data_file)[2] + table_offset 
    
    #printDebug("nr: %s , nWL %s, currsam:%s", num_readings, num_wavelengths,  current_sample_num)

    if ( current_method == "KINabsMatr" ) { 
      # raw_tab = data.frame() raw_tab[[i+1]] col.names=as.character(0:12),
      
      raw_tab <-lapply( (0:(num_readings-1)), function(i) { read.table(textConnection(data_file),  header=FALSE,
                                                                        skip=table_start_row + i*inter_table_offset, 
                                                                        nrows=current_layout_geometry[1], row.names=1, dec=".")  } )
      raw_tab_df = stack(unlist(raw_tab))
      raw_tab_df$ind <- NULL    # removing obsolete ind column
      raw_tab_df$Well = genWellNumbers()

      names(raw_tab_df) <- c("Value","Well") # make a nice name
      raw_tab_df$Duration <- deltaTime * rep( (0:(num_readings-1)), each=current_sample_num)
      raw_tab_df$DateTime <- raw_tab_df$Duration  + date_time 
            
      raw_tab_df$Wavelength <- as.numeric(sub("(^\\s*Wavelength\\s*\\[nm\\]\\s*)(\\d*)(.*)", "\\2", grep('^\\s*Wavelength\\s*',data_file, value=TRUE) ))

      if( current_barcode == "000000" )  printError("ERROR (LA_ImportData.varioskan): no barcode specified, please add a barcode to the file or specify a barcode as parameter!")
      raw_tab_df$Barcode <- as.factor(current_barcode)
      
    } else {
      printDebug("num reads:%s; num wl:%s; curr samp no:%s ", num_readings , num_wavelengths, current_sample_num)
      raw_tab_df <- read.table(textConnection(data_file), skip = table_start_row, header=FALSE, col.names=col_names, stringsAsFactors=TRUE,
                               nrows=num_readings * num_wavelengths * current_sample_num)
      if(addTime) raw_tab_df$DateTime <- raw_tab_df$Duration + date_time  else raw_tab_df$DateTime <- date_time 
      # assigning barcode to barcode from file or from input and converting Barcode colum to factors
      if( current_barcode == "000000" ) raw_tab_df$Barcode <- as.factor(raw_tab_df$Barcode) else raw_tab_df$Barcode <- as.factor(current_barcode)
    }
    
    # adding num for number of measurements 
    raw_tab_df$NumReads <- as.factor(file_num)
    
    # merge(aggregate(value ~ code, dat, min), dat, by = c("code", "value"))
    #gr_df <- merge(aggregate(DateTime~Wavelength, growth_df, rank ), growth_df, by = c("Wavelength",DateTime") )
    
    #raw_tab_df$RankTime <- rank(raw_tab_df$DateTime, ties.method="first")
    
    #raw_tab_df$RankTime <- 0
    # this is required to introduce a time independent time ranking for each maesurement
    #invisible(sapply(unique(raw_tab_df$Wavelength), function(x) { raw_tab_df[raw_tab_df$Wavelength == x,]$RankTime <<- rank(raw_tab_df[raw_tab_df$Wavelength == x,]$DateTime, ties.method="first")} ))
    
    raw_tab_df$Temperature <- temperature
    file_num <<- file_num + 1
    return(raw_tab_df)
  }
  file_num <- 1  # needs to be adjusted, if kinetics come from one single file
  full_abs_df <- do.call(rbind, lapply(filename_list, readAllData ))
  
  # determining default class, e.g. for plotting
  if( (file_num > 1) || (num_readings > 1) ) defaultClass = "kinetics" 

  date_time_ref = min(full_abs_df$DateTime)
  #print(head(full_abs_df, 12))
  #print(tail(full_abs_df, 12))
  #print(date_time_ref)
  full_abs_df$DiffTime <- difftime(full_abs_df$DateTime, date_time_ref , units=current_time_unit)
  
  output_df <- full_abs_df[order(full_abs_df$Well, full_abs_df$DiffTime),]
  
  #current_layout_geometry[1]*current_layout_geometry[2]
  #output_df$RankTime = rep((1:16), each=num_wavelengths)
  #growth_df[growth_df$Wavelength == 600,]$RankTime <<- rank(growth_df[growth_df$Wavelength == 600,]$DateTime)
  
  # adding Time Ranking for inter plate plotting
  #output_df$RankTime <- 1
  #sapply(unique(output_df$Wavelength), function(x) { output_df[output_df$Wavelength == x,]$RankTime <<- rank(output_df[output_df$Wavelength == x,]$DateTime, ties.method ="first")} )
  
  # start time of first measurement (this can be used to select the latest plate measured)
  output_df$StartTime <- 1 
  output_df$StartTime <- output_df[output_df$Well=="A01",]$DateTime[1]
  
  output_df$RefTime <- 1 #as.difftime(tim=1, units="secs")
  output_df$RefTime <- output_df[output_df$Well=="A01",]$DateTime # as.difftime(tim=output_df[output_df$Well=="A01",]$DiffTime, units="secs") #output_df[output_df$Well=="A01",]$DiffTime
  output_df$RefDiffTime <- output_df[output_df$Well=="A01",]$DiffTime

  output_df$UID <- 1
  output_df$UID <- output_df[output_df$Well=="A01",]$DateTime[1]
  
  if (isTRUE(coords)) {
    # removing coord columns
    output_df$C1 <- NULL
    output_df$C2 <- NULL
    output_df$C3 <- NULL
  }
  
  # adding plate layout information
  if ( isTRUE(load_layout) ) {
    output_df <- addPlateLayout(reader_df=output_df, barcode=current_barcode, 
                                useDBlayout=load_layout_from_db )
    if(identical(output_df, FALSE) ) return(FALSE)
  } else {
    # adding separate coordinates
    output_df$RowNum = as.factor(rep(LETTERS[1:current_layout_geometry[1]], each=current_layout_geometry[2]*num_readings*num_wavelengths))
    output_df$ColNum = as.factor(rep(rep(1:current_layout_geometry[2]), each=num_readings*num_wavelengths))
  }
  
  # optical pathlength correction
  if (isTRUE(pathlength_correction) & isTRUE(load_layout) ) {
    output_df$RawValue <- output_df$Value
    output_df$Value <- output_df$Value * pathlengthCorrectionFactor( liquidVolume=output_df$Volume )
  }
  # returning data frame ordered by well names
  return(output_df)
}

#' LA_ImportData.omega
#'
#' @title BMG omega - file reader
#' @description This function requires a table/list omega output file starting 
#'              from the keyword "Chromatic" with the following columns:
#'              
#' @param filename (string) - single varioskan outputfile of kinetic data
#' @param barcode="000000" (string) - for defining a new barcode
#' @return data.frame
#' @keywords plate readers, BMG omega
#' @export 
#' 
#' @note todo - storing meta info 

LA_ImportData.omega <- function(filename, coords=FALSE, ...)
{
  printDebug("plateReader - omegaSPabs: reading Omega SPabs - table format. ToDo: do.call s.o.")
  
  readAllData <- function(filename)
  {  
    table_offset = 2  # offset between the keyword 'Chromatic' and the begin of the data table
    #reading file only once  ?collapse=" ",
    data_file <- readLines(filename, encoding= "UTF-8")
    # reading time
    raw_date <- grep('Date:',data_file, value=TRUE)
    # %I hours in 1-12, %p AM/PM
    date_time <- strptime(raw_date, format = "Date: %Y/%m/%d Time: %I:%M:%S %p", tz = "CET")
    # reading barcode of plate
    barcode <- sub("(ID1:\\s*)(\\d*)(\\s*ID2:\\s)([^ID3:]+)(ID3:)(.*)", "\\2", grep('ID1:.*ID2:', data_file, value=TRUE) )
    # finding wavelengths    
    wavelengths <- as.numeric(sub("(\\s*\\d:\\s*)(\\d*)(nm\\s*)(.*)", "\\2",  grep('\\d{3}nm',data_file, value=TRUE) ))
    # finding temperature    
    temperature <- as.numeric(sub("(T\\[.C\\]:\\s*)(\\d*)(.*)", "\\2",  grep('T\\[.C\\]:',data_file, value=TRUE) ))
    # finding all data tables
    abs_table_start_rows <- grep('Chromatic', data_file)
    
    readMultipleWavelengthData <- function(wavelength)
    {
      # reading absorption data
      table_start_row <- abs_table_start_rows[i] + table_offset
      col_names <- c('Well', 'Value')
      raw_tab <- read.table(textConnection(data_file), skip = table_start_row, header=TRUE, stringsAsFactors=TRUE,
                            col.names=col_names, nrows=current_sample_num)  
      
      raw_tab$Barcode <- as.factor(barcode)
      raw_tab$Num <- as.factor(file_num)
      raw_tab$DateTime <- date_time
      raw_tab$Type <- as.factor("S")
      raw_tab$Wavelength <- as.factor(wavelength)
      raw_tab$Temperature <- temperature[i]
      
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
  full_abs_df$DiffTime <-  difftime(full_abs_df$DateTime, date_time_ref , units=current_time_unit)
  
  output_df <- full_abs_df[order(full_abs_df$Well, full_abs_df$DiffTime),]
  
  output_df$RefTime <- as.difftime(tim=1, units="secs")
  output_df$RefTime <- output_df[output_df$Well=="A01",]$DiffTime
  
  # start time of first measurement (this can be used to select the latest plate measured)
  output_df$StartTime <- 1 
  output_df$StartTime <- output_df[output_df$Well=="A01",]$DateTime[1]
  
  # UID to uniquely identify a plate (currently date is used, later hash could be added)
  output_df$UID <- 1
  output_df$UID <- output_df[output_df$Well=="A01",]$DateTime[1]
  
  if ( ! isTRUE(load_layout) ) {
    # adding separate coordinates
#     num_wavelengths <- length(wavelengths)
#     num_readings <- file_num * current_sample_num
#     output_df$Rown = as.factor(rep(LETTERS[1:8], each=12*num_readings*num_wavelengths))
#     output_df$Coln = as.factor(rep(rep(1:12), each=num_readings*num_wavelengths))
  }
  
  # adding plate layout information
  if ( isTRUE(load_layout) ) {
    output_df <- addPlateLayout(reader_df=output_df, barcode=current_barcode, 
                                useDBlayout=load_layout_from_db )
    if(identical(output_df, FALSE)) return(FALSE)
  }
  
  # optical pathlength correction
  if (isTRUE(pathlength_correction) & isTRUE(load_layout)) {
    output_df$RawValue <- output_df$Value
    output_df$Value <- output_df$Value * pathlengthCorrectionFactor( liquidVolume=output_df$Volume )
  }
  
  return(output_df)
}

#' omegaSPabsMatr
#'
#' @title BMG Omega: Singlepoint absorption measurement with multiple wavelengths - old file formats (matrix)
#' @description This function can be used for matrix type BMG omega output data
#' @param filename_list (string) - list of filenames to import
#' @return data.frame 
#' @keywords plate readers, BMG omega
#' @export 
#' @note todo: temperature, shaking, num flashes
#'   

omegaSPabsMatr <- function(filename_list)
{
  printDebug("reading Omega SPabs - matrix format : ", filename_list)
  
  read_all_data <- function(filename)
  {  
    table_offset = 4  # offset between the keyword 'Chromatic' and the begin of the data table
    
    #reading file only once  ?collapse=" ",
    data_file <- readLines(filename,encoding= "UTF-8")
    # reading time
    raw_date <- grep('Date:',data_file, value=TRUE)
    # %I hours in 1-12, %p AM/PM
    date_time <- strptime(raw_date, format = "Date: %Y/%m/%d Time: %I:%M:%S %p", tz = "CET")
    # reading barcode of plate  'ID1:.*ID2:'
    barcode <- strsplit(grep('ID1:.*', data_file, value=TRUE), " ")[[1]][2]
    # finding wavelengths
    wavelength_lines <- grep('\\d{3}nm',data_file, value=TRUE)
    # removing "nm" and gain values  from wavelengths
    wavelengths <- sub("nm.*", "", sub(".*: ", "",  wavelength_lines))
    # finding all data tables
    abs_table_start_rows <- grep('Chromatic', data_file)
    
    readMultipleWavelengthData <- function(wavelength)
    {
      # reading absorption data
      table_start_row <- abs_table_start_rows[i] + table_offset
      
      temp_tab <- read.table(textConnection(data_file), skip = table_start_row, header=FALSE, row.names=1, nrows=8)  
      
      raw_tab <- data.frame('Value'=c(apply(temp_tab, 1, function(x) x )))
      # transform data frame
      
      raw_tab$Well <- genWellNumbers(byRows=T)
      raw_tab$Barcode <- as.factor(barcode)
      raw_tab$Num <- as.factor(file_num)
      raw_tab$DateTime <- date_time
      raw_tab$Type <- as.factor("sample")
      raw_tab$Wavelength <- wavelength 
      
      i <<- i+1 
      full_abs_df <<- rbind(full_abs_df,raw_tab)
    }
    # iterating reading through all wavelength tables
    i <- 1  # "static" variable for iterating through table starts
    abs_data <- sapply(wavelengths,readMultipleWavelengthData )
    file_num <<- file_num + 1
  } 
  
  # iterating through all files in the file list
  file_num <- 1  # counter for multiple file reads 
  full_abs_df <- NULL
  sapply(filename_list, read_all_data )
  
  return(full_abs_df[order(full_abs_df$Num),])
}


#' LA_ImportData.tecanMagellanCSV
#'
#' @title TECAN
#' @description This function requires a table/list TECAN Magellan CSV  output file starting 
#'              from the keyword "Raw data" with the following columns:
#'              
#' @param filename (string) - TECAN Magellan MSExcel outputfile converted to CSV
#' @return data.frame
#' @keywords plate readers, Thermo Varioskan
#' @export 
#' 
#' @note todo - storing all meta info

LA_ImportData.tecanMagellanCSV <- function(filename, ...)
{
  printDebug("LA_ImportData.tecanCSV: Reading TECAN Magellan csv file: %s", filename_list[1])
  
  readAllData <- function(filename)
  { 
    #reading file only once
    data_file <- readLines(filename, encoding= "UTF-8")
    # reading time
    raw_date_time <- grep('Date of measurement:', data_file, value=TRUE)
    
    raw_date <- sub("(\\s*Date of measurement:\\s*)([0-9-]+)(/.*)", "\\2", raw_date_time )
    raw_dt_pattern <- sub(";", current_sep, "(\\s*Date of measurement:\\s*)([0-9-]+)(/Time of measurement:\\s*)([0-9:]+)(;*)")
    raw_dt <- sub(raw_dt_pattern, "\\2 \\4", raw_date_time )
    date_time <- strptime(raw_dt, format = "%Y-%m-%d %H:%M:%S", tz = "CET")
    
    #num_reads <- as.numeric(sub("(\\s*Number of Reads:\\s*)(\\d*)(,*)", "\\2", grep('Number of Reads:', data_file, value=TRUE) ))

    barcode_pattern <- sub(";", current_sep, "(\\s*Barcode:\\s*)([No]+|\\d*)(\\s*;*)")
    barcode <- sub(barcode_pattern, "\\2", grep('Barcode:', data_file, value=TRUE) )
    if (barcode == 'No') {
      printError("ERROR (LA_ImportData.tecanMagellanCSV): no barcode specified, please add a barcode to the file or specify a barcode as parameter!")
      return(FALSE)
    } 

    wavelength_pattern <- sub(";", current_sep, "(\\s*Measurement Wavelength:\\s*)(\\d*)(\\s*nm;*)" )
    wavelength <- as.numeric(sub(wavelength_pattern, "\\2", grep('Measurement Wavelength:', data_file, value=TRUE) ))
    
    abs_table_start_row <- as.numeric(grep('Raw data', data_file, value=FALSE)[1])
    abs_table_end_row <- as.numeric(grep('Date of measurement:', data_file, value=FALSE))
    
    num_reads <- abs_table_end_row - abs_table_start_row -2 # one less because of table head

    raw_tab <<- read.table(textConnection(data_file), skip = abs_table_start_row, header=TRUE,
                           nrows=num_reads, sep=current_sep, dec=".") 
    
    time_vec <- as.numeric(sub("s", "", raw_tab$X))
    temp_vec <- as.numeric(sub("\\s*°C", "", raw_tab$X.1))
    
    # removing obsolete columns
    raw_tab$X <- NULL
    raw_tab$X.1 <- NULL
    
    # add nice head to the data frame
    names(raw_tab) <- genWellNumbers()
    # stack all data in one column
    stacked_df <- stack(raw_tab)
    # assign a new name
    names(stacked_df) <- c("Value","Well") 
    # add the times 
    stacked_df$DiffTime <- time_vec
    
    stacked_df$Barcode <- as.factor(barcode)
    stacked_df$Wavelength <- wavelength
    # add the DateTime
    stacked_df$DateTime <- date_time #  stacked_df$Time #+ date_time 
    # add Temperatur info
    stacked_df$Temperature <- temp_vec
    
    temp_df <<- stacked_df
  }
    # iterating through all files in the file list
    file_num <- 1
    temp_df <- NULL
    sapply(filename_list, readAllData )
  
  output_df <- temp_df[order(temp_df$Well, temp_df$DiffTime),]
  
  output_df$RefTime <- as.difftime(tim=1, units="secs")
  output_df$RefTime <- output_df[output_df$Well=="A01",]$DiffTime
  
  # start time of first measurement (this can be used to select the latest plate measured)
  output_df$StartTime <- 1 
  output_df$StartTime <- output_df[output_df$Well=="A01",]$DateTime[1]
  
  # UID to uniquely identify a plate (currently date is used, later hash could be added)
  output_df$UID <- 1
  output_df$UID <- output_df[output_df$Well=="A01",]$DateTime[1]
  
  # adding plate layout information
  if ( isTRUE(load_layout) ) {
    output_df <- addPlateLayout(reader_df=output_df, barcode=current_barcode, 
                                useDBlayout=load_layout_from_db )
    if(identical(output_df, FALSE)) return(FALSE)
  }

  # optical pathlength correction
  if (isTRUE(pathlength_correction) & isTRUE(load_layout)) {
    output_df$RawValue <- output_df$Value
    output_df$Value <- output_df$Value * pathlengthCorrectionFactor( liquidVolume=output_df$Volume )
  }
  return(output_df)
}

#' LA_ImportData.tecanMagellanXLSX
#'
#' @title TECAN Magellan MS Excel xlxs reading module
#' @description This function requires a table/list TECAN Magellan XLSX output file starting 
#'              from the keyword "Photometric1" with the following columns:
#'              
#' @param filename (string) - TECAN Magellan MSExcel xlxs file name
#' @return data.frame
#' @keywords plate readers, TECAN
#' @export 
#' 
#' @note todo - storing all meta info

LA_ImportData.tecanMagellanXLSX <- function(filename, ...)
{
  printDebug("LA_ImportData.tecanXLSX: reading TECAN Magellan MSExcel xlsx file: %s", filename_list[1])
  printDebug("WARNING (LA_ImportData.tecanMagellanXLSX): file mata info not yet added")
  
  require("readxl")
  
  readAllData <- function(filename)
  { 
    table_offset <- 2    
    # reading MS Excel xlsx file  
    #filename = "20150603_DAAOassay_20150427_20150601racStability_30C.xlsx"
    raw_xlsx <- read_excel(filename, skip=table_offset) # Confirm it is the right Magellan layout!!
    meta_info_vec <<- raw_xlsx[,1]
    
    raw_xlsx <- na.omit(raw_xlsx) # removeing all NAs ! might give trouble when resaved with OpenOffice Calc
    num_reads <- nrow(raw_xlsx)
    
    time_vec <- as.numeric(sub("s", "", raw_xlsx[,1]))
    temp_vec <- as.numeric(sub("\\s*°C", "", raw_xlsx[,2]))
    
    # removing first and second column
    raw_xlsx[,2] <- NULL
    raw_xlsx[,1] <- NULL
    
    # adding missing empty columns
    invisible(lapply(genWellNumbers(padding=0, asVector=TRUE), 
                     function(col_name){ if ( ! col_name %in% names(raw_xlsx) ) { raw_xlsx[,col_name] <<- NA  } } ) )
    # stacking all the data in one column
    stacked_df <- stack(raw_xlsx)
    # adding nice column names
    names(stacked_df) <- c("Value","Well") 
    
    # reformat Well names to padding with 0 (required for other processes)
    stacked_df$Well <- sub(" ", "", 
                           sub("([A-H])([1-9])(.*)", "\\10\\2",  
                             sub("([A-H])([1])([0-9])", "\\1 \\2\\3", stacked_df$Well)))
    
    stacked_df$Well <- as.factor(stacked_df$Well)

    # assigning time (relative and absolute times)
    stacked_df$DiffTime <- time_vec # relative time (in seconds)
    
    # reading start time of measurement
    raw_date_time <- grep('Date of measurement:', meta_info_vec, value=TRUE)
    # extracting the date (as string)
    raw_date <- sub("(\\s*Date of measurement:\\s*)([0-9-]+)(/.*)", "\\2", raw_date_time )
   
    raw_dt <- sub("(\\s*Date of measurement:\\s*)([0-9-]+)(/Time of measurement:\\s*)([0-9:]+)(.*)", "\\2 \\4", raw_date_time )
    date_time <- strptime(raw_dt, format = "%Y-%m-%d %H:%M:%S", tz = "CET")

    # add the DateTime (absolute time)
    stacked_df$DateTime <-  stacked_df$DiffTime + date_time
    
    wavelength <- as.numeric(sub("(\\s*Measurement Wavelength:\\s*)(\\d*)(\\s*nm.*)", "\\2", 
                                 grep('Measurement Wavelength:', meta_info_vec, value=TRUE) ))
    stacked_df$Wavelength <- wavelength
    
    if ( current_barcode == "000000" )  barcode <- sub("(\\s*Barcode:\\s*)([No]+|\\d*)(\\s*)", "\\2", grep('Barcode:', meta_info_vec, value=TRUE) ) else
      barcode = current_barcode
    if (barcode == 'No' | barcode =="000000" ) {
      printError("ERROR (LA_ImportData.tecanMagellanXLSX): no barcode specified, please add a barcode to the file or specify a barcode as parameter!")
      return(FALSE)
    }

    stacked_df$Barcode <- as.factor(barcode)

    # add Temperatur info
    stacked_df$Temperature <- temp_vec
    
    temp_df <<- stacked_df
  }
  # iterating through all files in the file list
  file_num <- 1
  temp_df <- NULL
  sapply(filename_list, readAllData )
  
  output_df <- temp_df 
  
  printDebug("WARNING (LA_ImportData.tecanMagellanXLSX): no sorting by date and well !")
  #output_df <- temp_df[order(temp_df$DiffTime, temp_df$Well),]
  
  output_df$RefTime <- as.difftime(tim=1, units="secs")
  output_df$RefTime <- output_df[output_df$Well=="A01",]$DiffTime

  # start time of first measurement (this can be used to select the latest plate measured)
  output_df$StartTime <- 1 
  output_df$StartTime <- output_df[output_df$Well=="A01",]$DateTime[1]
  
  output_df$UID <- 1
  output_df$UID <- output_df[output_df$Well=="A01",]$DateTime[1]
  
  # adding plate layout information
  if ( isTRUE(load_layout) ) {
    output_df <- addPlateLayout(reader_df=output_df, barcode=current_barcode, 
                                useDBlayout=load_layout_from_db )
    if(identical(output_df, FALSE)) return(FALSE)
  }
  
  # optical pathlength correction
  if (isTRUE(pathlength_correction) & isTRUE(load_layout)) {
    output_df$RawValue <- output_df$Value
    output_df$Value <- output_df$Value * pathlengthCorrectionFactor( liquidVolume=output_df$Volume )
  }
  
  return(output_df)
}
