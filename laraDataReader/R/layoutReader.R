#_____________________________________________________________________________
#
# PROJECT: LARA
# CLASS: 
# FILENAME: layoutReader.R
#
# CATEGORY:
#
# AUTHOR: mark doerr
# EMAIL: mark@ismeralda.org
#
# VERSION: 0.1.0
#
# CREATION_DATE: 2015/04/27
# LASTMODIFICATION_DATE: 161130
#
# BRIEF_DESCRIPTION: Library for reading microtiter plate layouts 
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

#' loadPlateLayout
#'
#' @title Reading Plate Layout Information from Plate Layout csv File for Microtiter Plates.
#' @description Reading plate layout information from plate layout csv file for any microtiter well plates.
#'              Plate geometry is defined in layout file (s. templates folder). Default geometry: 8x12.
#'              The function is looking for a file "0*"barcode"_plate_layout.*csv" in the given directory.
#'              Default substrate, cofactor and enzyme concentrations overwrite individual concentrations.
#' @param barcode="000000" - this is used for finding the layout file BC_plate_layout_*.csv
#'        barcodes can have a non-numeric prefix, valid barcodes are e.g. TA1001, GA_20, Tbe.0003, XY_abc|0404, WZ.def.4432, 
#'        the minus sign "-" is omitted as separator, since it is need to denote intervals: TA1001-TA1024  
#' @param padding=4 - overall number of barcode digits, leading digits are filled with zeros, will be soon changed to 6
#' @param dir="./" (string) - directory of layout file with leading / 
#' @param concUnit="uM"  - unit of concentration, valid values are 'uM' and 'mM' 
#' @param asList=TRUE (boolean) this returns not only the plate layout, but also the barcodes, description and plate geometry (= layout meta information)
#' @return data.frame containing Wells, Type and Description, Substrates, Cofactors and Enzymes as columns or List
#' @keywords plate readers layout microtiter plate
#' @export    

loadPlateLayout <- function(barcode="000000", dir="./", padding=4, concUnit="uM", asList=TRUE)
{
  printDebug(module="loadPlateLayout", "v0.1.0d")
  
  file_pattern = paste("0*", barcode, "_plate_layout.*csv$", sep="")

  layout_filename <- list.files(path = dir, pattern = file_pattern  , all.files = FALSE,
                                full.names = FALSE, recursive = FALSE,
                                ignore.case = FALSE, include.dirs = FALSE)[1] # choose first item in barcode file list
  
  #reading file only once
  if ( is.na(layout_filename) ) {printError(module="loadPlateLayout", "No layout with specified pattern (%s) found !", file_pattern) ; return(FALSE) } else {
    data_file <- readLines(layout_filename,encoding= "UTF-8") }
  printDebug(module="loadPlateLayout", "Layout file %s read", layout_filename)
  
  # "asList" returns a list of layout meta information, like barcode, matrix size, and layout
  if (isTRUE(asList)) {
    # 1. retrieving barcode or barcode interval
    layout_barcodes <- sub("(^#\\s*Barcodes\\s*:\\s*)([^,]+)(\\s*)(,*)", "\\2;", grep('Barcodes\\s*:',data_file, value=TRUE)) 
    
    bc_str_vec <- unlist(strsplit(layout_barcodes, split="(\\s*;\\s*)"))
    
    if (length(bc_str_vec) > 0L){  # it is possible to denote an barcode interval, e.g. 4510-4555, even with a prefix: HY0001-HY0022 
      checkBarcodeInterval <- function(bc_str){
        bc_interval <- unlist(strsplit(bc_str, split="(\\s*-\\s*)"))
        printDebug(module="loadPlateLayout", "barcode interval", bc_str,  bc_interval)
        
        prefix_pattern = "([:alpha:]*[.,|,_]*)(\\d+$)"  # valid barcodes are e.g. TA1001, GA_20, Tbe.0003, XY_abc|0404, WZ.def.4432
        if ( length(bc_interval) == 1) {
          
          bc_split = unlist(strsplit( sub(prefix_pattern, "\\1>\\2", bc_interval[1]),">"))
          return(sprintf(paste(bc_split[1],"%0", padding, "d", sep=""), as.numeric(bc_split[2])) ) 
        } else {
          
          bc_split_left = unlist(strsplit( sub(prefix_pattern, "\\1>\\2", bc_interval[1]),">"))
          bc_split_right = unlist(strsplit( sub(prefix_pattern, "\\1>\\2", bc_interval[2]),">"))
          
          bc_padded <- sapply(bc_split_left[2]:bc_split_right[2], 
                              function(x) sprintf(paste(bc_split_left[1],"%0", padding, "d", sep=""), as.numeric(x)) )
          return(bc_padded)
        }
      }
      all_barcodes_vec <- unlist(lapply(bc_str_vec, checkBarcodeInterval))
    } 
    # 2. retrieving description of layout
    description <- sub("(#\\s*Description\\s*:\\s*)([^,]+)(\\s*)(,*)", "\\2", grep('Description:',data_file, value=TRUE))
    
    # 3. retrieving concentration_unit 
    concUnit_infile <- sub("(#\\s*concentration_unit\\s*:\\s*)([u,m,M]+)(\\s*;.*)", "\\2", grep('concentration_unit\\s*:',data_file, value=TRUE))
    printDebug(module="loadPlateLayout", "unit %s", concUnit_infile)
    if( length(concUnit_infile) > 0 ){
      if( grep("(uM)|(mM)|(M)", concUnit_infile )  ) concUnit = concUnit_infile
      }
  }

  # 4. plate size/geometry info, if present, else using default 8x12 layout
  plate_size_info <- grep('columns=', data_file, value=TRUE)
  
  if( length(plate_size_info) > 0 )  {
    # \\2 means 2nd match of parthesized pattern; expected string: "# rows=8; colums=12,,,,,,,,,,,,"
    psi = unlist(strsplit(plate_size_info, "((\\s*#\\s*)|(\\s*rows\\s*=\\s*)|(\\s*;\\s*)|(\\s*columns\\s*=\\s*))|((,|\\s)*)$"))
    printDebug(module="loadPlateLayout", "plate_size_info: %s ",psi)
    nrows <- as.numeric(psi[3])
    ncols <- as.numeric(psi[5])
    
    if(is.na(ncols)) {
      # default values for 96 well plate
      nrows <- 8
      ncols <- 12
    }
  } else {
    printError(module="loadPlateLayout", "No plate geometry specified !! (s. layout templates how to specify plate geometry)")
  }
  descr_tab_position <- grep('HEAD_DESCR',data_file, value=FALSE) 
  raw_well_info_df <- read.csv(textConnection(data_file), skip=descr_tab_position-1, nrows=nrows, 
                         header=TRUE, row.names=1, 
                         stringsAsFactors=TRUE, 
                         sep=",", encoding="UTF-8", comment.ch="#")

  wellInfo <- function(raw_well_info)
  {
    wi <- strsplit(raw_well_info, ":")[[1]] 
    temp_df <- data.frame('Type'=as.factor(trim.both(wi[1])), 'Description'=as.factor(trim.both(wi[2])) )
  }
  layout_df <- do.call(rbind, apply(raw_well_info_df, c(2,1), wellInfo ) )
  
  layout_df$Well <- genWellNumbers(nrows=nrows, ncols=ncols, byRows=TRUE)
  layout_df$RowNum <- as.factor(rep(LETTERS[1:nrows], each=ncols))
  layout_df$ColNum <- as.factor(rep(1:ncols))
    
  # processing Volume info 
  volume_pos <- grep('Volumes\\s*:', data_file, value=FALSE)
  
  if( length(volume_pos) > 0 )  {
    volume_raw <- grep('Volumes\\s*:', data_file, value=TRUE)
    well_volume <- as.numeric(sub("(#\\s*Volumes\\s*:\\s*)(\\d*[.]*\\d*)(\\s*)(,*)", "\\2", volume_raw))

    if ( (! is.na(well_volume))  & (well_volume >= 0)) {
      layout_df$Volume = well_volume
    } else {
      raw_volume_info_df <- read.csv(textConnection(data_file), skip=volume_pos, nrows=nrows, 
                                      header=TRUE, row.names=1, 
                                      stringsAsFactors=TRUE, strip.white=TRUE,
                                      sep=",", encoding="UTF-8", comment.ch="#")
      
      layout_df <- cbind(layout_df, do.call(rbind, apply(raw_volume_info_df, c(2,1), 
                                                         function(raw_volume_info) data.frame('Volume'=as.numeric(raw_volume_info) )) ))
    }
  }

  switch(concUnit,
           uM = {conc_factor <- 1e-6;},
           mM = {conc_factor <- 1e-3;},
           M = {conc_factor <- 1.0;},
           {conc_factor <- 1e-6 ;}
         )
  
  printDebug(module="loadPlateLayout", "conc unit: %s - fact %s", concUnit, conc_factor) 
  
  ai <- 1
  addition_pos <- grep('Addition\\s*1:', data_file, value=FALSE) # finding first occurence of additon 1
  while(length(addition_pos) > 0){    
    addition_raw <- grep(paste('Addition\\s*', ai, ':', sep=""), data_file, value=TRUE)
    addition_info <- sub(paste("(#\\s*Addition\\s*", ai, ":\\s*)([^,]+)(\\s*,*)", sep=""), "\\2", addition_raw)
    printDebug(module="loadPlateLayout","Addition info: %s", addition_info)
    addition_type <- trim.both(unlist(strsplit(addition_info, ":"))[1])
    addition_name <- trim.both(unlist(strsplit(addition_info, ":"))[2])
    addition_conc <- as.numeric(unlist(strsplit(addition_info, ":"))[3])
    conc_col_name <- paste(addition_type, "Conc", sep="")

    if ( (! is.na(addition_name)) ) {
          layout_df[,addition_type] = as.factor(addition_name)
          
          if ( (! is.na(addition_conc))  & (addition_conc > 0)) {
            layout_df[,conc_col_name] = addition_conc * conc_factor  # conversion from mM to molar concentration
          }  else layout_df[,conc_col_name] = NA 
    }else {
      raw_addition_info_df <- read.csv(textConnection(data_file), skip=addition_pos, nrows=nrows, 
                                        header=TRUE, row.names=1, 
                                        stringsAsFactors=TRUE, strip.white=TRUE,
                                        sep=",", encoding="UTF-8", comment.ch="#")
      
      additionInfo <- function(raw_addition_info)
      {
        add1_info <- strsplit(raw_addition_info, ":")[[1]] 
        temp_df <- data.frame(as.factor(trim.both(add1_info[1])), as.numeric(add1_info[2])*conc_factor ) # conversion to mol/l
        names(temp_df) <- c(addition_type, conc_col_name )
        return(temp_df)
      }
      layout_df <- cbind(layout_df, do.call(rbind, apply(raw_addition_info_df, c(2,1), additionInfo)) )
    }
    assign("ai", ai + 1)
    addition_pos <- grep(paste('Addition\\s*', ai, ':', sep=""), data_file, value=FALSE)
  }
  
  # processing medium info 
  medium_pos <- grep('Medium/pH/conc:', data_file, value=FALSE)
  
  if( length(medium_pos) > 0 )  {
    medium_raw <- grep('Medium/pH/conc:', data_file, value=TRUE)
    medium_info <- sub("(#\\s*Medium/pH/conc\\s*:\\s*)([^:]+)(\\s*\\:\\s*)(\\d*.\\d*)(\\s*:\\s*)(\\d*.\\d*)(\\s*)(,*)", "\\2:\\4:\\6", medium_raw)
    medium_name <- unlist(strsplit(medium_info, ":"))[1]
    medium_pH <- as.numeric(unlist(strsplit(medium_info, ":"))[2])
    medium_conc <- as.numeric(unlist(strsplit(medium_info, ":"))[3])
    
    if ( (! is.na(medium_conc))  & (medium_conc > 0)) {
      layout_df$Medium = as.factor(trim.both(medium_name))
      layout_df$MediumpH = medium_pH
      layout_df$MediumConc = medium_conc * conc_factor # conversion to molar concentration
    } else {
      raw_medium_info_df <- read.csv(textConnection(data_file), skip=medium_pos, nrows=nrows, 
                                     header=TRUE, row.names=1, 
                                     stringsAsFactors=TRUE, strip.white=TRUE,
                                     sep=",", encoding="UTF-8", comment.ch="#")

      mediumInfo <- function(raw_medium_info) {
        medium_Info <- unlist(strsplit(raw_medium_info, ":")) 
        data.frame('Medium'=as.factor(trim.both(medium_Info[1])), 'MediumpH'=medium_Info[2], 'MediumConc'=medium_Info[3] )
      }
      layout_df <- cbind(layout_df, do.call(rbind, apply(raw_medium_info_df, c(2,1), mediumInfo) ) )
    }
  }

  if (isTRUE(asList)) {
    return(list('Barcodes'=all_barcodes_vec, 'LayoutDescription'=description, 'ConcUnit'=concUnit,
                 'Rows'=nrows, 'Columns'=ncols, 'Layout'=layout_df))
  }   else return(layout_df)
}

#' addPlateLayout
#'
#' @title Adding Plate Layout Information to reader data frame 
#' @description Convenient function to load plate layout from a layout file or database 
#'              and merging it with the reader data frame.
#' @param reader_df=NULL (data.frame) - plate reader data frame from import reader file
#' @param barcode="000000" (string) - this is used for finding the layout file BC_plate_layout_*.csv
#'        barcodes can have a non-numeric prefix, valid barcodes are e.g. TA1001, GA_20, Tbe.0003, XY_abc|0404, WZ.def.4432
#' @param dir="./" (string) - directory of layout file with leading '/' 
#' @param concUnit="uM"  - unit of concentration, valid values are 'uM' and 'mM' 
#' @param padding - used for adding leading zeros to barcode - will later be changed to 6 
#' @param useDBlayout - retrieve layout from django database
#' @param setValueNA=TRUE (boolean) - set values to NA for empty plates of type '0'
#' @param setSlopeNA=FALSE (boolean) - set slope and intercept values to NA for empty plates of type '0'
#' @keywords plate readers, plate layout
#' @export 
#'   
#' @note todo : check, if merging bug with multiple measurements per data frame (e.g. groth data) still exists

addPlateLayout <- function(reader_df=NULL, barcode="000000", 
                           useDBlayout=FALSE, padding = 4, concUnit='uM', 
                           setValueNA=FALSE, setSlopeNA=FALSE)
{  
  if (barcode == "000000") { # auto choose barcode from reader_df if no barcode provided
    barcode <- levels(reader_df$Barcode)[1] # just take first barcode of data frame
    numeric_barcode <- as.numeric(barcode)
    if (! is.na(numeric_barcode)) { # add padding to numeric barcode else: leave barcode as is
      barcode <- sprintf(paste("%0", padding,"d", sep=""), numeric_barcode)
    } 
  } 

  if(isTRUE(useDBlayout)) { 
    require("laraDB") 
    printDebug(module = "addPlateLayout", "addPlateLayout: retrieving layout from DB")
    layout_df <- getPlateLayoutDB(barcode=barcode) 
  }
  else { layout_list <- loadPlateLayout(barcode=barcode, concUnit=concUnit)
         if(identical(layout_list, FALSE) ) {
           printError("ERROR (addPlateLayout): plate layout missing !") 
           return(FALSE) 
         } else layout_df <- layout_list$Layout
  }
  
  # remove old Type and Description
  reader_df$Type = NULL
  reader_df$Description = NULL
  
  # merging new layout info into original data frame
  reader_df <- merge(reader_df, layout_df )
  
  # set values to NA for empty plates
  if(isTRUE(setValueNA)) reader_df[reader_df$Type == '0',]$Value = NA
  if(setSlopeNA) {
    reader_df[reader_df$Type == '0',]$Slope = NA
    reader_df[reader_df$Type == '0',]$Intercept = NA
  } 
  return(reader_df)
}