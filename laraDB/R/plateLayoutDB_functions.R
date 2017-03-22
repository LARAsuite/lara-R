#'_____________________________________________________________________________
#
# PROJECT: LARA
# CLASS: 
# FILENAME: plateLayoutDB_functions.R
#
# CATEGORY:
#
# AUTHOR: mark doerr
# EMAIL: mark@ismeralda.org
#
# VERSION: 0.1.1
#
# CREATION_DATE: 2015/07/15
# LASTMODIFICATION_DATE: 2017/03/17
#
# BRIEF_DESCRIPTION: Library for reading and wrting plate layout information into LARA database
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


#' addPlateLayoutDB
#'
#' @title adding container plate layout to database v0.1.0
#' @description adds container plate layout to LARA database
#' @param DB_connect=NULL (database connection) - in case one needs to use an exisiting connection
#' @param DB_filename= "/var/local/lara/laraDB.sqlite3" (string) - path to SQLite database file
#' @param barcode="0000" (string) - barcode for layout file to load
#' @keywords plate readers
#' @return data frame with 
#' @note relativly well runtime optimized version - don't be confused about the ugly code: it's quite fast
#' @export
#' 
#' @note todo: 
#' 

addPlateLayoutDB <- function(DB_connect=NULL, DB_filename="/var/local/lara/laraDB.sqlite3", barcode="0000")
{
  require("DBI")
  require("laraDataReader")
  
  print("addPlateLayoutDB 0.1.1y")
  
  #debugging <- TRUE
  
  layout_lst <- loadPlateLayout(barcode=barcode, asList=TRUE)
  
  #printDebug("load plate layout barcodes:")
  #print(head(layout_lst$Layout))
  
  ## connecting/using an existing database file
  if(is.null(DB_connect)) {
    DB_connect <- dbConnect(RSQLite::SQLite(), DB_filename)
    dbBegin(DB_connect)
  }
  # serialization is required to save the data frame into database BLOB
  layout_ser <- serialize(layout_lst$Layout, NULL, ascii=TRUE)
  #print(layout_ser)
  insertion_df <- data.frame(layout=I(list(layout_ser)),rows=layout_lst$Rows, cols=layout_lst$Columns,
                             conc_unit=layout_lst$ConcUnit, description=layout_lst$LayoutDescription)
  
  results <-  dbExecute(DB_connect, "INSERT INTO lara_containers_container_layout (rows, cols, layout, conc_unit, description)
                                 VALUES ($rows, $cols, $layout, $conc_unit, $description);", params = insertion_df )
  
  #lr_query <- dbExecute(DB_connect, "SELECT last_insert_rowid();")
  #last_row <- as.numeric(dbFetch(lr_query))
  
  last_row <- as.numeric(dbGetQuery(DB_connect, "SELECT last_insert_rowid();"))

  printDebug("layout for %s inserted in DB - as list, datetime", barcode)
  print(last_row)

  # connecting layout info with container 
  connectLayoutContainers <- function(barcode) {
    # !! checking before inserting, if container has a layout
    query <- sprintf("SELECT container_id,layout_id FROM lara_containers_container WHERE barcode = '%s';",
                      barcode )
    #print(query)
    results <- dbGetQuery(DB_connect, query) # dbFetch(dbSendStatement(DB_connect, query)) 
    
    print(results);  print(results$layout_id)
    
    if(length(results$layout_id) == 0)  # == no container found, insert new container
    {
      query <- sprintf("INSERT INTO lara_containers_container (barcode, description, layout_id) 
                        VALUES ('%s','%s',%i);",
                        barcode, layout_lst$LayoutDescription, last_row )
      print(query)
      results <- dbExecute(DB_connect, query) 
    }
    else { 
      # possible check of layoutID
      # connecting container_layout with projects_container 
      query <- sprintf("UPDATE lara_containers_container
                        SET description = '%s', layout_id = %i 
                        WHERE barcode = '%s';",
                        layout_lst$LayoutDescription, last_row, barcode)
      print(query)
      results <- dbExecute(DB_connect, query) 
    }
  }
  sapply(layout_lst$Barcodes, connectLayoutContainers )
  
  dbCommit(DB_connect)    
  dbDisconnect(DB_connect)
  print("layout set inserted, database disconnected  ...")
  
  return(TRUE)
}

#' getPlateLayoutDB
#'
#' @title retrieving plate layout from database
#' @description retrieving plate layout from database
#' @param DB_connect=NULL
#' @param DB_filename="/var/local/lara/laraDB.sqlite3"
#' @param barcode="0000"
#' @keywords plate readers
#' @return data frame with 
#' @note relativly well runtime optimized version - don't be confused about the ugly code: it's quite fast
#' @export
#' @examples
#'  lin_mod_df <- calcAllLinModels(kin_df, wavelength=245)
#' @note todo: add barcode handling !!
#' 

getPlateLayoutDB <- function(barcode="0000", DB_connect=NULL, DB_filename="/var/local/lara/laraDB.sqlite3")
{
  require("DBI")
  print("getPlateLayoutDB 0.1.0c")
  
  print(DB_filename)
  
  ## connecting/using an existing file
  if(is.null(DB_connect)) {
    DB_connect <- dbConnect(RSQLite::SQLite(), DB_filename)
    dbBegin(DB_connect)
    printDebug("getPlateLayoutDB: ... now connected to DB")
  }
  
  
  # retriving the data:
  query <- sprintf("SELECT layout FROM lara_containers_container PC 
                    JOIN lara_containers_container_layout CL ON PC.layout_id = CL.id  
                    WHERE PC.barcode = '%s'", barcode)
  #print(query)
  layout_raw <- dbGetQuery(DB_connect, query)
  
  #printDebug("raw layout:", layout_raw)
  #print("raw layout")
  #print(head(layout_raw))
  
  if(nrow(layout_raw) == 0 ) {
    printDebug("ERROR(getPlatLayoutDB): no layout for plate %s in database", barcode)
    return(FALSE)
  } 
  layout_df <- unserialize(unlist(layout_raw))
  
  #print("layout from db - data frame unserialized: ")
  #print(head(layout_lst))
  
  dbDisconnect(DB_connect)
  return(layout_df)
}

#query <- paste("SELECT expID FROM lara_container WHERE barcode=", barcode)
#print(query)
#results <- dbSendQuery(DB_connect, query)
#exp_parameters_df <- dbFetch(results, -1)
#barcode_df <- dbFetch(results, -1