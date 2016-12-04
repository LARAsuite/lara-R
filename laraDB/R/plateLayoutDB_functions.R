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
# VERSION: 0.0.9
#
# CREATION_DATE: 2015/07/15
# LASTMODIFICATION_DATE: 2015/08/02
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
#' @description Calculates all possible linar models and stores coefficients in a data frame
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
  require("RSQLite")
  require("laraDataReader")
  
  print("addPlateLayoutDB 0.0.9")

  layout_lst <- loadPlateLayout(barcode=barcode, asList=TRUE)
  
  #print("load plate layout barcodes:")
  #print(head(layout_lst$Layout))
  
  ## connecting/using an existing file
  if(is.null(DB_connect)) {
    sqlite    <- dbDriver("SQLite")
    DB_connect <- dbConnect(sqlite,DB_filename)
    dbBegin(DB_connect)
    #print("addPlateLayout: ... now connected to DB")
  }
  # serialization is required to save the data frame into database BLOB
  layout_ser <- serialize(layout_lst$Layout, NULL, ascii=TRUE)
  #print(layout_ser)
  insertion_df <- data.frame(layout=I(list(layout_ser)))
  results <- dbSendPreparedQuery(DB_connect, "INSERT INTO projects_container_layout (layout) 
                                 VALUES(?)",insertion_df )
  
  lr_query <- dbSendQuery(DB_connect, "SELECT last_insert_rowid()")
  last_row <- as.numeric(dbFetch(lr_query))

  #printDebug("layout for %s inserted in DB - as list, datetime", barcode)
  #print(last_row)

  # connecting layout info with container 
  connectLayoutContainers <- function(barcode) {
    # !! checking before inserting, if container has a layout
    query <- sprintf("SELECT id,layout_id FROM projects_container WHERE barcode = '%s'",
                      barcode )
    #print(query)
    results <- dbFetch(dbSendQuery(DB_connect, query))
    
    #print(results);  print(results$id)
    
    if(length(results$id) == 0)  # == no container found
    {
      query <- sprintf("INSERT INTO projects_container (barcode, description, date_time, layout_id) 
                        VALUES ('%s','%s','%s',%i)",
                        barcode, layout_lst$LayoutDescription, "20150101 000000", last_row )
      #print(query)
      results <- dbSendQuery(DB_connect, query) 
    }
    else { 
      # possible check of layoutID
      # connect container_layout with projects_container 
      query <- sprintf("UPDATE projects_container 
                        SET description = '%s', layout_id = %i 
                        WHERE barcode = '%s'",
                        layout_lst$LayoutDescription, last_row, barcode)
      #print(query)
      results <- dbSendQuery(DB_connect, query) 
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
  require("RSQLite")
  #require("laraDataReader")
  print("getPlateLayoutDB 0.0.9")
  
  ## connecting/using an existing file
  if(is.null(DB_connect)) {
    sqlite    <- dbDriver("SQLite")
    DB_connect <- dbConnect(sqlite, DB_filename)
    dbBegin(DB_connect)
    printDebug("getPlateLayoutDB: ... now connected to DB")
  }
  
  # retriving the data:
  query <- sprintf("SELECT layout FROM projects_container PC 
                    JOIN projects_container_layout CL ON PC.layout_id = CL.id  
                    WHERE PC.barcode = '%s'", barcode)
  print(query)
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
#barcode_df <- dbFetch(results, -1)