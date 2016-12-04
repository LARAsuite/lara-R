#'_____________________________________________________________________________
#
# PROJECT: LARA
# CLASS: 
# FILENAME: dataDB_functions.R
#
# CATEGORY:
#
# AUTHOR: mark doerr
# EMAIL: mark@ismeralda.org
#
# VERSION: 0.1.0
#
# CREATION_DATE: 2015/07/15
# LASTMODIFICATION_DATE: 2016/07/20
#
# BRIEF_DESCRIPTION: Library for reading and wrting data into LARA database
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


#' addDataDB
#'
#' @title adding  plate data to database
#' @description Calculates all possible linar models and stores coefficients in a data frame
#' @param kin_data_df, wavelength
#' @keywords plate readers
#' @return data frame with 
#' @note relativly well runtime optimized version - don't be confused about the ugly code: it's quite fast
#' @export
#' @examples
#'  lin_mod_df <- calcAllLinModels(kin_df, wavelength=245)
#' @note todo: add barcode handling !!
#' 

addDataDB <- function(connect_db=NULL, data=NULL, evalPath="", filename="", startDate=NULL, startTime=NULL,
                      barcode="", device="", method="" , expName="", asCSV=FALSE, exportData=FALSE,
                      digits=0, scientific=-3, metaInfo="", commit=TRUE,
                      DB_filename="/var/local/lara/laraDB.sqlite3" )
{
  require("RSQLite")
  
  printDebug("addDataDB 0.1.0a - trying to import %s data to database", expName)
  # might speed up with dbGetQuery ....
  
  new_db_connection = FALSE
  if (is.null(connect_db)) {
    sqlite <- dbDriver("SQLite")
    connect_db <- dbConnect(sqlite, DB_filename)
    dbBegin(connect_db)
    new_db_connection = TRUE  
  }  
  
  # adjusting number format in output data 
  if ( digits > 0) {  
    data <- format(data, digits=digits, scientific=scientific)
  }
  if (isTRUE(exportData)) {
    write.table(data, file=filename, row.names=FALSE, sep=",", dec=".", qmethod="double") 
  }
  if (isTRUE(asCSV)) {
    # writing results to database as csv
    txt_conn_1 <- textConnection("output_text", open="w", encoding=c("UTF-8")) 
    #write.table(results_df, file=txt_conn_1, sep=",", eol="\n", row.names=FALSE)
    #cat("#", metaInfo, "\n", file=txt_conn_1)
    write.table(data, file=txt_conn_1, row.names=FALSE, sep="|", dec=".", qmethod="double")
    close(txt_conn_1)  

    data <- paste(output_text, collapse="\n")
  }
  
  if (is.null(startDate) ) startDate <- format(Sys.Date(),format='%Y%m%d')
  if (is.null(startTime) ) startTime <- format(Sys.time(),format='%H%M%S')
  
  if( class(data) == "data.frame" ) data <- list(serialize(data, NULL, ascii=TRUE))
  
  # remember to generate data SHA256 hash 
  insertion_df <- data.frame( "filename"=filename, 
                              "start_date"=startDate,
                              "start_time"=startTime,
                              "meta_info"=metaInfo,
                              "exp_data"=I(data) )
  print(insertion_df)
  
  printDebug(" D: %s, M: %s",device, method)
  
  query <- sprintf("INSERT INTO  projects_data  ( filename, start_date, start_time, meta_info, exp_data, device_id, method_id ) 
                    VALUES(?,?,?,?,?,
                      (SELECT id FROM projects_device D WHERE D.name ='%s'),
                      (SELECT id FROM projects_item_class PC WHERE PC.item_class ='%s'))", device, method )
  
  dbSendPreparedQuery(connect_db, query ,insertion_df )
  
  query_res <- dbGetQuery(connect_db, "SELECT last_insert_rowid()")
  last_data_row <- as.numeric(query_res)
  
  #last_data_row <- as.numeric(dbFetch(lr_query))
    
  query <- sprintf("INSERT INTO projects_proj_item_data (proj_item_id, data_id) 
                    VALUES ((SELECT id FROM projects_proj_item 
                    WHERE name='%s'),'%s')", expName, last_data_row )
  
  #print(query)
  results <- dbSendQuery(connect_db, query )
  
  if (isTRUE(commit)) dbCommit(connect_db)
  
  if (isTRUE(new_db_connection) ) dbDisconnect(connect_db)

  printDebug("addDataDB: reader data set %s written in db ....", expName)
  
  return(last_data_row)
}



#' addMeasDataDB
#'
#' @title adding  plate data to database
#' @description Calculates all possible linar models and stores coefficients in a data frame
#' @param kin_data_df, wavelength
#' @keywords plate readers
#' @return data frame with 
#' @note relativly well runtime optimized version - don't be confused about the ugly code: it's quite fast
#' @export
#' @examples
#'  lin_mod_df <- calcAllLinModels(kin_df, wavelength=245)
#' @note todo: add barcode handling !!
#' 

addMeasDataDB <- function(connect_db=NULL, dbFilename="/var/local/lara/laraDB.sqlite3", filename="", 
                      evalPath="", barcode="", device="", method="" , expName="", PLC=FALSE )
{
  require("RSQLite")
  library("laraDataReader")
  
  printDebug("addDataDB 0.0.9 - trying to import %s data to database", expName)
  # might speed up with dbGetQuery ....
  
  setwd(evalPath)
  
  filename_to_import <- structure(filename, class=device)
  meas_df = LA_ImportData(filename=filename_to_import, method=method, barcode=barcode, layout=TRUE, useDBlayout=TRUE, PLC=PLC)
  
  sqlite <- dbDriver("SQLite")
  connect_db <- dbConnect(sqlite, dbFilename)
  dbBegin(connect_db)
  
  query <- sprintf("SELECT id, name, start_date, start_time 
                    FROM projects_proj_item 
                    WHERE name='%s'", expName )
  results <- dbSendQuery(connect_db, query)
  exp_parameters_df <- dbFetch(results, -1)
  
  proj_it_id <- exp_parameters_df$id
  
  query <- paste("SELECT id FROM projects_device WHERE name='", device,"'",sep="")
  results <- dbSendQuery(connect_db, query)
  device_id <- dbFetch(results, -1)$id
  
  abs_ser <- serialize(meas_df, NULL, ascii=TRUE)
  insertion_df <- data.frame( filename=abs_filename, 
                              date=argsL['date'], time=argsL['time'], 
                              exp_data=I(list(abs_ser)), deviceID=device_id )
  
  query <- sprintf("INSERT INTO  projects_data ( filename, start_date, start_time, exp_data, device_id, method_id) 
                    VALUES(?,?,?,?,?,(SELECT id FROM projects_item_class IC WHERE IC.item_class ='%s'))",method_name)
  
  results <- dbSendPreparedQuery(connect_db, query ,insertion_df )
  lr_query <- dbSendQuery(connect_db, "SELECT last_insert_rowid()")
  last_data_row <- as.numeric(dbFetch(lr_query))
 
#  query <-sprintf("SELECT PPI.id  
#                  FROM projects_proj_item PPI   
#                     JOIN projects_proj_item_container PCI ON PPI.id=PCI.proj_item_id 
#                  JOIN projects_container PC  ON PC.barcode='%s'  
#                  WHERE PPI.method_id=(SELECT id FROM projects_method PM WHERE  PM.name='%s')", barcode, method_name)
#  print(query)
#  results <- dbSendQuery(connect_db, query )
#  proj_it_id <- as.numeric(dbFetch(results))
#  print(proj_it_id)
  
  query <- sprintf("INSERT INTO projects_proj_item_data (proj_item_id, data_id) 
                    VALUES('%s','%s')", proj_it_id, last_data_row )
  
  #print(query)
  results <- dbSendQuery(connect_db, query )
  dbCommit(connect_db)
    
  dbDisconnect(connect_db)

  printDebug("addDataDB: reader data set %s written in db ....", expName)
  
  return(proj_it_id)
}

#' getExperiments2EvalDB
#'
#' @title adding container plate layout to database
#' @description Calculates all possible linar models and stores coefficients in a data frame
#' @param kin_data_df, wavelength
#' @keywords plate readers
#' @return data frame with 
#' @note relativly well runtime optimized version - don't be confused about the ugly code: it's quite fast
#' @export
#' @note todo: add barcode handling !!
#' 

getExperiments2EvalDB <- function(dbConnection=NULL, dbFilename="/var/local/lara/laraDB.sqlite3", expName="")
{
  require("RSQLite")
  
  new_db_connection = FALSE
  if (is.null(dbConnection)) {
    sqlite <- dbDriver("SQLite")
    dbConnection <- dbConnect(sqlite, dbFilename)
    dbBegin(dbConnection)
    new_db_connection = TRUE  
  }  
  
  # data handling missing: PEVAL.experiments_data,  
  query <- sprintf("SELECT PEVAL.parameters
                    FROM projects_evaluation PEVAL
                    INNER JOIN projects_proj_item_evaluation PIE ON PIE.evaluation_id = PEVAL.id
                    INNER JOIN projects_proj_item PI ON PI.id = PIE.proj_item_id
                    WHERE PI.name='%s' ", expName )  
  print(query)
  query_result <- dbGetQuery(dbConnection, query)
  
  print(query_result$experiments_data)
  
  if (isTRUE(new_db_connection) ) dbDisconnect(dbConnection)
  
  return(query_result)
}

#' getExperiments2EvalIDDB
#'
#' @title adding container plate layout to database
#' @description Calculates all possible linar models and stores coefficients in a data frame
#' @param kin_data_df, wavelength
#' @keywords plate readers
#' @return data frame with 
#' @note relativly well runtime optimized version - don't be confused about the ugly code: it's quite fast
#' @export
#' @note todo: add barcode handling !!
#' 

getExperiments2EvalIDDB <- function(dbConnection=NULL, dbFilename="/var/local/lara/laraDB.sqlite3", evalID=0)
{
  require("RSQLite")
  
  new_db_connection = FALSE
  if (is.null(dbConnection)) {
    sqlite <- dbDriver("SQLite")
    dbConnection <- dbConnect(sqlite, dbFilename)
    dbBegin(dbConnection)
    new_db_connection = TRUE  
  }  
  
  # query <- sprintf(" SELECT PEVAL.experiments_data, PEVAL.parameters
  #                  FROM projects_evaluation PEVAL
  #                  WHERE PEVAL.id=%s", evalID )  
  
  
  
  query <- sprintf("SELECT  PI.id, PI.name, PE.parameters
                    FROM  projects_proj_item PI
                    INNER JOIN projects_evaluation_experiments PIE ON PI.id = PIE.proj_item_id 
                    INNER JOIN projects_evaluation PE ON PE.id =PIE.evaluation_id
                    WHERE PIE.evaluation_id=%s", evalID )  
  
  query_result <- dbGetQuery(dbConnection, query)
  
  # print(query_result$experiments_data)
  
  if (isTRUE(new_db_connection) ) dbDisconnect(dbConnection)
  
  return(query_result)
}


#' getExpDataByIDDB
#'
#' @title adding container plate layout to database
#' @description Calculates all possible linar models and stores coefficients in a data frame
#' @param kin_data_df, wavelength
#' @keywords plate readers
#' @return data frame with 
#' @note relativly well runtime optimized version - don't be confused about the ugly code: it's quite fast
#' @export
#' @note todo: add barcode handling !!
#' 

getExpDataByIDDB <- function(dbConnection=NULL, dbFilename="/var/local/lara/laraDB.sqlite3", expID=1, method="")
{
  require("RSQLite")
  
  new_db_connection = FALSE
  if (is.null(dbConnection)) {
    sqlite <- dbDriver("SQLite")
    dbConnection <- dbConnect(sqlite, dbFilename)
    dbBegin(dbConnection)
    new_db_connection = TRUE  
  }  
  
  # query  <- sprintf(" SELECT PD.exp_data FROM projects_data PD 
  #                     INNER JOIN projects_proj_item_data PID ON PD.id = PID.data_id
  #                     INNER JOIN projects_method PM ON PM.id=PD.method_id
  #                     INNER JOIN projects_proj_item PI ON PI.id=PID.proj_item_id WHERE PI.id = '%d' AND PM.name='%s'", expID, method )
  
  
  # SELECT "projects_proj_item"."id", "projects_proj_item"."name", "projects_proj_item"."title", "projects_proj_item"."description", "projects_proj_item"."start_date", "projects_proj_item"."start_time", "projects_proj_item"."item_class_id", "projects_proj_item"."method_id", "projects_proj_item"."device_id", "projects_proj_item"."procedure_id", "projects_proj_item"."parameters", "projects_proj_item"."sample_source", "projects_proj_item"."status_id", "projects_proj_item"."outcome_id", "projects_proj_item"."selected", "projects_proj_item"."remarks", "projects_proj_item"."reference_experiment_id", "projects_proj_item"."parent_id", "projects_proj_item"."lft", "projects_proj_item"."rght", "projects_proj_item"."tree_id", "projects_proj_item"."level" FROM "projects_proj_item" INNER JOIN "projects_evaluation_experiments" ON ("projects_proj_item"."id" = "projects_evaluation_experiments"."proj_item_id") WHERE "projects_evaluation_experiments"."evaluation_id" = 44 ORDER BY "projects_proj_item"."tree_id" ASC, "projects_proj_item"."lft" ASC
  
 
  query  <- sprintf("SELECT PD.exp_data 
                     FROM projects_data PD 
                     INNER JOIN projects_proj_item_data PID ON PD.id = PID.data_id
                     INNER JOIN projects_item_class PC ON PC.id=PD.method_id
                     INNER JOIN projects_proj_item PI ON PI.id=PID.proj_item_id 
                     WHERE PI.id = '%d' AND PC.item_class='%s'", expID, method )
  
  print(query)
  res <- dbGetQuery(dbConnection, query)
  
  if(length(res$exp_data) > 0){
    comb_df <- do.call(rbind, lapply(res$exp_data , function(x) unserialize(x)))
  } else {
    print("no data found !!")
    comb_df <- data.frame()
  }
  if (isTRUE(new_db_connection) ) dbDisconnect(dbConnection)
    
  return(comb_df)
}


#' getExpDataDB
#'
#' @title adding container plate layout to database
#' @description Calculates all possible linar models and stores coefficients in a data frame
#' @param kin_data_df, wavelength
#' @keywords plate readers
#' @return data frame with 
#' @note relativly well runtime optimized version - don't be confused about the ugly code: it's quite fast
#' @export
#' @note todo: add barcode handling !!
#' 

getExpDataDB <- function(dbConnection=NULL, dbFilename="/var/local/lara/laraDB.sqlite3", expName="", method="")
{
  require("RSQLite")
  
  new_db_connection = FALSE
  if (is.null(dbConnection)) {
    sqlite <- dbDriver("SQLite")
    dbConnection <- dbConnect(sqlite, dbFilename)
    dbBegin(dbConnection)
    new_db_connection = TRUE  
  }  
  
  query  <- sprintf(" SELECT PD.exp_data FROM projects_data PD 
                      INNER JOIN projects_proj_item_data PID ON PD.id = PID.data_id
                      INNER JOIN projects_method PM ON PM.id=PD.method_id
                      INNER JOIN projects_proj_item PI ON PI.id=PID.proj_item_id WHERE PI.name = '%s' AND PM.name='%s'", expName, method )
  
#   query  <- sprintf(" SELECT PD.exp_data FROM projects_data PD 
#                       INNER JOIN projects_proj_item_data PID ON PD.id = PID.data_id
#                       INNER JOIN projects_proj_item PI ON PI.id=PID.proj_item_id WHERE PI.name = '%s' ", exp_name )
  
  print(query)
  res <- dbGetQuery(dbConnection, query)
  
  comb_df <- do.call(rbind, lapply(res$exp_data , function(x) unserialize(x)))
  
  if (isTRUE(new_db_connection) ) dbDisconnect(dbConnection)
  
  return(comb_df)
  
#   query <- sprintf("SELECT PD.exp_data 
#                     FROM projects_data PD 
#                     INNER JOIN projects_proj_item_data PID ON PD.id=PID.data_id 
#                     WHERE PID.proj_item_id=%s", expID)
#   print(query)
#   res <- dbGetQuery(dbConnection, query)
  #eval(parse(text=res$parameters))
  
#   comb_df <- do.call(rbind, lapply(res$exp_data , function(x) unserialize(x)))
#   
#   return(list("Parameter"=parse(text=res$parameters), "Data"=comb_df[order(comb_df$Well, comb_df$DiffTime),] ))
}
