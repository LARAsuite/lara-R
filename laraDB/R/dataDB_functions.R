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
# VERSION: 0.1.1
#
# CREATION_DATE: 2015/07/15
# LASTMODIFICATION_DATE: 2017/03/21
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
#' @title addDataDB - adding data to database
#' @description Adding arbitrary data to LARA database
#' @param connect_db
#' @keywords database
#' @return last_data_row
#' @note relativly well runtime optimized version - don't be confused about the ugly code: it's quite fast
#' @export 
#' @examples
#'   addDataDB()
#' @note todo: add barcode handling !!
#'             add metainfo handling !!
#' 

addDataDB <- function(connect_db=NULL, data=NULL, evalPath="", filename="", startDatetime=NULL,
                      barcode="", device="", method="" , expName="", asCSV=FALSE, exportData=FALSE,
                      digits=0, scientific=-3, metaInfo="", commit=TRUE,
                      DB_filename="/var/local/lara/laraDB.sqlite3" )
{
  require("DBI")
  
  debugging <- TRUE
  
  printDebug("addDataDB 0.1.1a - trying to add %s data to database", expName)
  
  new_db_connection = FALSE
  if (is.null(connect_db)) {
    connect_db <- dbConnect(RSQLite::SQLite(), DB_filename)
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
  
  if (is.null(startDatetime) ) startDatetime <-Sys.time()  #format(Sys.time(),format='%Y%m%d%H%M%S')
  
  if( class(data) == "data.frame" ) data_ser <- list(serialize(data, NULL, ascii=TRUE))
  
  # remember to generate data SHA256 hash 
#  insertion_df <- data.frame( "filename"=filename, 
#                              "start_datetime"=startDatetime,
#                              #"metainfo"=metaInfo,
#                              "exp_data"=I(data_ser),
#                              device=device, 
#                              method=method)
  #print(insertion_df)
  
  printDebug(" D: %s, M: %s",device, method)
  
  #query <- sprintf("INSERT INTO  lara_data_data  ( filename, start_datetime, exp_data, device_id, method_id ) 
  #                  VALUES(?,?,?,?,?,
  #                    (SELECT id FROM projects_device D WHERE D.name ='%s'),
  #                    (SELECT id FROM projects_item_class PC WHERE PC.lara_metainfo_item_class ='%s'))", device, method )
  
  #try full insertion_df
  query <- sprintf("INSERT INTO  lara_data_data ( filename, start_datetime, exp_data, device_id, method_id ) 
                    VALUES($filename, $start_datetime, $exp_data, 
                      (SELECT device_id FROM lara_devices_device D WHERE D.name ='%s'),
                      (SELECT item_class_id FROM lara_metainfo_item_class MMIC WHERE MMIC.item_class ='%s'))", device, method )
  print(query)
#-  dbExecute(connect_db, query, insertion_df )
  
  #query_res <- dbGetQuery(connect_db, "SELECT last_insert_rowid()")
  #last_data_row <- as.numeric(query_res)
  
#-  last_data_row <- as.numeric(dbGetQuery(DB_connect, "SELECT last_insert_rowid();"))
  
  #last_data_row <- as.numeric(dbFetch(lr_query))
  
  # now connecting data to related project_item  
#-  query <- sprintf("INSERT INTO lara_projects_proj_item_data (projitem_id, data_id) 
#-                    VALUES ((SELECT id FROM lara_projects_proj_item 
#-                    WHERE name='%s'),'%s')", expName, last_data_row )
  
  print(query)
#-  results <- dbExecute(connect_db, query )
  
  ### ----- new 
  
 #- data_ser <- serialize(meas_df, NULL, ascii=TRUE)
  
  if (is.null(startDatetime) ) startDatetime <-Sys.time()  #format(Sys.time(),format='%Y%m%d%H%M%S')
  print(startDatetime)
  
  # remember to generate data SHA256 hash
#  insertion_df <- data.frame( "filename"=filename,
#                              "start_datetime"=startDatetime,
#                              #"metainfo"=metaInfo,
#                              "exp_data"=I(list(data_ser)),
#                              "device"=device,
#                              "method"=method)
  
  query <- "INSERT INTO  lara_data_data ( filename, start_datetime, exp_data, device_id, method_id )
                VALUES($filename, $start_datetime, $exp_data,
                (SELECT device_id FROM lara_devices_device D WHERE D.name = $device),
                (SELECT item_class_id FROM lara_metainfo_item_class MMIC WHERE MMIC.item_class =$method));" 
  
  print(query)
#-  res <- dbExecute(connect_db, query, params=insertion_df )
  
  #results <- dbSendPreparedQuery(connect_db, query ,insertion_df )
  
#-  lr_query <- dbSendQuery(connect_db, "SELECT last_insert_rowid()")
#-  last_data_row <- as.numeric(dbFetch(lr_query))
  
#-  query <- sprintf("INSERT INTO lara_projects_proj_item_data (projectitem_id, data_id) 
#-                    VALUES('%s','%s')", proj_it_id, last_data_row )
  
#-  if (isTRUE(commit)) dbCommit(connect_db)
  
  if (isTRUE(new_db_connection) ) dbDisconnect(connect_db)

  printDebug("addDataDB: reader data set %s written in db ....", expName)
  
  #-return(last_data_row)
}

#' addMeasDataDB
#'
#' @title addMeasDataDB - adding data to database
#' @description adding data to database
#' @param connect_db=NULL,
#' @param DB_filename="/var/local/lara/laraDB.sqlite3"
#' @param filename="" 
#' @param evalPath=""
#' @param barcode=""
#' @param device=""
#' @param method="" 
#' @param expName=""
#' @param startDatetime=NULL
#' @param  PLC=FALSE
#' @keywords plate readers
#' @return proj_it_id
#' @note  none
#' @export 
#' @examples
#'   lin_mod_df <- calcAllLinModels(kin_df, wavelength=245)
#' @note todo: add barcode handling !!
#' 

addMeasDataDB <- function(connect_db=NULL, DB_filename="/var/local/lara/laraDB.sqlite3", filename="", 
                      evalPath="", barcode="", device="", method="" , expName="", startDatetime=NULL, PLC=FALSE )
{
  require("DBI")
  library("laraDataReader")
  
  debugging <- TRUE
  
  printDebug("check obsolete ? addDataDB 0.1.0d - trying to import %s data to database", expName)
  # might speed up with dbGetQuery ....
  
  print(filename)
  
  setwd(evalPath)
  
  filename_to_import <- structure(filename, class=device)
  meas_df = LA_ImportData(filename=filename_to_import, method=method, barcode=barcode, layout=TRUE, useDBlayout=TRUE, PLC=PLC)
  
  print(head(meas_df))
  
  if (is.null(connect_db)) {
    connect_db <- dbConnect(RSQLite::SQLite(), DB_filename)
    dbBegin(connect_db)
    new_db_connection = TRUE  
  }
  
  query <- sprintf("SELECT proj_item_id, name, start_datetime
                    FROM lara_projects_proj_item 
                    WHERE name='%s'", expName )
  print(query)
  results <- dbSendQuery(connect_db, query)
  exp_parameters_df <- dbFetch(results, -1)
  
  print(head(exp_parameters_df))
  
  proj_it_id <- exp_parameters_df$proj_item_id
  
#  query <- paste("SELECT device_id FROM lara_devices_device WHERE name='", device,"'",sep="")
#  results <- dbSendQuery(connect_db, query)
#  device_id <- dbFetch(results, -1)$device_id
  
  data_ser <- serialize(meas_df, NULL, ascii=TRUE)
  
  if (is.null(startDatetime) ) startDatetime <-Sys.time()  #format(Sys.time(),format='%Y%m%d%H%M%S')
  print(startDatetime)

  # remember to generate data SHA256 hash
  insertion_df <- data.frame( "filename"=filename,
                              "start_datetime"=startDatetime,
                              #"metainfo"=metaInfo,
                              "exp_data"=I(list(data_ser)),
                              "device"=device,
                              "method"=method)

  query <- "INSERT INTO  lara_data_data ( filename, start_datetime, exp_data, device_id, method_id )
                VALUES($filename, $start_datetime, $exp_data,
                (SELECT device_id FROM lara_devices_device D WHERE D.name = $device),
                (SELECT item_class_id FROM lara_metainfo_item_class MMIC WHERE MMIC.item_class =$method));" 
  
  print(query)
  res <- dbExecute(connect_db, query, params=insertion_df )

  #results <- dbSendPreparedQuery(connect_db, query ,insertion_df )
  
  lr_query <- dbSendQuery(connect_db, "SELECT last_insert_rowid()")
  last_data_row <- as.numeric(dbFetch(lr_query))
 
 # print("lr")
 #  print(last_data_row)
  
#  query <-sprintf("SELECT PPI.id  
#                  FROM projects_proj_item PPI   
#                     JOIN projects_proj_item_container PCI ON PPI.id=PCI.proj_item_id 
#                  JOIN projects_container PC  ON PC.barcode='%s'  
#                  WHERE PPI.method_id=(SELECT id FROM projects_method PM WHERE  PM.name='%s')", barcode, method_name)
#  print(query)
#  results <- dbSendQuery(connect_db, query )
#  proj_it_id <- as.numeric(dbFetch(results))
#  print(proj_it_id)
  
  query <- sprintf("INSERT INTO lara_projects_proj_item_data (projectitem_id, data_id) 
                    VALUES('%s','%s')", proj_it_id, last_data_row )
  
  print(query)
  results <- dbExecute(connect_db, query )
  dbCommit(connect_db)
    
  dbDisconnect(connect_db)


  printDebug("addDataDB: reader data set %s written in db ....", expName)
  
  return(proj_it_id)
}

#' getExperiments2EvalDB
#'
#' @title getExperiments2EvalDB - Get Evaluation Parameters of an Evaluation by IDadding container plate layout to database
#' @description Get Evaluation Parameters of an Evaluation by IDadding container plate layout to database
#' @param dbConnection=NULL
#' @param dbFilename="/var/local/lara/laraDB.sqlite3"
#' @param expName=""
#' @keywords plate readers
#' @return data frame with 
#' @note none
#' @export 
#' @note todo: 
#' 

getExperiments2EvalDB <- function(dbConnection=NULL, dbFilename="/var/local/lara/laraDB.sqlite3", expName="")
{
  require("RSQLite")
  
  new_db_connection = FALSE
  if (is.null(dbConnection)) {
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
#' @title getExperiments2EvalIDDB - Get Evaluation Parameters of an Evaluation by IDadding container plate layout to database
#' @description Get Evaluation Parameters of an Evaluation by IDadding container plate layout to database
#' @param evalID=0
#' @keywords plate readers
#' @return data frame with 
#' @note none
#' @export 
#' @note todo: 
#' 

getExperiments2EvalIDDB <- function(dbConnection=NULL, dbFilename="/var/local/lara/laraDB.sqlite3", evalID=0)
{
  require("DBI")
  
  new_db_connection = FALSE
  if (is.null(dbConnection)) {
    print("getExperiments2EvalIDDB: no database connection - connecting to db")
    dbConnection <- dbConnect(RSQLite::SQLite(), dbFilename)
    dbBegin(dbConnection)
    new_db_connection = TRUE  
  }  
  
  # query <- sprintf(" SELECT PEVAL.experiments_data, PEVAL.parameters
  #                  FROM projects_evaluation PEVAL
  #                  WHERE PEVAL.id=%s", evalID )  

  # query <- sprintf("SELECT  PI.proj_item_id, PI.name, PE.parameters
  #                   FROM  lara_projects_proj_item PI
  #                   INNER JOIN lara_projects_evaluation_experiments PIE ON PI.proj_item_id = PIE.proj_item_id 
  #                   INNER JOIN lara_data_evaluation DE ON DE.eval_id = PIE.evaluation_id
  #                   WHERE PIE.evaluation_id=%s", evalID )  
  
    
  query <- sprintf("SELECT  PI.proj_item_id, PI.name, PE.parameters
                    FROM  lara_projects_proj_item PI
                    INNER JOIN lara_projects_evaluation_experiments PIE ON PI.proj_item_id = PIE.proj_item_id 
                    INNER JOIN lara_data_evaluation DE ON DE.eval_id = PIE.evaluation_id
                    WHERE PIE.evaluation_id=%s", evalID )  
  
  query_result <- dbExecute(dbConnection, query)
  
  # print(query_result$experiments_data)
  
  if (isTRUE(new_db_connection) ) dbDisconnect(dbConnection)
  
  return(query_result)
}


#' getEvalParametersDB
#'
#' @title getEvalParametersDB - Get Evaluation Parameter of an Evaluation by ID 
#' @description Get Evaluation Parameter of an Evaluation by ID
#' @param evalID
#' @keywords lara_database
#' @return parameter string 
#' @note none
#' @export
#' @note todo: 
#' 

getEvalParametersDB <- function(dbConnection=NULL, dbFilename="/var/local/lara/laraDB.sqlite3", evalID=0)
{
  print("getEvalParam")
  require("DBI")
  
  new_db_connection = FALSE
  if (is.null(dbConnection)) {
    print("getExperiments2EvalIDDB: no database connection - connecting to db")
    dbConnection <- dbConnect(RSQLite::SQLite(), dbFilename)
    dbBegin(dbConnection)
    new_db_connection = TRUE  
  }  
  
  query <- sprintf("SELECT  DE.parameters
                    FROM  lara_data_evaluation DE 
                    WHERE DE.eval_id=%s", evalID )  
  # printDebug(query)
  query_result <- dbGetQuery(dbConnection, query)
  
  #print(query_result$parameters)
  
  if (isTRUE(new_db_connection) ) dbDisconnect(dbConnection)
  
  return(query_result)
}

#' getExpDataByIDDB
#'
#' @title getExpDataByID - Get Data of an Experiment by ID
#' @description This function retrieved data related to an experiment identified by expID and method name 
#' @param expID=1
#' @param    method=""
#' @keywords plate readers
#' @return data frame with experiment data
#' @note none
#' @export
#' @note todo: 
#' 

getExpDataByIDDB <- function(dbConnection=NULL, dbFilename="/var/local/lara/laraDB.sqlite3", expID=1, method="")
{
  print("getExpDataByIDDB 1.1.0a")
  
  debugging <- TRUE
  
  new_db_connection = FALSE
  if (is.null(dbConnection)) {
    require("DBI")
    dbConnection <- dbConnect(RSQLite::SQLite(), dbFilename)
    dbBegin(dbConnection)
    new_db_connection = TRUE  
  }
 
  query  <- sprintf("SELECT PD.exp_data 
                     FROM lara_data_data PD 
                     INNER JOIN lara_projects_proj_item_data PID ON PD.data_id = PID.data_id
                     INNER JOIN lara_metainfo_item_class MIIC ON MIIC.item_class_id=PD.method_id
                     INNER JOIN lara_projects_proj_item PI ON PI.proj_item_id=PID.projectitem_id 
                     WHERE PI.proj_item_id = '%d' AND MIIC.item_class='%s'", expID, method )
 
  printDebug(query)
  res <- dbGetQuery(dbConnection, query)
  
  if(length(res$exp_data) > 0){
    comb_df <- do.call(rbind, lapply(res$exp_data , function(x) unserialize(x)))
  } else {
    print("ERROR (getExpDataByIDDB) - no data found !!")
    comb_df <- data.frame()
  }
  if (isTRUE(new_db_connection) ) dbDisconnect(dbConnection)
    
  return(comb_df)
}

#' getExpDataDB
#'
#' @title getExpData - Get Data of an Experiment by experiment name and method
#' @description This function retrieved data related to an experiment identified by experiment Name and method name 
#' @param expName=""
#' @param       method=""
#' @keywords plate readers
#' @return data frame with experiment data
#' @note  none
#' @export
#' @note todo: 
#' 

getExpDataDB <- function(dbConnection=NULL, dbFilename="/var/local/lara/laraDB.sqlite3", expName="", method="")
{
  new_db_connection = FALSE
  if (is.null(dbConnection)) {
    require("DBI")
    dbConnection <- dbConnect(RSQLite::SQLite(), dbFilename)
    dbBegin(dbConnection)
    new_db_connection = TRUE  
  }  
  
  # query  <- sprintf(" SELECT PD.exp_data FROM projects_data PD 
  #                     INNER JOIN projects_proj_item_data PID ON PD.id = PID.data_id
  #                     INNER JOIN projects_method PM ON PM.id=PD.method_id
  #                     INNER JOIN projects_proj_item PI ON PI.id=PID.proj_item_id WHERE PI.name = '%s' AND PM.name='%s'", expName, method )
  
  query  <- sprintf(" SELECT PD.exp_data FROM lara_data_data PD 
                      INNER JOIN lara_projects_proj_item_data PID ON PD.projectitem_id = PID.data_id
                      INNER JOIN lara_projects_method PM ON PM.id=PD.method_id
                      INNER JOIN lara_projects_proj_item PI ON PI.proj_item_id=PID.proj_item_id 
                      WHERE PI.name = '%s' AND PM.name='%s'", expName, method )
  
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
