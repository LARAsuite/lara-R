#_____________________________________________________________________________
#
# PROJECT: LARA
# CLASS: 
# FILENAME: linModels.R
#
# CATEGORY:
#
# AUTHOR: mark doerr
# EMAIL: mark@ismeralda.org
#
# VERSION: 0.1.0
#
# CREATION_DATE: 2015/06/10
# LASTMODIFICATION_DATE: 2015/09/09
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


#' calcAllLinModels
#'
#' @title Calculate All Linear Models of given Data Frame
#' @description Calculates all possible linar models and stores coefficients in a data frame with the following columns
#'              'Well', 'Barcode', 'Num', 'Type',  'Description', 'Slope', 'Intercept'
#' @param kin_data_df (data.frame)
#' @param wavelength = 0 (int)
#' @param barcode="0000" (string)
#' @keywords plate readers
#' @return data frame with slopes added
#' @note relativly well runtime optimized version - don't be confused about the ugly code: it's quite fast
#' @export
#' @examples
#'  lin_model_df <- calcAllLinModels(kin_data_df, wavelength=245)
#' @note todo: adding R-square to output

calcAllLinModels <- function(kin_data_df=NULL, barcode="0000", wavelength=0, modelVar=c("DiffTime", "Value"), groupingVar="Well")
{
  if(barcode == "0000") barcode <- levels(kin_data_df$Barcode)[1]
  
  if (wavelength > 0)  kin_data_sel_df <- kin_data_df[kin_data_df$Wavelength == wavelength,] else {
    kin_data_sel_df <- kin_data_df
    print("Warning(calcAllLinModels): no wavelength specified, plotting first available wavelength")
  }
  
  lm_df <- data.frame('Well'=NA,'Barcode'=NA,'Type'=NA,'Description'=NA,'Slope'=NA, 'Intercept'=NA, "StdErr"=NA, "Tvalue"=NA)
  
  calcLinMod <- function(curr_item)
  {      
    x_model_data <- curr_item[,modelVar[1]]
    y_model_data <- curr_item[,modelVar[2]]
    
    # ALL NA handling
    if( length(na.omit(y_model_data)) == 0 ) { slope <- NA; intercept <- NA } else {
      lin_model <- lm( y_model_data~x_model_data, na.action=na.omit )   # poly3_fit <- lm( y_model_data~poly(time_delta,3) )  
      slope <- coef(lin_model)[['x_model_data']]
      intercept <- coef(lin_model)[['(Intercept)']]
      std_err <- coef(summary(lin_model))[['x_model_data','Std. Error']]
      t_value <- coef(summary(lin_model))[['x_model_data','t value']]
    }
        
    lm_df[i,] <<- c('Well'=as.character.factor(curr_item$Well[1]), 
                    'Barcode'=barcode, 
                    'Type'=as.character.factor( curr_item$Type[1]),  
                    'Description'=as.character.factor(curr_item$Description[1]), 
                    'Slope'=slope,
                    'Intercept'=intercept,
                    "StdErr"=std_err,
                    "Tvalue"=t_value)
    i <<- i + 1
  }
  # main iteration loop
  i<-1
  by(kin_data_sel_df, kin_data_sel_df[,groupingVar], calcLinMod)
  
  lm_df$Well <- as.factor(lm_df$Well)
  lm_df$Barcode <- as.factor(lm_df$Barcode)
  lm_df$Description <- as.factor(lm_df$Description)
  lm_df$Type <- as.factor(lm_df$Type)
  lm_df$Slope <- as.numeric(lm_df$Slope)
  lm_df$Intercept <- as.numeric(lm_df$Intercept)
  lm_df$StdErr <- as.numeric(lm_df$StdErr)
  lm_df$Tvalue <- as.numeric(lm_df$Tvalue)
  
  return(lm_df)
}

#' selectBestLinModel
#'
#' @title Select Best Linear Model for a given Data Frame
#' @description Calculates all possible linar models and stores coefficients in a data frame
#'              based on a suggestion of Martin Weiss to find the smallest R-square 
#' @param kin_data_df
#' @keywords plate readers
#' @return data frame with 
#' @note relativly well runtime optimized version - don't be confused about the ugly code: it's quite fast
#' @export
#' @examples
#'  for examples, please wait until fixed !!!
#' @note todo: Rewrite as aggregate function ?
#'       system.time()

selectBestLinModel <- function(kin_data_df=NULL, barcode="0000", wavelength=0, 
                               modelVar=c("DiffTime", "Value"), groupingVar="Well", minIntervalSize=0, minIntervalSizeFraction=0.8)
{
  printDebug("selecteBestLinModel v0.0.9b")
  
  # definitions
  min_interval_size <- minIntervalSize
  # this fraction is used to autmatically determine the minimal interval size, if none is provide
  min_interval_size_fraction <- minIntervalSizeFraction  # = 80% of all points should be at least in linear range
  
  if(barcode == "0000") barcode <- levels(kin_data_df$Barcode)[1]
    
  if (wavelength > 0)  kin_data_sel_df <- kin_data_df[kin_data_df$Wavelength == wavelength,] else {
    kin_data_sel_df <- kin_data_df
    print("Warning(selectBestLinModel): no wavelength specified, plotting first available wavelength")
  }
    
  best_lm_df <- data.frame('Well'=NA,'Barcode'=NA,'Type'=NA,'Description'=NA,
                           'Slope'=NA, 'Intercept'=NA, "StdErr"=NA, "Tvalue"=NA, 'BestLinLeft'=NA, 'BestLinRight'=NA)
  
  analyzePerItemKinetics <- function(curr_item) {
    #if (i > 1) return()
    # calcualtes linear properties of one given datapoint interval
    calcLinProperties <- function(first_data_point, last_data_point) {
      x_model_data <- curr_item[(first_data_point:last_data_point),modelVar[1]]
      y_model_data <- curr_item[(first_data_point:last_data_point),modelVar[2]]
      
      # ALL NA handling
      if(  all(is.na(y_model_data)) ) { r_squared <- 0; slope <- NA; std_err <- NA; t_value<-NA;
                                                 intercept <- NA ; lin_model <- data.frame("A"=1,"B"=1) } else {
        lin_model <- lm( y_model_data~x_model_data, na.action=na.omit )   # poly3_fit <- lm( y_model_data~poly(time_delta,3) )  
        r_squared <- summary(lin_model)$r.squared
        slope <- coef(lin_model)[['x_model_data']]
        intercept <- coef(lin_model)[['(Intercept)']]
        std_err <- coef(summary(lin_model))[['x_model_data','Std. Error']]
        t_value <- coef(summary(lin_model))[['x_model_data','t value']]
      }
      
      #lin_model <- lm(y_model_data~x_model_data, na.action=na.omit)
      
      #printDebug("rsqr: %s ", summary(lin_model)$r.squared )
      
      return(c("R_squared"=r_squared, 
               "Slope"=slope, "Intercept"=intercept, "StdErr"=std_err, "Tvalue"=t_value,
               "BestLinLeft"=first_data_point, "BestLinRight"=last_data_point,  "LinMod"=lin_model))
    }
    
    temp_lin_model_lst <- NULL
    # iteration through all relevant right indices with minimal interval size of min_interval_size
    calcAllLinModels <- function(first_data_point) {
      right_index_vec <- ((first_data_point+min_interval_size):num_meas_cycles)
      temp_lin_model_lst <- sapply(right_index_vec, function(last_data_point) calcLinProperties(first_data_point, last_data_point) )   
      local_max_r_sq <- which.max(temp_lin_model_lst["R_squared",])
      
      return(temp_lin_model_lst[,local_max_r_sq])
    }
    # left index loop
    num_meas_cycles <- nrow(curr_item)
    if (min_interval_size == 0) min_interval_size <- floor(num_meas_cycles * min_interval_size_fraction)
    #printDebug("selecteBestLinModel: min interval size: %s", min_interval_size)
    all_lin_coeff_lst <- sapply((1:(num_meas_cycles - min_interval_size)), calcAllLinModels)

    # selecting the global best r squared
    max_r_sq <-which.max(all_lin_coeff_lst["R_squared",])
    # selecting model with highest r squared
    best_lin_model <<- all_lin_coeff_lst[,max_r_sq]
      
    best_lm_df[i,] <<- c('Well'=as.character.factor(curr_item$Well[1]), 
                         'Barcode'=barcode, 
                         'Type'=as.character.factor( curr_item$Type[1]),  
                         'Description'=as.character.factor(curr_item$Description[1]), 
                         'Slope'=best_lin_model$Slope, 
                         'Intercept'=best_lin_model$Intercept,
                         'StdErr'=best_lin_model$StdErr,
                         'Tvalue'=best_lin_model$Tvalue,
                         'BestLinLeft'=curr_item[best_lin_model$BestLinLeft,]$DiffTime, 
                         'BestLinRight'=curr_item[best_lin_model$BestLinRight,]$DiffTime
                         )     
    i <<- i + 1
  }
  # main iteration loop
  i<-1
  by(kin_data_sel_df, kin_data_sel_df[,groupingVar], analyzePerItemKinetics)
  
  best_lm_df$Well <- as.factor(best_lm_df$Well)
  best_lm_df$Barcode <- as.factor(best_lm_df$Barcode)
  best_lm_df$Description <- as.factor(best_lm_df$Description)
  best_lm_df$Type <- as.factor(best_lm_df$Type)
  best_lm_df$Slope <- as.numeric(best_lm_df$Slope)
  best_lm_df$Intercept <- as.numeric(best_lm_df$Intercept)
  best_lm_df$StdErr <- as.numeric(best_lm_df$StdErr)
  best_lm_df$Tvalue <- as.numeric(best_lm_df$Tvalue)
  best_lm_df$BestLinLeft <- as.numeric(best_lm_df$BestLinLeft)  
  best_lm_df$BestLinRight <- as.numeric(best_lm_df$BestLinRight)  
  
  return(best_lm_df)
}

#' selectBestSlopeModel
#'
#' @title Select Best Linear Model for a given Data Frame
#' @description Calculates all possible linar models and stores coefficients in a data frame
#'              based on a suggestion of Martin Weiss to find the smallest R-square 
#' @param kin_data_df
#' @keywords plate readers
#' @return data frame with 
#' @note relativly well runtime optimized version - don't be confused about the ugly code: it's quite fast
#' @export
#' @examples
#'  please wait until fixed !!!
#' @note todo: not yet adapted to new data frame structure !! Rewrite as aggregate function
#'       system.time()

selectBestSlopeModel <- function(kin_data_df=NULL, barcode="0000", wavelength=0, 
                                 modelVar=c("DiffTime", "Value"), groupingVar="Well",
                                 minIntervalSize=0, minIntervalSizeFraction=0.8)
{
  printDebug("SelectBestSlopeModel (v.0.0.9): change difftime, removeing Well column ? !!")
  # definitions
  min_interval_size <- minIntervalSize
  # this fraction is used to autmatically determine the minimal interval size, if none is provide
  min_interval_size_fraction <- minIntervalSizeFraction  # = 80% of all points should be at least in linear range
  
  if(barcode == "0000") barcode <- levels(kin_data_df$Barcode)[1]
  
  if (wavelength > 0)  kin_data_sel_df <- kin_data_df[kin_data_df$Wavelength == wavelength,] else {
    kin_data_sel_df <- kin_data_df
    printDebug("Warning(selectBestLinModel): no wavelength specified, using whole data frame for the calculations !")
  }
  
  best_lm_df <- data.frame('Barcode'=NA,'Type'=NA,'Description'=NA,
                           'Slope'=NA, 'Intercept'=NA, "StdErr"=NA, "Tvalue"=NA, 'BestLinLeft'=NA, 'BestLinRight'=NA)
  
  analyzePerItemKinetics <- function(curr_item) {
    #if (i > 1) return()
    # calcualtes linear properties of one given datapoint interval
    calcLinProperties <- function(first_data_point, last_data_point) {
      x_model_data <- curr_item[(first_data_point:last_data_point),modelVar[1]]
      y_model_data <- curr_item[(first_data_point:last_data_point),modelVar[2]]
#       
#       print("best slope")
#       print(head(curr_item),2)
#       
      
#       r_squared <- 0; slope <- NA; std_err <- NA; t_value<-NA ;
#       intercept <- NA ; lin_model <- data.frame("A"=1,"B"=1)

      # ALL NA handling
      if( all(is.na(y_model_data)) ) {  return(NA) } else {
        lin_model <- lm( y_model_data~x_model_data, na.action=na.omit )   # poly3_fit <- lm( y_model_data~poly(time_delta,3) )  
        r_squared <- summary(lin_model)$r.squared
        slope <- coef(lin_model)[['x_model_data']]
        intercept <- coef(lin_model)[['(Intercept)']]
        std_err <- coef(summary(lin_model))[['x_model_data','Std. Error']]
        t_value <- coef(summary(lin_model))[['x_model_data','t value']]
      }      
      return(c("R_squared"=r_squared, 
               "Slope"=slope, "Intercept"=intercept,  "StdErr"=std_err, "Tvalue"=t_value,
               "BestLinLeft"=first_data_point, "BestLinRight"=last_data_point,  "LinMod"=lin_model))
    }
    
    temp_lin_model_lst <- NULL
    # iteration through all relevant right indices with minimal interval size of min_interval_size
    calcAllLinModels <- function(first_data_point) {
      right_index_vec <- ((first_data_point+min_interval_size):num_meas_cycles)
      temp_lin_model_lst <- sapply(right_index_vec, function(last_data_point) calcLinProperties(first_data_point, last_data_point) )
      
#       print("selectBestSlopeModel- slope")
#       print(temp_lin_model_lst)
#       
      if( all(is.na(temp_lin_model_lst)) ) return(NA)
      
      local_max_r_sq <- which.max(temp_lin_model_lst["R_squared",])
      local_max_slope <- which.max(temp_lin_model_lst["Slope",])
      
      return(temp_lin_model_lst[,local_max_slope])
    }
    # left index loop
    num_meas_cycles <- nrow(curr_item)
    if (min_interval_size == 0) min_interval_size <- floor(num_meas_cycles * min_interval_size_fraction)
    printDebug("min int size: %s",min_interval_size)
    all_lin_coeff_lst <- sapply(c(1), calcAllLinModels)  # for best slope we only need to calc. from first position
    #all_lin_coeff_lst <- sapply((1:(num_meas_cycles - min_interval_size)), calcAllLinModels)
    
    # selecting the global best r squared
#     print("selectBestSlopeModel-  slope")
#     print(all_lin_coeff_lst)

    if( all(is.na(all_lin_coeff_lst)) ) return() 

    max_slope <-which.max(all_lin_coeff_lst["Slope",])
    # selecting model with highest r squared
    best_lin_model <<- all_lin_coeff_lst[,max_slope]
    
    best_lm_df[i,] <<- c('Barcode'=barcode,
                         'Type'=as.character.factor( curr_item$Type[1]),  
                         'Description'=as.character.factor(curr_item$Description[1]),
                         'Slope'=best_lin_model$Slope, 
                         'Intercept'=best_lin_model$Intercept,
                         'StdErr'=best_lin_model$StdErr,
                         'Tvalue'=best_lin_model$Tvalue,
                         'BestLinLeft'=curr_item[best_lin_model$BestLinLeft,]$DiffTime, 
                         'BestLinRight'=curr_item[best_lin_model$BestLinRight,]$DiffTime
    )
    i <<- i + 1
  }
  # main iteration loop
  i<-1
  by(kin_data_sel_df, kin_data_sel_df[,groupingVar], analyzePerItemKinetics)
  
  best_lm_df$Barcode <- as.factor(best_lm_df$Barcode)
  best_lm_df$Description <- as.factor(best_lm_df$Description)
  best_lm_df$Type <- as.factor(best_lm_df$Type)
  best_lm_df$Slope <- as.numeric(best_lm_df$Slope)
  best_lm_df$Intercept <- as.numeric(best_lm_df$Intercept)
  best_lm_df$StdErr <- as.numeric(best_lm_df$StdErr)
  best_lm_df$Tvalue <- as.numeric(best_lm_df$Tvalue)
  best_lm_df$BestLinLeft <- as.numeric(best_lm_df$BestLinLeft)  
  best_lm_df$BestLinRight <- as.numeric(best_lm_df$BestLinRight)  
  
  return(best_lm_df)
}

# ------------------------------------

#' selectBestLinModelList
#'
#' @title Select Best Linear Model for a given Data Frame
#' @description Calculates all possible linar models and stores coefficients in a data frame
#'              based on a suggestion of Martin Weiss to find the smallest R-square 
#' @param kin_data_df
#' @keywords plate readers
#' @return data frame with 
#' @note relativly well runtime optimized version - don't be confused about the ugly code: it's quite fast
#' @export
#' @examples
#'  please wait until fixed !!!
#' @note todo: not yet adapted to new data frame structure !!
#'       system.time()

selectBestLinModelList <- function(kin_data_lst=NULL, barcode="0000", wavelength=0, minIntervalSize=0, minIntervalSizeFraction=0.8)
{
  # definitions
  min_interval_size <- minIntervalSize
  # this fraction is used to autmatically determine the minimal interval size, if none is provide
  min_interval_size_fraction <- minIntervalSizeFraction  # = 80% of all points should be at least in linear range
  
  if(barcode == "0000") barcode <- kin_data_lst$Barcode
  print(barcode)
  
  if (wavelength > 0)  kin_data_sel_df <- kin_data_lst[kin_data_lst$Wavelength == wavelength,] else {
    kin_data_sel_df <- kin_data_lst
    print("Warning(selectBestLinModel): no wavelength specified, plotting first available wavelength")
  }
  
  best_lm_df <- data.frame('Well'=NA,'Barcode'=NA,'Type'=NA,'Description'=NA,
                           'Slope'=NA, 'Intercept'=NA, 'BestLinLeft'=NA, 'BestLinRight'=NA)
  
  analyzePerItemKinetics <- function(curr_item) {
    #if (i > 1) return()
    # calcualtes linear properties of one given datapoint interval
    calcLinProperties <- function(first_data_point, last_data_point) {
      y_model_data <- curr_item[(first_data_point:last_data_point),]$Value 
      x_model_data <- curr_item[(first_data_point:last_data_point),]$DiffTime 
      
      print("best slope")
      print(head(curr_item),2)
      
      # ALL NA handling
      if( length(na.omit(y_model_data)) == 0 ) { r_squared <- 0; slope <- NA; intercept <- NA ; lin_model <- data.frame("A"=1,"B"=1) } else {
        lin_model <- lm( y_model_data~x_model_data, na.action=na.omit )   # poly3_fit <- lm( y_model_data~poly(time_delta,3) )  
        r_squared <- summary(lin_model)$r.squared
        slope <- coef(lin_model)[['x_model_data']]
        intercept <- coef(lin_model)[['(Intercept)']]
      }
      
      #lin_model <- lm(y_model_data~x_model_data, na.action=na.omit)
      
      #printDebug("rsqr: %s ", summary(lin_model)$r.squared )
      
      return(c("R_squared"=r_squared, 
               "Slope"=slope, "Intercept"=intercept, 
               "BestLinLeft"=first_data_point, "BestLinRight"=last_data_point , "LinMod"=lin_model))
    }
    
    temp_lin_model_lst <- NULL
    # iteration through all relevant right indices with minimal interval size of min_interval_size
    calcAllLinModels <- function(first_data_point) {
      right_index_vec <- ((first_data_point+min_interval_size):num_meas_cycles)
      temp_lin_model_lst <- sapply(right_index_vec, function(last_data_point) calcLinProperties(first_data_point, last_data_point) )   
      local_max_r_sq <- which.max(temp_lin_model_lst["R_squared",])
      
      return(temp_lin_model_lst[,local_max_r_sq])
    }
    # left index loop
    num_meas_cycles <- nrow(curr_item)
    if (min_interval_size == 0) min_interval_size <- floor(num_meas_cycles * min_interval_size_fraction)
    printDebug("min int: %s",min_interval_size)
    all_lin_coeff_lst <- sapply((1:(num_meas_cycles - min_interval_size)), calcAllLinModels)
    
    # selecting the global best r squared
    max_r_sq <-which.max(all_lin_coeff_lst["R_squared",])
    # selecting model with highest r squared
    best_lin_model <<- all_lin_coeff_lst[,max_r_sq]
        
    i <<- i + 1
     
    return( list('Well'=curr_item$Well[1], 
                         'Barcode'=as.factor(barcode), 
                         'Type'=as.character.factor(curr_item$Type[1]),  
                         'Description'=as.character.factor(curr_item$Description[1]), 
                         'Substrate1'=as.character.factor(curr_item$Substrate1[1]),
                         'Substr1Conc'=as.numeric.factor(curr_item$Substr1Conc[1]),
                         'Enzyme'=as.character.factor(curr_item$Enzyme[1]),
                         'EnzymeConc'=as.numeric.factor(curr_item$EnzymeConc[1]),
                         'Slope'=best_lin_model$Slope, 
                         'Intercept'=best_lin_model$Intercept,
                         'BestLinLeft'=curr_item[best_lin_model$BestLinLeft,]$DiffTime, 
                         'BestLinRight'=curr_item[best_lin_model$BestLinRight,]$DiffTime )  )
  }
  # main iteration loop
  i<-1
  best_lm_lst <- lapply(kin_data_sel_df, analyzePerItemKinetics)
    
  return(best_lm_lst)
}
