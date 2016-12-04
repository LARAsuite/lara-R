#_____________________________________________________________________________
#
# PROJECT: LARA
# CLASS: 
# FILENAME: enzymeKinetics.R
#
# CATEGORY:
#
# AUTHOR: mark doerr
# EMAIL: mark@ismeralda.org
#
# VERSION: 0.0.9
#
# CREATION_DATE: 2015/07/07
# LASTMODIFICATION_DATE: 2015/09/11
#
# BRIEF_DESCRIPTION: Library for handling enyme kinetics
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

#as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

#' michaelisMentenKinetics
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
  
michaelisMentenKinetics <- function(mm_df=NULL, k_cat=0.1, k_m=0.001, addZero=TRUE, trace=FALSE, csvOut=FALSE)
{
  printDebug("WARNING(MMkinetics): v.0.09b, a) check columnames b) add fit-Error to output file !")
  
  # remove empty wells from calculation
  mm_df <- mm_df[mm_df$Type != "0",]
  
  mm_fit_lst = NULL 
  i = 1
  perEnzymeKinetics <- function(mm_subset)
  {
    print("mmkin")
    print(head(mm_subset))
    
    enz_conc = mm_subset$EnzymeConc[1]
  
    if(isTRUE(addZero)) {
      zero_val = mm_subset[1,]
      zero_val$Substrate1Conc <- 0
      zero_val$v0 <- 0
      rbind( mm_subset, zero_val)
    }
    
    #if(enz_conc == 0 ) { printDebug("ERROR(MMkinetics): enzyme concentration = 0 !") ;  return() }
    
    if(all(is.na(enz_conc))) {
      print("ERROR(michaelisMentenKinetics): no Enzyme concentration specified, please remove these enzymes from your data frame"); 
      return() }
    
    mm_fit <- nls( v0 ~ ( ( k_cat * enz_conc * Substrate1Conc)/(k_m + Substrate1Conc)), 
                   data = mm_subset, 
                   start = list(k_cat=k_cat, k_m=k_m), 
                   trace = trace)
    
#     require(minpack.lm)
#     mm_fit <- nlsLM( v0 ~ ( ( k_cat * enz_conc * Substrate1Conc)/(k_m + Substrate1Conc)), 
#                      data = mm_subset, 
#                      start = list(k_cat=k_cat, k_m=k_m), 
#                      trace = TRUE) 
#     
    #sum_fit <- summary(mm1_lst$S39T$CurveFit)
    #coefficients(sum_fit)['k_cat','Std. Error']
    
    k_cat = coef(mm_fit)["k_cat"]
    k_m  = coef(mm_fit)["k_m"]
    mm_fit_lst[[i]] <<- list("Enzyme"=mm_subset$Enzyme[1], 
                             "EnzymeConc"=enz_conc, 
                             "Km"=k_m,
                             "Kcat"=k_cat,
                             "KcatKm"=k_cat/k_m,
                             "CurveFit"=mm_fit)
    i <<- i + 1
  }
  invisible(by(mm_df, mm_df$Enzyme, perEnzymeKinetics ))
  
  # Warning ! might be error - prone ...:
  names(mm_fit_lst) <- as.character(unique(mm_df$Enzyme))

  if(csvOut) {
    mm_out_df = do.call(rbind, lapply(mm_fit_lst, function(item) c(as.character.factor(item$Enzyme), item$EnzymeConc,  item$Km, item$Kcat, item$KcatKm)  ))
    dimnames(mm_out_df) <- list(c(1:nrow(mm_out_df)), c("Enzyme", "EnzymeConc","Km", "Kcat", "KcatKm"))
    filename <- "MichaelisMentenKinetics_summary.csv"
    write.table(mm_out_df, file=filename,  append=TRUE, sep="," , row.names=FALSE)
    
#     #shortcut, not as nice
#     mm_out_df = do.call(rbind, lapply(mm_fit_lst, c))
  }

  return(mm_fit_lst)
}

#' michaelisMentenKineticsSP
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

michaelisMentenKineticsSP <- function(mm_df=NULL)
{
  mm_fit_lst = NULL 
  i = 1
  perEnzymeKinetics <- function(mm_subset)
  {
    enz_conc <- mm_subset$EnzymeConc[1]
    
    substr_conc <- mm_subset$Substrate1Conc[1]
    
    v_0 <- mm_subset$Slope[1]
    
    kcat_km <- v_0 / (enz_conc * substr_conc)
    
#     mm_fit <- nls( v0 ~ ( ( k_cat * enz_conc * Substrate1Conc)/(k_m + Substrate1Conc)), 
#                    data = mm_subset, 
#                    start = list(k_cat=0.1, k_m=0.001), 
#                    trace = trace)
#     
    #sum_fit <- summary(mm1_lst$S39T$CurveFit)
    #coefficients(sum_fit)['k_cat','Std. Error']
    
#     k_cat = coef(mm_fit)["k_cat"]
#     k_m  = coef(mm_fit)["k_m"]

    mm_fit_lst[[i]] <<- list("Enzyme"=mm_subset$Enzyme[1], 
                             "EnzymeConc"=enz_conc, 
                             "Km"=NA,
                             "Kcat"=NA,
                             "KcatKm"=kcat_km,
                             "CurveFit"=NA)
    i <<- i + 1
  }
  invisible(by(mm_df, mm_df$Enzyme, perEnzymeKinetics ))
  
  # Warning ! might be error - prone ...:
  names(mm_fit_lst) <- as.character(unique(mm_df$Enzyme))
  
  return(mm_fit_lst)
}

#' pHProfile
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

pHProfile <- function(pH_df=NULL, kcat_km=100, pKa=6.5, trace=FALSE)
{
  pH_fit_lst = NULL 
  i = 1
  perEnzymeKinetics <- function(pH_subset)
  {
    enz_conc = pH_subset$EnzymeConc[1]
    
    pH_fit <- nls( KcatKm ~ ( ( kcat_km * 10^-pKa)/(10^-MediumpH + 10^-pKa)), 
                   data = pH_subset, 
                   start = list(kcat_km=kcat_km, pKa=pKa), 
                   trace = trace)
    
    #     require(minpack.lm)
    
    kcat_km = coef(pH_fit)["kcat_km"]
    pKa  = coef(pH_fit)["pKa"]
    pH_fit_lst[[i]] <<- list("Enzyme"=pH_subset$Enzyme[1], 
                             "EnzymeConc"=enz_conc, 
                             "KcatKm"=kcat_km,
                             "PKa"=pKa,
                             "CurveFit"=pH_fit)
    i <<- i + 1
  }
  invisible(by(pH_df, pH_df$Enzyme, perEnzymeKinetics ))
  
  # Warning ! might be error - prone ...:
  names(pH_fit_lst) <- as.character(unique(pH_df$Enzyme))
  
  return(pH_fit_lst)
}
