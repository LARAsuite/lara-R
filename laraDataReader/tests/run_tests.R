#!/usr/bin/Rscript

#_____________________________________________________________________________
#
# PROJECT: LARA
# CLASS: 
# FILENAME: run_tests.R
#
# CATEGORY:
#
# AUTHOR: mark doerr
# EMAIL: mark@ismeralda.org
#
# VERSION: 0.1.0
#
# CREATION_DATE: 161124
# LASTMODIFICATION_DATE: 161130
#
# BRIEF_DESCRIPTION: unit tests of the laraDataReader package
# DETAILED_DESCRIPTION: 
# HISTORY: 
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

library(testthat) 

debugging <- TRUE #FALSE

setwd(".")

source("../R/importData.R")
source("../R/layoutReader.R")

test_results <- test_dir(".", reporter="summary")
