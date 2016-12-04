#_____________________________________________________________________________
#
# PROJECT: LARA
# CLASS: 
# FILENAME: test_laraDataReader.R
#
# CATEGORY:
#
# AUTHOR: mark doerr
# EMAIL: mark@ismeralda.org
#
# VERSION: 0.1.0
#
# CREATION_DATE: 161124
# LASTMODIFICATION_DATE: 161124
#
# BRIEF_DESCRIPTION: unit tests of the laraDataReader package
# DETAILED_DESCRIPTION: 
# HISTORY: 
# INSTALL: 
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


test_that("testing most simple layout reading", { 
  ### ------ plain plate layout functions
  # layout matrix is determined by the layout file 
  
  # most simple layout 
  
  tmp <- tempfile(pattern = "ldr_prof_", fileext = ".txt")
  
  printDebug(module="test_that", "set current working directory to 'demo'")

  setwd("../plate_layout_templates")

  Rprof(tmp)
  pl96well = loadPlateLayout(barcode="0001")
  Rprof(NULL) 

  expect_equal(pl96well$Barcodes, "0001" ) 
  expect_equal(pl96well$Rows, 8 )
  expect_equal(pl96well$Columns, 12 )
  
  curr_layout = pl96well$Layout
  expect_equal(as.character.factor(curr_layout$Description[1]), 'WT' ) 
  expect_equal(as.character.factor(curr_layout$Type[96]), 'nCTRL' ) 

  prof_df = summaryRprof(tmp)
  
  print(prof_df)
  
  }
)

test_that("testing complex layout reading", { 
  ### ------ plain plate layout functions
  # layout matrix is determined by the layout file 
  
  tmp <- tempfile(pattern = "ldr_prof_", fileext = ".txt")
  
  printDebug(module="test_that", "set current working directory to 'demo'")
  
  setwd("../plate_layout_templates")
  
  # testing very complex layout
  # with individually defined medium information for each wells and uM unit
  Rprof(tmp)
  pl96well = loadPlateLayout(barcode="0022") 
  Rprof(NULL) 
  
  expect_equal(pl96well$Barcodes[27], "WZ.def.4436" ) 
  expect_equal(pl96well$Rows, 8 )
  expect_equal(pl96well$Columns, 12 )
  
  prof_df = summaryRprof(tmp)
  
  print(prof_df)
  
}
)

