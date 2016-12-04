#_____________________________________________________________________________
#
# PROJECT: LARA
# CLASS: 
# FILENAME: laraDataReader_demo.R
#
# CATEGORY:
#
# AUTHOR: mark doerr
# EMAIL: mark@ismeralda.org
#
# VERSION: 0.1.0
#
# CREATION_DATE: 2015/06/01
# LASTMODIFICATION_DATE: 161124
#
# BRIEF_DESCRIPTION: Demo and unit tests of the laraDataReader package
# DETAILED_DESCRIPTION: 
# HISTORY: 
# INSTALL: 
#          library("devtools")
#          document("laraDataReader")
#          install("laraDataReader")
#          document("laraEvalVis")
#          install("laraEvalVis")
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

library("laraDataReader")

print("set current working directory to 'demo'")

setwd("../plate_layout_templates")

### ------ plain plate layout functions
# layout matrix is determined by the layout file 
pl96well = loadPlateLayout(barcode="0001") # most simple layout 

pl96well$Barcodes
pl96well$Rows
pl96well$Columns

print(pl96well)

pl24well = loadPlateLayout(barcode="0002") # layout with 4x8 geometry
print(pl24well)

# with fixed well volume (211 ul)
pl96well = loadPlateLayout(barcode="0003") 
print(pl96well)

# with individually defined well volumes
pl96well = loadPlateLayout(barcode="0004") 
print(pl96well)

# with single substrate1 concentration for all wells
pl96well = loadPlateLayout(barcode="0005") 
print(pl96well)

# with individually defined substrate1 concentration for each wells
pl96well = loadPlateLayout(barcode="0006") 
print(pl96well)

# with single substrate2 concentration for all wells
pl96well = loadPlateLayout(barcode="0007") 
print(pl96well)

# with individually defined substrate2 concentration for each wells
pl96well = loadPlateLayout(barcode="0008") 
print(pl96well)

# with single addition (e.g. inductor, like IPTG) concentration for all wells
pl96well = loadPlateLayout(barcode="0009") 
print(pl96well)

# with individually defined addition (e.g. inductor, like IPTG) concentration for each wells
pl96well = loadPlateLayout(barcode="0010") 
print(pl96well)

# with single medium information (pH, conc.) for all wells
pl96well = loadPlateLayout(barcode="0011") 
print(pl96well)

# with individually defined medium information for each wells
pl96well = loadPlateLayout(barcode="0012") 
print(pl96well)

# with individually defined medium information for each wells and uM unit
pl96well = loadPlateLayout(barcode="0021") 
print(pl96well)

# with individually defined medium information for each wells and uM unit
pl96well = loadPlateLayout(barcode="0022") 
print(pl96well)

# ... I think, you got the idea (there is almost no limit with adding additions)

# unit testing, speed
system.time(loadPlateLayout(barcode="0002"))

# Evaluate shortFunction() for 100 times
system.time( replicate(n = 10, loadPlateLayout(barcode="0002")) )

system.time( replicate(n = 10, loadPlateLayout(barcode="0022")) )

system.time(loadPlateLayout(barcode="0016"))

### ------ combined reading of layout and data
setwd("../demo")
## BMG omega plate reader
# * defining the filenames to be read and the class of the data
growth_filenames <- structure("BC_0021.*omega_SPabs.*", class="omega")

# * reading multiple BMG omega single point absorption files with two wavelengths (600nm and 660nm)
# with corresponding plate layout information 
growth_df = LA_ImportData(growth_filenames, layout=TRUE, PLC=TRUE)
head(growth_df,24)

system.time(LA_ImportData(growth_filenames, layout=TRUE, PLC=TRUE))
## * plate reading Thermo varioskan

# single point absorption measurement (SPabs)
#assay_filename <- structure("BC_0031.*varioskan_SPabs.*", class="varioskan")

# reading multiple Thermo single point absorption files with two wavelengths (600nm and 660nm)
# with corresponding plate layout information 
vs_assay_df = LA_ImportData(assay_filename, method='SPabs', layout=TRUE, PLC=TRUE)

# kinetic absorption measurement (KINabs)
assay_filename <- structure("BC_0032.*varioskan_KINabs.*", class="varioskan")

# reading multiple Thermo single point absorption files with two wavelengths (600nm and 660nm)
# with corresponding plate layout information 
vs_assay_df = LA_ImportData(assay_filename, method='KINabs', layout=TRUE, PLC=TRUE)
print(head(vs_assay_df))

# importing data in Varioskan matrix format
assay_filename <- structure("BC_0041.*varioskan_KINabsMatr.*", class="varioskan")
vs_kinabsmatr_df = LA_ImportData(assay_filename, barcode="0041", layout=TRUE, method='KINabsMatr', concUnit="mM")

printDebug("varioskan matrix data frame:")
print(head(vs_kinabsmatr_df))

### *** plate reading TECAN
# defining the filenames to be read and the class of the data
# assay_filename <- structure("BC_0063.*tecan_KINabs.*", class="tecanMagellanCSV")
# tecan_assay_df = LA_ImportData(assay_filename, method='KINabs')
# printDebug("TECAN data frame:")
# print( head(tecan_assay_df))

assay_filename <- structure("BC_0052_20150603_DAAOassay_20150427_20150601racStability_30C.xlsx", class="tecanMagellanXLSX")
tecan_assay_df = LA_ImportData(assay_filename, barcode="0052", method='KINabs')
printDebug("TECAN data frame:")
print( head(tecan_assay_df))
# now with layout and pathlength correction
tecan_assay_df = LA_ImportData(assay_filename, method='KINabs', barcode="0052", layout=TRUE, PLC=TRUE)
printDebug("TECAN data frame:")
print( head(tecan_assay_df))

### - well number generator

genWellNumbers() # A01 B01 C01 ...

genWellNumbers(rep=3) # A01 A01 A01 B01 B01 B01 B01 ...

genWellNumbers(byRows=TRUE) # A01 A02 A03 ....

genWellNumbers(byRows=TRUE, repeats=4) # A01 A01 A01 A01 A02 A02 A02 A02 A03 ....


### - path length correction factor

pathlengthCorrectionFactor() # for 200ul Vol. and std. well radius

pathlengthCorrectionFactor(liquidVolume=180)

