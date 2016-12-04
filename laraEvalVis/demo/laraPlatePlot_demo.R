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
# VERSION: 0.0.1
#
# CREATION_DATE: 2015/06/08
# LASTMODIFICATION_DATE: 2015/06/08
#
# BRIEF_DESCRIPTION: Demo of the laraDataReader package
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

library("laraDataReader")
library("laraEvalVis")
setwd("./")


### *** plate reading Thermo varioskan
# defining the filenames to be read and the class of the data
assay_filename <- structure("BC_0062.*varioskan_KINabs.*", class="varioskan")

# reading multiple Thermo single point absorption files with two wavelengths (600nm and 660nm)
# with corresponding plate layout information 
vs_assay_df = LA_ImportData(assay_filename, method='KINabs', layout=TRUE, PLC=TRUE)

printDebug("varioskan data frame:")
print(head(vs_assay_df))
print(tail(vs_assay_df))

lin_mod = calcAllLinModels(kin_data_df=vs_assay_df, wavelength=340)

best_lin_mod_df = selectBestLinModel(kin_data_df=vs_assay_df, wavelength=340)
  
class(vs_assay_df) <- "kinetics"

LA_Plot( plot_data_df=vs_assay_df, slopes_df=lin_mod, ylim=c(0.3,1.1), xlim=c(0, 400.0), description=TRUE)
# single plots
LA_Plot( plot_data_df=vs_assay_df, slopes_df=lin_mod, ylim=c(0.3,1.1), xlim=c(0, 1000.0), description=TRUE, singlePlot=T, overview=F)

LA_Plot( plot_data_df=vs_assay_df, slopes_df=best_lin_mod_df, ylim=c(0.3,1.1), xlim=c(0, 10.0), markBest=TRUE, bestNegSlope=TRUE, plotBestLinMod=TRUE,  description=TRUE)

# -------------------- Anders DAAOassay exmaple

assay_filename <- structure("0316_racemase_daao_vanillicacid.xlsx", class="tecanXLSX")
tecan_assay_df = LA_ImportData(assay_filename, method='KINabs', barcode="0316", layout=TRUE, PLC=T)

assay_filename <- structure("20150603_DAAOassay_20150427_20150601racStability_30C.xlsx", class="tecanXLSX")

tecan_assay_df = LA_ImportData(assay_filename, method='KINabs', barcode="0065", layout=TRUE, PLC=TRUE)

printDebug("Tecan/Magellan data frame:")
print(head(tecan_assay_df))
print(tail(tecan_assay_df))

lin_mod = calcAllLinModels(kin_data_df=tecan_assay_df)
# fast best model
best_lin_mod_df = selectBestLinModel(kin_data_df=tecan_assay_df)
# slow best model
best_lin_mod_df = selectBestLinModel(kin_data_df=tecan_assay_df, minIntervalSizeFraction=0.3)

class(tecan_assay_df) <- "kinetics"

LA_Plot( plot_data_df=tecan_assay_df, slopes_df=lin_mod, ylim=c(0.0,6.5), xlim=c(0, 3000.0), description=TRUE, filename="_kinPlot_lin_mod_1")

LA_Plot( plot_data_df=tecan_assay_df, slopes_df=best_lin_mod_df, ylim=c(0.0,6.5), xlim=c(0, 3000.0), filename="_kinPlot_best_lin_mod_12")

# single file plots

LA_Plot( plot_data_df=tecan_assay_df, slopes_df=best_lin_mod_df, ylim=c(0.0,6.5), xlim=c(0, 3000.0), singlePlot=T, overview=F,
              plotBestLinMod=TRUE, description=TRUE,  filename="_kinPlot_best_lin_mod_singlePlot")

#-------------------------------------------

growth_filename <- structure("BC_4693.*omega_SPabs.*", class="omega")
growth_df = LA_ImportData(growth_filename, method='SPabs', layout=TRUE, PLC=TRUE)
class(growth_df) <- "kineticsPlate"
LA_Plot( plot_data_df=growth_df, ylim=c(0.0,5.5), xlim=c(0, 180000.0), xlab="time / s", description=T )

growth_filename <- structure("BC_4694.*omega_SPabs.*", class="omega")
growth_df = LA_ImportData(growth_filename, method='SPabs', layout=TRUE, PLC=TRUE)
class(growth_df) <- "kineticsPlate"
LA_Plot( plot_data_df=growth_df, ylim=c(0.0,5.5), xlim=c(0, 180000.0),xlab="time / s", description=T )

growth_filename <- structure("BC_4695.*omega_SPabs.*", class="omega")
growth_df = LA_ImportData(growth_filename, method='SPabs', layout=TRUE, PLC=TRUE)
class(growth_df) <- "kineticsPlate"
LA_Plot( plot_data_df=growth_df, ylim=c(0.0,5.5), xlim=c(0, 180000.0),xlab="time / s", description=T  )

growth_filename <- structure("BC_4696.*omega_SPabs.*", class="omega")
growth_df = LA_ImportData(growth_filename, method='SPabs', layout=TRUE, PLC=TRUE)

class(growth_df) <- "growth"
LA_Plot(plot_data_df=growth_df, xlim=c(0,50), ylim=c(0,6), timeUnit="hours", errorBars=T, averageGrowth=T, overview=T )

### *** plate reading TECAN
# defining the filenames to be read and the class of the data
assay_filename <- structure("BC_0063.*tecan_KINabs.*", class="tecan")

assay_filename <- structure("BC_0064.*tecan_KINabs.*", class="tecan")

# reading multiple BMG omega single point absorption files with two wavelengths (600nm and 660nm)
# with corresponding plate layout information 
tecan_assay_df = LA_ImportData(assay_filename, method='KINabs')
tecan_assay_df = LA_ImportData(assay_filename, method='KINabs', layout=TRUE, PLC=TRUE)

printDebug("TECAN data frame:")
print( head(tecan_assay_df))
