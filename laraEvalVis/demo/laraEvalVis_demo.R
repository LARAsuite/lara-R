#_____________________________________________________________________________
#
# PROJECT: LARA
# CLASS: 
# FILENAME: laraEvalVis_demo.R
#
# CATEGORY:
#
# AUTHOR: mark doerr
# EMAIL: mark@ismeralda.org
#
# VERSION: 0.1.0
#
# CREATION_DATE: 2015/06/01
# LASTMODIFICATION_DATE: 2015/08/31
#
# BRIEF_DESCRIPTION: Demo of the laraEvalVis package
# DETAILED_DESCRIPTION: 
# HISTORY: 
# INSTALL: 
#          library("devtools")
#          document("laraEvalVis")
#          install("laraEvalVis")
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
library("laraEvalVis")

setwd("./")

## Michaelis-Menten Kinetics Demo

ext_coeff_Substr = 15800

assay_df = LA_ImportData( structure("BC_0070.*varioskan_KINabsMatr.*", class="varioskan"), 
                          method='KINabsMatr', barcode="0070", layout=TRUE, PLC=TRUE)

trunc_assay_df <- assay_df[assay_df$DiffTime < as.difftime(400, units="secs"),]
print("varioskan trunc data frame:")
print(tail(trunc_assay_df,120))

# averaging contracting to means
av_df = averageByGroups(non_corr_df=trunc_assay_df)

# blank substraction
av_bs_df = substractBlank(non_corr_df=av_df)

# finding v_max for all  
vmax_corr_df = selectBestSlopeModel(kin_data_df=av_bs_df, modelVar=c("DiffTime", "AbsMeanCorr"), 
                                    groupingVar="Description", wavelength=380, minIntervalSizeFraction=0.5)

# conversion of units from Abs/s to M/s
vmax_corr_df$Slope = vmax_corr_df$Slope / ext_coeff_Substr

# adding required info for Michaelis-Menten calculations (like Substr. Concentrations), adjusting concentration units
mm_corr_df <- addInfo(data_df=vmax_corr_df, info_df=av_bs_df ) 

# central Michaelis-Menten fit function
mm1_lst = michaelisMentenKinetics(mm_df=mm_corr_df)

#class(mm1_corr_df) <- "data.frame"

# writing the data to a file
write.csv(mm1_corr_df, file ="mm1_raw.csv")
write.csv(mm1_lst, file ="mm_fit.txt")

# and now the Michaelis-Menten plots
class(mm1_corr_df) <- "MMkinetics"

plot_xlim = c(0,1.1e-3) 
plot_ylim = c(1e-8, 3.5e-7)

LA_PlatePlot(plot_data_df=mm1_corr_df, mm_lst=mm1_lst, xlim=plot_xlim, ylim=plot_ylim, filename="MM_plot_1.png")

## Spectral Eval Demo

spectrum_filename <- structure("BC_0002.*varioskan_SPECabs.*", class="varioskan")

spectrum_df = LA_ImportData(spectrum_filename, method='SPECabs', barcode="0002", wlStart=350, wlEnd=700, layout=TRUE, PLC=FALSE)
class(spectrum_df) <- "data.frame"
head(spectrum_df, 120 )
tail(spectrum_df, 120 )

class(spectrum_df) <- "spectrum"

plot_xlim = c(300, 700) 
plot_ylim = c(0, 4.0)
LA_Plot(plot_data_df=spectrum_df, xlim=plot_xlim, ylim=plot_ylim, barcode="0002", singlePlot=F, overview=T, overlay=F)

# grouped overlay plot
LA_Plot(plot_data_df=spectrum_df, xlim=plot_xlim, ylim=plot_ylim, barcode="0002", singlePlot=T, overview=F, overlay=T, plotType="l", filename="PhRedSpectrum")

# growth plots

assay_filename <- structure("BC_3275_20150820.*varioskan_SPabs.*", class="varioskan")
growth_df = LA_ImportData(assay_filename, method='SPabs', barcode="3275", useTime=FALSE, layout=TRUE, PLC=TRUE)
head(growth_df, 32)
tail(growth_df, 192)
class(growth_df) <- "growth"

LA_Plot(plot_data_df=growth_df, xlim=c(0,80), ylim=c(0,6), timeUnit="hours", errorBars=F, averageGrowth=F)
LA_Plot(plot_data_df=growth_df, xlim=c(0,80), ylim=c(0,6), timeUnit="hours", errorBars=T, averageGrowth=T, filename="_av_growth_error_bars")
LA_Plot(plot_data_df=growth_df, xlim=c(0,20), ylim=c(0,4), numGrowth=3, timeUnit="hours", errorBars=T, averageGrowth=T, overview=TRUE, filename="_av_growth_error_bars")

LA_Plot(plot_data_df=growth_df, xlim=c(0,20), ylim=c(0,4), numGrowth=3, timeUnit="hours",  overview=TRUE, filename="_well_plot")


class(growth_df) <- "data.frame"
growth_df = LA_ImportData(assay_filename, method='SPabs', barcode="3275", useTime=FALSE, layout=TRUE, PLC=T)

growth_df <- growth_df[order(growth_df$NumReads, growth_df$Well),]
print(tail(growth_df, 98))
class(growth_df) <- "barplot3d"
LA_Plot(plot_data_df=growth_df, wavelength=600, fillCol="bluered", filename="_well_plot3D")

# chlorella

assay_filename <- structure("BC_4595.*varioskan_SPabs.*", class="varioskan")
growth_df = LA_ImportData(assay_filename, method='SPabs', barcode="4595", useTime=FALSE, layout=TRUE, PLC=TRUE)

print(tail(growth_df, 98))

class(growth_df) <- "growth"

LA_Plot(plot_data_df=growth_df, xlim=c(0,45), ylim=c(0,3), timeUnit="hours", errorBars=F, averageGrowth=F, PLC=TRUE)
LA_Plot(plot_data_df=growth_df, xlim=c(0,45), ylim=c(0,3), timeUnit="hours", errorBars=F, overview=TRUE, averageGrowth=F, PLC=TRUE)

LA_Plot(plot_data_df=growth_df, xlim=c(0,45), ylim=c(0,3), timeUnit="hours",  plot_description=TRUE, errorBars=T, averageGrowth=T, PLC=TRUE, filename="_av_growth_error_bars")

assay_filename <- structure("BC_4722.*varioskan_SPabs.*", class="varioskan")
growth_df = LA_ImportData(assay_filename, method='SPabs', barcode="4722", useTime=FALSE, layout=TRUE, PLC=TRUE)

class(growth_df) <- "growth"

LA_PlatePlot(plot_data_df=growth_df, xlim=c(0,60), ylim=c(0,6), timeUnit="hours", errorBars=F, averageGrowth=F)
LA_PlatePlot(plot_data_df=growth_df, xlim=c(0,60), ylim=c(0,6), timeUnit="hours", description=TRUE, errorBars=T, averageGrowth=T, filename="_av_growth_error_bars")

assay_filename <- structure("BC_4723.*varioskan_SPabs.*", class="varioskan")
growth_df = LA_ImportData(assay_filename, method='SPabs', barcode="4723", useTime=FALSE, layout=TRUE, PLC=TRUE)

class(growth_df) <- "growth"

LA_PlatePlot(plot_data_df=growth_df, xlim=c(0,80), ylim=c(0,6), timeUnit="hours", errorBars=F, averageGrowth=F)
LA_PlatePlot(plot_data_df=growth_df, xlim=c(0,80), ylim=c(0,6), timeUnit="hours", errorBars=T, averageGrowth=T, filename="_av_growth_error_bars")

# Assay plots

assay_df = LA_ImportData(structure("BC_3295.*varioskan_KINabs.*", class="varioskan"), method='KINabs', barcode="3295", layout=TRUE, PLC=TRUE)
head(assay_df,120)
# blank substraction
#av_bs_df = substractBlank(non_corr_df=av_df)

# finding v_max for all

slopes_df = selectBestLinModel(kin_data_df=assay_df)
slopes_df


#slopes_df <- addInfo(data_df=slopes_df, info_df=assay_df ) 

class(assay_df) <- "kinetics"
LA_Plot( plot_data_df=assay_df, slopes_df=slopes_df, ylim=c(0.0,0.6), xlim=c(0, 800.0), markBest=TRUE, plotBestLinMod=TRUE, plotRef=TRUE,  description=TRUE)


class(assay_df) <- "data.frame"
head(assay_df)

# The END 
# ------
LA_PlatePlot(plot_data_df=growth_df, xlim=c(0,80), ylim=c(0,6), timeUnit="hours", errorBars=T, singlePlot=T, overview=F, averageGrowth=T)



assay_filename <- structure("BC_3174.*omega_SPabs.*", class="omega")
growth_df = LA_ImportData(assay_filename, method='SPabs', barcode="3174",  layout=TRUE, PLC=TRUE)

class(growth_df) <- "growth"

LA_PlatePlot(plot_data_df=growth_df, numGrowth=6, xlim=c(0,20), ylim=c(0,6), timeUnit="hours", errorBars=T, averageGrowth=T)

LA_PlatePlot(plot_data_df=growth_df, numGrowth=6, xlim=c(0,20), ylim=c(0,6), timeUnit="hours", errorBars=T, averageGrowth=F)

system.time(LA_PlatePlot(plot_data_df=growth_df, numGrowth=6, xlim=c(0,20), ylim=c(0,6), timeUnit="hours", errorBars=T, averageGrowth=T))


LA_PlatePlot(plot_data_df=growth_df, xlim=c(0,50), ylim=c(0,6), timeUnit="hours")

LA_PlatePlot(plot_data_df=growth_df, xlim=c(0,50), ylim=c(0,6), timeUnit="hours", averageGrowth=TRUE)
LA_PlatePlot(plot_data_df=growth_df, xlim=c(0,50), ylim=c(0,6), timeUnit="hours", averageGrowth=TRUE, overview=FALSE)
LA_PlatePlot(plot_data_df=growth_df, xlim=c(0,50), ylim=c(0,6), timeUnit="hours", averageGrowth=TRUE, singlePlot=TRUE)

## Phenol Red Assay Demo

assay_filename <- structure("BC_4480_20150611.*varioskan_SPabs.*", class="varioskan")

assay_df = LA_ImportData(assay_filename, method='SPabs', barcode="4480", layout=TRUE, PLC=FALSE)

phr_result_df = phenolRedAssay(assay_df=assay_df)

class(phr_result_df) <- "wells"

LA_PlatePlot(plot_data_df=phr_result_df, barcode="4480")

#### chromatograms

hplc_filename <- structure("20150717_Atu_D-Ala.ctx", class="MerckLaChrom")
hplc_df = LA_ImportData(hplc_filename, method='HPLCabs', barcode="7071", numSamples=9000, wavelength=330)

class(hplc_df) = "chromatogram"
LA_Plot(plot_data_df=hplc_df, plotPeaks=T)

# HPLC in CSV format

hplc_filename <- structure("HPLC_biocat_B2t30min.csv", class="CSV")
hplc_df = LA_ImportData(hplc_filename)

head(hplc_df)

class(hplc_df) = "chromatogram"
LA_Plot(plot_data_df=hplc_df, plotPeaks=T, charScale=1.7)

# Kinetics in CSV format

kin_filename <- structure("Acy1_activities_HPLC.csv", class="CSV")
kin_df = LA_ImportData(kin_filename)

class(kin_df) = "data.frame"
head(kin_df,12)

class(kin_df) = "kinetics"
LA_Plot(plot_data_df=kin_df, plot_overlay=T, xlim=c(0,35), ylim=c(0,50000),
        groupingVar="Description", plot_overview=F, plotLegend=T, plot_description=F, charScale=1.7, barcode="0004", ylab="fluorescence (RFU)", xlab="time (min)",  plotQuality=100, filename="DLA_Acy1_act")

LA_Plot(plot_data_df=kin_df, plot_overlay=T, xlim=c(0,35), ylim=c(0,50000),
        groupingVar="Description", plot_overview=F, plotLegend=T, plot_description=F, charScale=0.5, cex=1.0, barcode="0004", ylab="fluorescence (RFU)", xlab="time (min)",  plotQuality=100, filename="DLA_Acy1_act", outputFormat="eps")


LA_Plot(plot_data_df=kin_df, plot_overlay=T, xlim=c(0,35), ylim=c(0,50000),
        groupingVar="Description", plot_overview=F, plotLegend=T, plot_description=F, xlab="time / min",  plotWidth=11, plotHeight=8, filename="DLA_Acy1_act", outputFormat="svg")


### histograms

barcode <- "0543"

fl_0543_df <- LA_ImportData(structure("_0543__20141020", class='varioskan'), layout=T,  method="SPfl" )
fl_0543_df$Slope <- fl_0543_df$Value
head(fl_0543_df)

class(fl_0543_df) <- "barplot3d"
LA_Plot(plot_data_df=fl_0543_df, scale=0.5, barcode="0543" , filename="fl_demo_3D")

LA_Plot(plot_data_df=fl_0543_df, scale=0.5, fillCol="lightgreen", barcode="0543" , filename="fl_demo_3D")

LA_Plot(plot_data_df=fl_0543_df, scale=0.5, fillCol="red", barcode="0543" , filename="fl_demo_3D")

class(fl_0543_df) <- "data.frame"

# fl histogram

hist_dat_df <- fl_0543_df

ctrl_wells = c(27, 34, 63, 70)

hist_dat_df[ctrl_wells,]$Value = NA

hist_dat_df$Ratio <- hist_dat_df$Value / mean(hist_dat_df$Value, na.rm=T)

hist_dat_df

class(hist_dat_df) <- "histogram"
LA_Plot(plot_data_df=hist_dat_df, plotVar=c('Ratio'), breaks=15, xlim=c(0.0,6.0), ylim=c(0.0,30.0), fillCol="lightgreen",
        fitting=T, barcode=barcode, filename="fl", outputFormat="svg", xlab="RFU / <RFU>", plotWith=NULL, plotHeight=NULL)


### barplots

class(hist_dat_df) <- "barplot"
LA_Plot(plot_data_df=hist_dat_df)

LA_Plot(plot_data_df=hist_dat_df, fillCol="orange", plotRef=TRUE)

## growth plot

barcode <- "3275"
growth_df <- LA_ImportData(structure(paste("BC_", barcode, "_", sep=""), class='varioskan'), method="SPabs", layout=T, PLC=F)
head(growth_df, 96)

class(growth_df) <- "barplot"
LA_Plot(plot_data_df=growth_df)


class(growth_df) <- "barplot3d"

LA_Plot(plot_data_df=growth_df, wavelength=600)

class(growth_df) <- "data.frame"


### top N plots

class(hist_dat_df) <- "barplot"
LA_Plot(plot_data_df=hist_dat_df, topN=10)

LA_Plot(plot_data_df=hist_dat_df, topN=15, fillCol=c("red", "orange", "yellow"), plotRef=TRUE)
