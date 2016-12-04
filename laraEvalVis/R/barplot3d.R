#'_____________________________________________________________________________
#
# PROJECT: LARA
# CLASS: 
# FILENAME: barplot3d.R
#
# CATEGORY:
#
# AUTHOR: mark doerr
# EMAIL: mark@ismeralda.org
#
# VERSION: 0.1.0
#
# CREATION_DATE: 2015/09/08
# LASTMODIFICATION_DATE: 2015/09/08
#
# BRIEF_DESCRIPTION: Library for plotting microtiter plate data in 3D representation
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


#' barplot3d
#'
#' Plotting 3D Bars
#' @description plots data from a (heights x rows x columns ) array
#' @param heigths_arr
#' @keywords plate readers
#' @export 
#' @examples
#'  barplot3d()
#' 
#' @note todo: 
#' 

barplot3d <- function(heights_arr=NULL, barcode = "0000", filename = "_3Dbarplot",
                      column_colours = 'bluered', numCol=20, zmax=1.0, nticks=4, moreLight=FALSE, axisBack=TRUE,
                      theta=-60, phi=15, scale=1.0, char_scale=1.1, width=700, height=900, transp=1.0,
                      col_lab=NULL, row_lab=NULL, z_lab=NULL,
                      barWidth=0.5, barDistance=0.8, 
                      plotBg='white',
                      outputFormat='screen')
{
  require(rgl)
  # **** RGL settings
  #rgl.open()
  rgl.init()
  
  open3d()
  par3d(windowRect=c(0, 0, width, height))
  save <- par3d(skipRedraw=TRUE)
  on.exit(par3d(save))
  
  # Choosing a background
  rgl.bg(color=plotBg) #rgl.bg(color="gray")  #rgl.bg(col="#cccccc") lightgrey
  
  if (isTRUE(moreLight)) rgl.light() else {
    # removing reflections
    rgl.pop("lights") 
    light3d(specular="black") 
  }

  title3d('','', ylab=z_lab, outher=TRUE) #title3d('','',xlab='xlab',ylab='Abs/AU',zlab='ylab')
  
  # alpha is transparency
  
  # **** scaling hights array
  heights_arr <- heights_arr * scale
 # print(heights_arr)
  
  # **** selecting color scheme 
  # factor to spread colours over the whole range and +1 to prevent indexing by 0
  num_col <- ( max(heights_arr, na.rm=TRUE) * numCol ) + 1
  
  switch(column_colours,
         bluered={column_colours <-colorRampPalette(c("blue","green","yellow","red"))(num_col)},
         rainbow={column_colours <-rainbow(num_col)},
         green={column_colours <-colorRampPalette(c("yellow","green","darkgreen"))(num_col)},
         lightgreen={column_colours <-colorRampPalette(c("yellow","lightgreen","green"))(num_col)},
         red={column_colours <-colorRampPalette(c("yellow","red","darkred"))(num_col)},
         lightred={column_colours <-colorRampPalette(c("yellow","orange","red"))(num_col)},
         orange={column_colours <-colorRampPalette(c("yellow","red","darkred"))(num_col)},
         blue={column_colours <-colorRampPalette(c("lightblue","blue","darkblue"))(num_col)},
         grey={column_colours <-colorRampPalette(c("white", "lightgrey","grey"))(num_col)},
         lightgrey={column_colours <-colorRampPalette(c("white", "lightgrey"))(num_col)},
         violet={column_colours <-colorRampPalette(c("lightgrey","grey", "violet"))(num_col)},
         heat={column_colours <- rev(heat.colors(num_col))}
  )
  
  nrows <- as.integer(dim(heights_arr)[1])
  ncols <- as.integer(dim(heights_arr)[2])
  nheights <- as.integer(dim(heights_arr)[3])
  
  printDebug("barplolt3D(DEBUG): n nheights: %s - cols :%s - rows: %s", nheights, ncols,  nrows )
  
  # spawning a 3D matrix for corner coordinates of the cuboids (default:12x8x2+n*z)
  ncubes = nrows * ncols
    
  x_distance <- barWidth + barDistance
  y_distance <- barWidth + barDistance
  
  # lowering dimension to matrix
  ##cube_matr <- matrix(heights_arr, c(nheights, ncubes))
  
  ## adding y-coordinates
  ##cube_matr <- rbind(rep(seq(0,(ncols-1)*y_distance, y_distance), nrows), cube_matr )
  ## adding x-coordinates
  ##cube_matr <- rbind(rep(seq((nrows-1)*x_distance,0, -x_distance), each=ncols) , cube_matr )
  
  # generating y-coordinates
  y_vec <- rep(seq(0,(ncols-1)*y_distance, y_distance), each=nrows)
  
  # generating x-coordinates
  x_vec <- rep(seq((nrows-1)*x_distance,0, -x_distance), ncols)

  # combining everything to one array (=stacking the arrays)
  cube_arr  <- array(c(x_vec, y_vec, heights_arr), dim=c(nrows, ncols , nheights+2))

  #print(cube_arr)  

  cubeFactory <-function(cube_vec, cube_width, curr_colour)
  {  
    # geometry of the cube
    xw <- cube_width
    yw <- cube_width
    
    # lower left corner of cube and height differences
    x1 <- cube_vec[1]
    y1 <- cube_vec[2]
    z0 <- cube_vec[3]
    z1 <- cube_vec[4]
    
    # cube face color
    curr_colour <- column_colours[(z1* numCol )+1]
    
    right_side <- matrix(c(x1,x1,x1,x1,z0,z0,z1,z1,y1,y1+yw,y1+yw,y1), 4,3)
    front_side <- matrix(c(x1,x1+xw,x1+xw,x1,z0,z0,z1,z1,y1,y1,y1,y1), 4,3)
    
    # transformation matrices
    y_tr  <- matrix(c(0,0,0,0,0,0,0,0,yw,yw,yw,yw), 4,3)
    x_tr  <- matrix(c(xw,xw,xw,xw,0,0,0,0,0,0,0,0), 4,3)
    
    # transformation of two cube plains (front side, rigth side) in both directions
    left_side <- right_side + x_tr
    back_side <- front_side + y_tr 
    
    rgl.quads(rbind(right_side,left_side,front_side,back_side), col=curr_colour)
  }
  
  # helper function for preparation of the cuboid colums
  axis <- TRUE  
  plotCuboidColumn <-function(cube_vec=NULL, cube_width=0, column_colours=NULL, numCol=20)
  {  
    #print(cube_vec)
    
    # cube geometry
    xw <- cube_width
    yw <- cube_width
    
    # lower left corner and height differences
    x1 <- cube_vec[1]
    y1 <- cube_vec[2]
    z0 <- cube_vec[3] # cuboid bottom height
    z1 <- cube_vec[length(cube_vec)] # length(cube_vec) = index of cuboid lid hight
    
    # checking Z values and melting the coordinates to one matrix containing all cube coordinates
    checknStackZ <- function(z_idx)
    {
      if(cube_vec[z_idx] < cube_vec[z_idx+1])  return( c(x1, y1, cube_vec[z_idx], cube_vec[z_idx+1]) ) else {
        printError("WARNING(barplot3d): z cube coordinate is bigger then z+1 coordinate, please check cube stacking order for z=%s!", z_idx )
        return( c(x1, y1, cube_vec[z_idx], cube_vec[z_idx]) )
      }
    }
    
    num_heights <- length(cube_vec) - 1 

    curr_colour <- column_colours[(z1*numCol)+1]
    # plotting stack of cubes
    invisible(apply( sapply((3:num_heights), checknStackZ ), 2, cubeFactory, cube_width, curr_colour ))
    
    # plotting bottom and lid
    bottom <- matrix(c(x1,x1+xw,x1+xw,x1,0,0,0,0,y1,y1,y1+yw,y1+yw), 4,3)
    z_tr  <- matrix(c(0,0,0,0,z1,z1,z1,z1,0,0,0,0), 4,3)
    
    #curr_colour <- column_colours[(z1*numCol)+1]
    
    rgl.quads(bottom, col=curr_colour)
    # and now the lid
    rgl.quads(bottom+z_tr, col=curr_colour)
    #plot only axis for first plot
    if (axis == TRUE) {axis <<- FALSE }
  } 
  # plotting the cubes
  apply(cube_arr, c(1,2), plotCuboidColumn, barWidth, column_colours )

  rgl.points(0,zmax,0)

  # adding the axes and grid
  #axes3d(c('x','y','z'), expand=1.00, colour="black")
  #pos=c(0, -0.8, -0.8), eax=0.3
 
  if(isTRUE(axisBack)){
    grid3d(side=c('x+-'), col="gray")
    axis3d(edge=c("y+-"), color="black", cex=char_scale, nticks=nticks) # axis in back
  } else {
    grid3d(side=c('x','z'), col="gray")
    axes3d(edge=c('y'), expand=1.00, color="black", cex=char_scale, nticks=nticks) # axis in front   
  }
  
 # grid3d(col="gray")
 
  # well coordinates 
  row.names <- LETTERS[1:nrows]
  col.names <- as.character(1:ncols)

  text3d(x=seq((nrows-1)*x_distance,0, -x_distance)+0.2, y=-0.2, z=-0.5, texts=row.names ,col="black", cex=char_scale)
  text3d(x=-0.6, y=-0.2, z=seq(0,(ncols-1)*y_distance, y_distance), texts=col.names ,col="black", cex=char_scale)

  ##par3d(userMatrix=um, FOV=19.28572,  c(0,0,1200,800)) # projection !
  par3d( FOV=19.28572)
  view3d(theta = -115, phi=25)
  
  switch(outputFormat,
         screen={},
         webGL={ out_filename_3D <- paste(barcode, "_", filename, ".html", sep="" )
                 cat("webGL file: ", out_filename_3D,"\n")
                 cat("webGL file path: ", system.file(file.path("WebGL", "template.html"), package = "rgl") ,"\n")
                 
                 writeWebGL( dir="webGL", filename=file.path("webGL", out_filename_3D),  
                       template = system.file(file.path("WebGL", "template.html"), package = "rgl"), 
                       width=width, height=height) 
               },
         png={ out_filename_3D <- paste(barcode,"_", filename, ".png", sep="" )
              rgl.snapshot(out_filename_3D) },
         svg={ out_filename_3D <- paste(barcode, "_", filename, ".svg", sep="" )
               rgl.postscript( filename=out_filename_3D, fmt="svg", drawText=TRUE ) },
         { printError("ERROR(LA_Plot.wells): unknown output format (%s) specified ! Supported formats: 'screen', 'webGL', 'png', 'svg' !", outputFormat) }
  )
  
  if(outputFormat %in% c('webGL', 'svg', 'png') ) rgl.close()
  return(TRUE)
}

#' LA_Plot.barplot3d
#'
#' @title Plotting 3D bars of Measurements in Microtiter Format
#' @description
#'    Wrapper function for 3D bar plotting 
#' @param heigths_arr
#' @keywords plate readers
#' @export 
#' @examples
#'  platePlot3d()
#' 
#' @note todo: 
#' 

LA_Plot.barplot3d <- function( plot_data_df=NULL, num=0, zmax=1.0, ...)
{
  require(rgl)
  print("barplot3d 0.9i")
  printDebug("barplolt3D(DEBUG): plot width %s, height: %s", plot_width, plot_height )
 # plot_zlab = 'Absorption / AU'
  
  if (is.null(plot_var) ) plot_var = c("Value", "RefTime")
  
  if(current_filename == 'LA_PLot') current_filename <- "3Dbarplot"
  
  # reverting original classes
  if ( ! is.null(plot_data_df)) class(plot_data_df) <- "data.frame" else return(FALSE)
    
  # removing neg. values
#   if( min(plot_data_df$Slope, na.rm=T) < 0 ) {
#     plot_data_df[plot_data_df$Slope <0, ] <- 0
#     plot_data_df[plot_data_df$Value <0, ] <- 0    
#   }

  if(current_wavelength == 0) printError("WARNING(plotePlot3D): No wavelength specified !")
  
  umatrix = matrix(c( -0.4596869, 0.01776623,  0.8879033,    0,
                      0.3495693, 0.92270774,  0.1625170 ,   0,
                      -0.8163879 ,0.38509068 ,-0.4303671 ,   0,
                      0.0000000, 0.00000000 , 0.0000000  ,  1), c(4,4))
 
  if(isTRUE(plot_preview))  axisBack <- FALSE  else axisBack <- TRUE
    
  # converting data frame to array for fast plotting
  heights_arr <- readerDF2Array(source_data_df=plot_data_df, wavelength=current_wavelength,num=num, plotVar=plot_var)
  
  barplot3d(heights_arr=heights_arr, barcode=current_barcode, filename=current_filename,
            zmax=zmax, column_colours=fill_colours, plotBg=plot_bg, numCol=num_colours, char_scale, moreLight=plot_preview, axisBack=axisBack,
            theta=plot_theta, phi=plot_phi, scale=plot_scale, width=plot_width, height=plot_height, transp=plot_transparence,
            col_lab=plot_xlab, row_lab=plot_ylab, z_lab=plot_zlab,
            barWidth=plot_bar_width, barDistance=plot_bar_distance,
            outputFormat="screen")
  
  # ! to save a plot to file, first plot to screen and finally save it 
  switch(plot_output_format,
         screen={  par3d(windowRect=c(0,0,1024,900))
                   par3d(userMatrix=t(umatrix), zoom= 0.80) },
         webGL={ out_filename_3D <- paste(current_barcode,"_", current_filename, ".html", sep="" )
                 writeWebGL( dir="webGL", filename=file.path("webGL", out_filename_3D),  
                             template = system.file(file.path("WebGL", "template.html"), package = "rgl"), 
                             width=plot_width, height=plot_height) 
         },
         png={ out_filename_3D <- paste(current_barcode,"_", current_filename, ".png", sep="" )
               par3d(windowRect=c(0,0,plot_width,plot_height))
               par3d(userMatrix=t(umatrix), zoom= 0.80)
               rgl.snapshot(out_filename_3D) },
         svg={ out_filename_3D <- paste(current_barcode, "_", current_filename, ".svg", sep="" )
               rgl.postscript( filename=out_filename_3D, fmt="svg", drawText=TRUE ) },
        { printError("ERROR(LA_Plot.wells): unknown output format (%s) specified - supported formats: 'screen', 'webGL', 'png', 'svg' !", outputFormat) }
  )
  
  if (plot_output_format != "screen" ) rgl.close()
  return(TRUE)
}
