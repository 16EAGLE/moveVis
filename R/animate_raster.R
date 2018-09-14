#' Animate raster data
#'
#' \code{animate_raster} animates raster data provided as list of \code{raster} class objects. The function creates an animated GIF or video file and saves it into the output directory.
#'
#' @inheritParams animate_move
#' @param layer list. List of raster objects.
#' @param layer_dt POSIXct or vector. Optional vector of POSIXct date/time stamps corresponding to the acquisition dates of the \code{layer} raster objects to display a time scale.
#' @param ... additional arguments, see \link{animate_move}.
#' @return None or logical (see \code{log_logical}). The output GIF or video file is written to the ouput directory.
#' 
#' @details \code{animate_raster} is based on \code{ggplot2}. Depending on the selected output format (\code{out_format}, it either needs the \code{convert} tool of the ImageMagick software package (.gif format) or either \code{ffmpeg} from FFmpeg or \code{avconv} from libav (video formats). The command or directory to the convert tool needs to be provided with \code{conv_dir}. Please use \code{\link{get_libraries}} to search for the needed libraries and command/tool directories on your system or to automatically download and install the required software. See \code{\link{get_libraries}} and \code{out_format} and \code{conv_dir} for details.
#' 
#' @examples
#' \dontrun{
#' #Create a list of several raster objects to be displayed one after another
#' #If layer_type = RGB, use a brick class obejct with RGB bands!
#' data("basemap_data") #example MODIS dataset
#' layer <- basemap_data[[1]] #list of rasters
#' 
#' #Get your convert directory/command
#' conv_dir <- get_libraries()
#' 
#' #Specify the output directory, e.g.
#' out_dir <- "/out/test"
#' 
#' #Call animate_raster
#' animate_raster(layer,out_dir = out_dir, conv_dir = conv_dir, layer_type = "gradient",
#'                out_format = "gif")
#'                
#' #use another file format for longer videos
#' animate_raster(layer,out_dir = out_dir, conv_dir = conv_dir, layer_type = "gradient",
#'                out_format = "mov")
#' }
#' 
#' @author Jakob Schwalb-Willmann
#' @seealso \code{\link{get_libraries}}, \code{\link{animate_move}}, 
#' 
#' @export

animate_raster <- function(layer, out_dir, conv_dir = "convert", layer_dt = NULL, layer_type = "gradient", layer_stretch = "none",
                           layer_col = c("sandybrown","white","darkgreen"), layer_nacol = "white", ...){
  if(is.null(layer_dt)){
    time_scale <- FALSE
    layer_dt <- seq.POSIXt(as.POSIXct("2000-01-01"),by=1,length.out=length(layer))
  }else{
    time_scale <-  TRUE
  }
  
  #Call animate_move (alias function)
  animate_move(raster_only = TRUE, layer = layer, layer_dt = layer_dt, layer_stretch = layer_stretch,
               out_dir = out_dir, conv_dir = conv_dir, layer_type = layer_type,
               layer_col = layer_col, layer_nacol = layer_nacol, ...)
}