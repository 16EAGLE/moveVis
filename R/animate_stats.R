#' Animate movement data statistics
#'
#' \code{animate_stats} animates statistic plot from movement data provided as \code{move} class objects or a list of them and basemap data provided as \code{raster}. It extracts basemap values of pixels that are part of the movement paths and visualizes frequencies per value. The function creates an animated GIF file and saves it into the output directory. See also \code{\link{animate_move}}. 
#'
#' @param data_ani list or \code{moveStack} class object. Needs to contain one or several \code{move} class objects (one for each individual path to be displayed) containing point coordinates, timestamps, projection and individual ID.
#' @param out_dir character. Output directory for the GIF file creation.
#' @param conv_dir character. Command or directory to call the ImageMagick convert tool (default to be \code{convert}). You can use \code{conv_dir = get_imconvert()} to search for the right command/tool directory and/or get the required software.
#' @param layer raster, list or character. Single raster object or list of raster objects to be used as (dynamically changing) basemap layer. Default is \code{"basemap"} to download a static basemap layer. Use a rasterBrick class object and set layer_type to "\code{RGB}" to compute a RGB basemap.
#' @param layer_dt POSIXct or list. Single POSIXct date/time stamp or list of POSIXct date/time stamps corresponding to the acquisition dates of the \code{layer} raster objects.
#' @param layer_int logical. Whether to interpolate the basemap layer objects over time, if several are provided (\code{TRUE}), or to display them one after another depending on the animation time frame that is displayed (\code{FALSE}). Default is \code{FALSE}.
#' @param layer_type charachter. Layer type. Can be either "\code{RGB}" (if layer is a rasterBrick class onejct), "\code{gradient}" or "\code{discrete}". Default is "\code{RGB}". Ignored, if \code{layer = "basemap"}.
#' @param val_limits numeric vector. Fixed minimum and maximum limit values of the basemap value range (gradient layer type). Default is NA for data-depending minimum and maximum values. Ignored, if \code{layer_type} is "discrete" or "RGB".
#' @param paths_col character vector. Colours of the individual animation paths. If set to "auto", a predfined colour set will be used. If single colour, all paths will be displayed by the same colour. If more individuals then colours, the colours are repeated.
#' @param paths_mode character vector. Mode to be used for dealing with time information when displaying multiple individual paths. See \code{\link{animate_move}} for details. Default is "true_data".
#' @param stats_type character. Defines which standard plot design should be used. Select either "line" or "bar". Ignored, if \code{stats_gg} is used.
#' @param stats_gg character. Enables usage of \code{ggplot2} syntax for plot design. If set, \code{stats_type} is ignored. See \code{details} for information on the statistic data structure to be used by the user defined plot function.
#' @param stats_digits numeric. Defines how detailed the statistic plot should be as number of decimals. Values with more decimals are rounded. Default is 1 for one decimal.
#' @param stats_tframe numeric. Defines the temporal range of the periodic stats plot. Default is 5 meaning that five time frames back from the displayed frame are evaluated.
#' @param stats_title character vector. Optional plot titles. Two character strings within a vector.
#' @param frames_layout matrix. Optional layout. Define, which plots should be placed where using a matrix represnting the GIF frame. Matrix elements can be the following plot identifiers: "map" for the spatial plot, "st_all", "st_per" for the overall and periodic stats plot or "st_allR", "st_perR", "st_allG", "st_perG", "st_allB", "st_perB" for the overall and periodic stats plots per band, when using \code{layer_type = "RGB"}, and 'st_leg' for a stats legend. Alternatively, integers from 1 to 8 corresponding to the described order can be used. Plots not mentioned using \code{frames_layout} identifiers are not displayed. If set to 0, layout is generated automatically. Default is 0.
#' @param frames_nmax numeric. Number of maximum frames. If set, the animation will be stopped, after the specified number of frames is reached. Default is 0 (displaying all frames).
#' @param frames_interval numeric. Duration, each frame is displayed (in seconds). Default is .04.
#' @param frames_nres numeric. Interval of which frames of all frames should be used (nth elements). Default is 1 (every frame is used). If set to 2, only every second frame is used.
#' @param frames_width numeric. Number of pixels of frame width. Default is 600.
#' @param frames_height numeric. Number of pixels of frame height. Defualt is 600.
#' @param out_name character. Name of the output file. Default is "final_gif".
#' @param log_level numeric. Level of console output given by the function. There are three log levels. If set to 3, no messages will be displayed except erros that caused an abortion of the process. If set to 2, warnings and errors will be displayed. If set to 1, a log showing the process activity, wanrnings ans errors will be displayed.
#' @param log_logical logical. For large processing schemes. If TRUE, the function returns TRUE when finished processing succesfully.
#' @param ... optional arguments.
#' 
#' @return None or logical (see \code{log_logical}). The output GIF file is written to the ouput directory.
#' 
#' @details \code{animate_stats} is a wrapper function of \code{\link{animate_move}} to create single statistic plots without spatial plotting. For statistic plot animations sidy-by-side with spatial plot animations, use \code{\link{animate_move}} (see \code{stats_create} argument). The function can handle all arguments taken by \code{animate_stats} as well.
#' Use \code{stats_gg} to provide an own ggplot2 plot design as shown in the examples. The statistics are stored for both plots (periodic and accumulated) withn the variable \code{pdat} (list of two, indexed by k ranging from 1 to 2 for each plot). Both \code{pdat} lists contain the stats elements framewise for each time step. For this, see the \code{stats_gg} example. The variable \code{cols} (list of two, one per plot) contains the defined colour values and namings. 
#' 
#' @examples
#' \dontrun{
#' #Load move and moveVis packages
#' library(move)
#' library(moveVis)
#' 
#' #Get the sample data from the moveVis package
#' data("move_data")
#' move_data$dt <- as.POSIXct(strptime(move_data$dt, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
#' 
#' #Create moveStack object including multiple individuals
#' data_ani <- move(move_data$lon, move_data$lat, proj=CRS("+proj=longlat +ellps=WGS84"),
#'                  time = move_data$dt, animal=move_data$individual, data=move_data)
#'  
#' #Load basemap MODIS NDVI data
#' data("basemap_data")
#' layer = basemap_data[[1]]
#' layer_dt = basemap_data[[2]]
#' 
#' #Find command or directory to convert tool of ImageMagick
#' conv_dir <- get_imconvert()
#' 
#' #Specify the output directory, e.g.
#' out_dir <- "/out/test"
#' #or to a temporary directory:
#' out_dir <- paste0(tempdir(),"/test")
#' dir.create(out_dir)
#' 
#' 
#' #Call animate_stats()
#' animate_stats(data_ani, out_dir, conv_dir = conv_dir,
#'               layer=layer, layer_dt = layer_dt, layer_type = "gradient",
#'               stats_digits = 1, stats_type = "bar", out_name = "final_gif",
#'               log_level = 1,frames_nmax = 60)
#'               
#' #Define your own ggplot2 plot design
#' stats_gg <- 'ggplot(data = pdat[[k]][[i]], aes_(x = ~val, y = ~value, colour = ~variable)) + 
#'              geom_smooth() + geom_point() + theme_bw() + theme(aspect.ratio=1) +
#'              scale_y_continuous(expand = c(0,0),limits = c(0,stats_max[k])) +
#'              scale_x_continuous(expand = c(0,0)) + 
#'              scale_color_manual(name="",values = cols[[k]]) +
#'              labs(x = "Basemap Value", y="Frequency",
#'                   title=stats_title[[k]], label=c("123","456")) +
#'              theme(plot.title = element_text(hjust = 0.5),
#'                    plot.subtitle = element_text(hjust = 0.5))'
#'                   
#' #Call animate_stats() with stats_gg
#' animate_stats(data_ani, out_dir, conv_dir = conv_dir,
#'               layer=layer, layer_dt = layer_dt, layer_type = "gradient",
#'               stats_digits = 1, stats_gg = stats_gg, out_name = "final_gif",
#'               log_level = 1,frames_nmax = 60)
#' }
#' 
#' @author Jakob Schwalb-Willmann
#' @seealso \code{\link{get_imconvert}}
#'
#' @export

animate_stats <- function(data_ani, out_dir, conv_dir = "convert", layer = "basemap", layer_dt = "basemap", layer_int = FALSE, layer_type = "",
         val_limits = NA, paths_col = "auto",  paths_mode = "true_data",
         stats_type = "", stats_gg = "", stats_digits = 1, stats_tframe = 5,
         stats_title = "", frames_layout = 0, frames_nmax =  0, frames_interval = .04, frames_nres = 1, frames_width = 600,
         frames_height = 600, out_name = "final_gif", log_level = 1, log_logical = FALSE, ...){

  #Define output handling
  out <- function(input,type = 1){
    signs <- c("[LOG]: ", "[WARNING]: ")
    if(type == 2 & log_level <= 2){warning(paste(signs[2],input))}
    else{if(type == 3){stop(input,call. = FALSE)}else{if(log_level == 1){cat(paste(signs[1],input),sep="\n")}}}
  }
  
  if(layer[1] == "basemap"){out("Basemap cannot be a Google Basemap for computing stats pixelwise. Please provide a single-layer dataset.",type = 3)}
  #if(layer_type == "RGB"){out("Basemap cannot be of type 'RGB' for computing stats pixelwise. Please provide a single-layer data.",type = 3)}
  
  #Call animate_move (alias function)
  animate_move(data_ani, out_dir, conv_dir = conv_dir, layer = layer, layer_dt = layer_dt, layer_int = layer_int, layer_type = layer_type,
               tail_elements = 1, paths_col = paths_col, paths_mode = "true_data",
               stats_create = TRUE, stats_tframe = stats_tframe, frames_layout = frames_layout,
               stats_type = stats_type, stats_title = stats_title,
               stats_gg = stats_gg, stats_digits = stats_digits, frames_nmax =  frames_nmax, frames_interval = frames_interval, frames_nres = frames_nres, frames_width = frames_width,
               frames_height = frames_height, out_name = out_name, log_level = log_level, log_logical = log_logical,  stats_only = TRUE)
}
