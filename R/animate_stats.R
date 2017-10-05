#' Animate movement data statistics
#'
#' \code{animate_stats} animates statistic plot from movement data provided as \code{move} class objects or a list of them and basemap data provided as \code{raster}. It extracts basemap values of pixels that are part of the movement paths and visualizes frequencies per value. The function creates an animated GIF or video file and saves it into the output directory. See also \code{\link{animate_move}}. 
#'
#' @inheritParams animate_move
#' @param val_limits numeric vector. Fixed minimum and maximum limit values of the legend (gradient layer type). Default is NA for data-depending minimum and maximum values. Ignored, if \code{layer_type} is "discrete" or "RGB".
#' @param stats_type character. Defines which standard plot design should be used. Select either "line" or "bar". Ignored, if \code{stats_gg} is used.
#' @param stats_gg character. Enables usage of \code{ggplot2} syntax for plot design. If set, \code{stats_type} is ignored. See \code{details} for information on the statistic data structure to be used by the user defined plot function.
#' @param stats_digits numeric. Defines how detailed the statistic plot should be as number of decimals. Values with more decimals are rounded. Default is 1 for one decimal.
#' @param stats_tframe numeric. Defines the temporal range of the periodic stats plot. Default is 5 meaning that five time frames back from the displayed frame are evaluated.
#' @param stats_title character vector. Optional plot titles. Two character strings within a vector.
#' 
#' @return None or logical (see \code{log_logical}). The output GIF or video file is written to the ouput directory.
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
#' conv_dir <- get_libraries()
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
#'               stats_digits = 1, stats_type = "bar", out_name = "moveVis_ani",
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
#'               stats_digits = 1, stats_gg = stats_gg, out_name = "moveVis_ani",
#'               log_level = 1,frames_nmax = 60)
#' }
#' 
#' @author Jakob Schwalb-Willmann
#' @seealso \code{\link{get_libraries}}
#'
#' @export

animate_stats <- function(m, out_dir, conv_dir = "convert", layer = "basemap", layer_dt = "basemap", layer_int = FALSE, layer_type = "",
         val_limits = NA, paths_col = "auto",  paths_mode = "true_data",
         stats_type = "", stats_gg = "", stats_digits = 1, stats_tframe = 5,
         stats_title = "", frames_layout = 0, frames_nmax =  0, frames_fps = 25, frames_nres = 1, frames_tres = 0, frames_width = 800, 
         frames_height = 300, frames_pixres = 80, out_name = "moveVis_ani", out_format = "gif", log_level = 1, log_logical = FALSE){

  #Call animate_move (alias function)
  animate_move(m, out_dir, conv_dir = conv_dir, layer = layer, layer_dt = layer_dt, layer_int = layer_int, layer_type = layer_type,
               tail_elements = 1, paths_col = paths_col, paths_mode = "true_data",
               stats_create = TRUE, stats_tframe = stats_tframe, frames_layout = frames_layout,
               stats_type = stats_type, stats_title = stats_title, legend_limits = val_limits,
               stats_gg = stats_gg, stats_digits = stats_digits, frames_nmax =  frames_nmax, frames_fps = frames_fps, frames_nres = frames_nres, frames_width = frames_width,
               frames_height = frames_height, frames_pixres = frames_pixres, out_name = out_name, out_format = out_format, log_level = log_level, log_logical = log_logical,  stats_only = TRUE)
}
