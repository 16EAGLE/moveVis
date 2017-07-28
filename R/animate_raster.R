#' Animate raster data
#'
#' \code{animate_raster} animates raster data provided as list of \code{raster} class objects. The function creates an animated GIF file and saves it into the output directory.
#'
#' @param layer list. List of raster objects. 
#' @param layer_type charachter. Layer type. Can be either "\code{RGB}" (if layer is a rasterBrick class object), "\code{gradient}" or "\code{discrete}". Default is "\code{gradient}".
#' @param layer_col character vector.  Two or more colours to be used for displaying the background layer. If \code{layer_type = "gradient"}, a colour ramp between the colous is calcualted. If \code{layer_type = "discrete"}, the colours will be used per value range. Ignored, if \code{layer_type = "RGB"}.
#' @param layer_nacol character. Colour to be displayed for NA values. Default is "white".
#' @param out_dir character. Output directory for the GIF file creation.
#' @param conv_dir character. Command or directory to call the ImageMagick convert tool (default to be \code{convert}). You can use \code{conv_dir = get_imconvert()} to search for the right command/tool directory and/or get the required software.
#' @param img_title character. Titel to be displayed above the animated plot. If not specified, no title will be displayed.
#' @param img_sub character. Subtitel to be displayed underneath the title. If not specified, no subtitle will be displayed.
#' @param img_caption character. Caption to be displayed underneath the plot. If not specified, no caption will be displayed.
#' @param img_labs character. Axis titles to be displayed at the x and y axis of the plot. If not specified, labs will be computed depending on the projection or will be "x" and "y".
#' @param legend_title character. Title to be displayed above the basemap layer legend (if layer_type is not \code{"RGB"}). Ignored, if \code{layer = "basemap"}.
#' @param legend_limits numeric vector. Fixed minimum and maximum limit values of the legend (gradient layer type). Default is NA for data-depending minimum and maximum values. Ignored, if \code{layer_type} is "discrete" or "RGB".
#' @param legend_labels character vectors. Label for each legend break class. If set to "auto", values are displayed. Default is "auto".
#' @param map_elements logical. If \code{FALSE}, map elements (north arrow and scale bar) are hidden. Default is \code{TRUE}.
#' @param scalebar_col character. Colour of the scalebar text. Default is "white".
#' @param north_col character. Colour of the north arrow. Default is "white".
#' @param frames_nmax numeric. Number of maximum frames. If set, the animation will be stopped, after the specified number of frames is reached. Default is 0 (displaying all frames).
#' @param frames_interval numeric. Duration, each frame is displayed (in seconds). Default is .04.
#' @param frames_nres numeric. Interval of which frames of all frames should be used (nth elements). Default is 1 (every frame is used). If set to 2, only every second frame is used.
#' @param frames_width numeric. Number of pixels of frame width. Default is 600 (with stats plots 1000).
#' @param frames_height numeric. Number of pixels of frame height. Defualt is 600.
#' @param out_name character. Name of the output file. Default is "final_gif".
#' @param log_level numeric. Level of console output given by the function. There are three log levels. If set to 3, no messages will be displayed except erros that caused an abortion of the process. If set to 2, warnings and errors will be displayed. If set to 1, a log showing the process activity, wanrnings ans errors will be displayed.
#' @param log_logical logical. For large processing schemes. If \code{TRUE}, the function returns \code{TRUE} when finished processing succesfully.
#' 
#' @return None or logical (see \code{log_logical}). The output GIF file is written to the ouput directory.
#' 
#' @details \code{animate_raster} is partly based on the \code{animation} package and needs the \code{convert} tool of the \code{ImageMagick} software package to assemble the GIF file. The command or directory to the convert tool needs to be provided with \code{conv_dir}. Please use \code{\link{get_imconvert}} to search for the convert command/tool directory on your system or to automatically download and install the required software. See \code{\link{get_imconvert}} for details.
#' 
#' @examples
#' \dontrun{
#' #Create a list of several raster objects to be displayed one after another
#' #If layer_type = RGB, use a brick class obejct with RGB bands!
#' layer <- list(raster1, raster2, raster2)
#' 
#' #Get your convert directory/command
#' conv_dir <- get_imconvert()
#' 
#' #Spcecify output directory
#' out_dir <- "path/to/output/dir"
#' 
#' #Call animate_raster
#' animate_raster(layer,out_dir = our_dir, conv_dir = conv_dir, layer_type = "RGB")
#' }
#' 
#' @author Jakob Schwalb-Willmann
#' @seealso \code{\link{get_imconvert}}
#' 
#' @export

animate_raster <- function(layer, out_dir, conv_dir = "convert", layer_type = "gradient",
                           layer_col = c("sandybrown","white","darkgreen"), layer_nacol = "white",
                           img_title = 'title', img_sub = 'subtitle', img_caption = "caption", img_labs = "labs",
                           legend_title = "", legend_limits = NA, legend_labels = "auto",
                           map_elements = TRUE, scalebar_col = "white", north_col = "white",
                           frames_nmax =  0, frames_interval = .04, frames_nres = 1, frames_width = NA, frames_height = NA,
                           out_name = "final_gif", log_level = 1, log_logical = FALSE){
  
  #Define output handling
  out <- function(input,type = 1){
    signs <- c("[LOG]: ", "[WARNING]: ")
    if(type == 2 & log_level <= 2){print(paste(signs[2],input))}
    else{if(type == 3){stop(input,call. = FALSE)}else{if(log_level == 1){print(paste(signs[1],input))}}}
  }
  
  layer_dt <- seq.POSIXt(as.POSIXct("2000-01-01"),by=1,length=length(layer))
  
  #Call animate_move (alias function)
  animate_move(raster_only = TRUE, layer = layer, layer_dt = layer_dt,
               out_dir = out_dir, conv_dir = conv_dir, layer_type = layer_type,
               layer_col = layer_col, layer_nacol = layer_nacol,
               img_title = img_title, img_sub = img_sub, img_caption = img_caption, img_labs = img_labs,
               legend_title = legend_title, legend_limits = legend_limits, legend_labels = legend_labels,
               map_elements = map_elements, scalebar_col = scalebar_col, north_col = north_col,
               frames_nmax =  frames_nmax, frames_interval = frames_interval, frames_nres = frames_nres, frames_width = frames_width, frames_height = frames_height,
               out_name = out_name, log_level = log_level, log_logical = log_logical)
}