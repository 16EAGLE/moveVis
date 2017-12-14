#' Animate movement data
#'
#' \code{animate_move} animates movement data provided as \code{move} class objects or a list of them. The function creates an animated GIF or video file and saves it into the output directory. \code{animate_move} can be operated in different timing modes (see \code{paths_mode}) and with different background layer types (see \code{layer}, \code{layer_type} and \code{map_type}). 
#'
#' @param m list or \code{moveStack} class object. Needs to contain one or several \code{move} class objects (one for each individual path to be displayed) containing point coordinates, timestamps, projection and individual ID.
#' @param out_dir character. Output directory of the output file.
#' @param conv_dir character. Command of or directory to required image/video converter library. Depends on, what is specified for \code{out_format}. If \code{out_format = "gif"}, animate_move() works with the ImageMagick \code{convert} tool. In this case, specify command of or path to the \code{convert} tool. You can use \code{\link{get_libraries}} to find or download/install \code{convert}. If \code{out_format} is a video format (e.g. "mp4", "mov" ...), animate_move() works with either the FFmpeg \code{ffmepg} tool or the libav \code{avconv} tool. In this case, specify command of or path to the \code{ffmpeg} or \code{avconv} tool. See also \code{\link{get_libraries}}. If not specified, animate_move() trys to find libraries automatically.
#' @param layer raster, list or character "basemap". Single raster object or list of raster objects to be used as (dynamically changing) basemap layer. Default is \code{"basemap"} to download a static basemap layer. Use a rasterBrick class object and set layer_type to "\code{RGB}" to compute a RGB basemap.
#' @param layer_dt POSIXct vector or list. Single POSIXct date/time stamp or list of POSIXct date/time stamps representing the acquisition dates of the \code{layer} raster objects.
#' @param layer_int logical. Whether to interpolate the basemap layer objects over time, if several are provided (\code{TRUE}), or to display them one after another depending on the animation time frame that is displayed (\code{FALSE}). Default is \code{FALSE}.
#' @param layer_type charachter. Layer type. Either "\code{RGB}" (if layer is a rasterBrick class onejct), "\code{gradient}" or "\code{discrete}". Default is "\code{RGB}". Ignored, if \code{layer = "basemap"}.
#' @param layer_stretch character. Ignored, if \code{layer_type} is not "RGB". Either "none", "lin", "hist", "sqrt" or "log" for no stretch, linear, histogram, square-root or logarithmic stretch. Default is "none".
#' @param layer_col character vector.  Two or more colours to be used for displaying the background layer. If \code{layer_type = "gradient"}, a colour ramp between the colous is calcualted. If \code{layer_type = "discrete"}, the colours will be used per value range. Ignored, if \code{layer_type = "RGB"}.
#' @param layer_nacol character. Colour to be displayed for NA values. Default is "white".
#' @param map_type character.  Static basemap type. Chosse from "roadmap", "satellite", "hybrid", "terrain".
#' @param static_data data.frame. Data (e.g. static points) to be displayed within the spatial plot of the output animation. At least, "x", "y" columns for the coordinates and "names" for the naming of the point have to be included. If "static_gg" remains unspecified, "static_data" is plottet as points to the output map, annotated with their namings. Points outside the frame extent are not displayed. See "static_gg" for further options. 
#' @param static_gg character. One or several \code{ggplot2} functions, concatenated by "+" specifying how "static_data" should be displayed, e.g. using \code{geom_point} and \code{geom_text} for displaying points annotated with text. \code{ggplot2 data} and \code{aes, aes_} arguments etc. need to referr to the columns specified in "static_data". As default, "static_data" is plotted as \code{geom_point} and \code{geom_label}.
#' @param tail_elements numeric. Number of points to be displayed as path tail of the animation paths. Default is 10.
#' @param tail_size numeric. Size of the first tail element. Default is 4.
#' @param img_title character. Titel to be displayed above the animated plot. If not specified, no title will be displayed.
#' @param img_sub character. Subtitel to be displayed underneath the title. If not specified, no subtitle will be displayed.
#' @param img_caption character. Caption to be displayed underneath the plot. If not specified, no caption will be displayed.
#' @param img_labs character. Axis titles to be displayed at the x and y axis of the plot. If not specified, labs will be computed depending on the projection or will be "x" and "y".
#' @param legend_title character. Title to be displayed above the basemap layer legend (if layer_type is not \code{"RGB"}). Ignored, if \code{layer = "basemap"}.
#' @param legend_limits numeric vector. Fixed minimum and maximum limit values of the legend (gradient layer type). Default is NA for data-depending minimum and maximum values. Ignored, if \code{layer_type} is "discrete" or "RGB".
#' @param legend_labels character vectors. Label for each legend break class. If set to "auto", values are displayed. Default is "auto".
#' @param scalebar_col character. Colour of the scalebar text. Default is "white".
#' @param scalebar_dist numeric. Distance represented by the scalebar in kilometers.
#' @param map_elements logical. If \code{FALSE}, map elements (north arrow and scale bar) are hidden. Default is \code{TRUE}.
#' @param time_scale logical. If \code{FALSE}, time scale is hidden. Default is \code{TRUE}.
#' @param time_bar_col character. Colour of the time progress bar on the top edge of the map. Default is "grey".
#' @param extent_factor numeric. Defines the distance between the spatial extents of the movement data set and the basemap as proportion of the axis distance. Default is 0.0001. The higher the value, the larger the basemap extent. Ignored, if \code{layer} = "basemap".
#' @param north_col character. Colour of the north arrow. Default is "white".
#' @param paths_col character vector. Colours of the individual animation paths. If set to "auto", a predfined colour set will be used. If single colour, all paths will be displayed by the same colour. If more individuals then colours, the colours are repeated.
#' @param paths_alpha numeric. Set transparency of pathes. If set to 0, path is invisible. Default is 1.
#' @param paths_mode character vector. Mode to be used for dealing with time information when displaying multiple individual paths. If set to "true_data", paths are displayed based on true coverage times, showing only time periods that are covered. Time gaps will be skipped. Each frame is linked to a specific true time. If set to "true_time",  paths are displayed based on true coverage times. Time gaps will be filled with non-movement frames. This mode is only recommended, if the dataset has no time gaps. Each frame is linked to a specific, true time. If set to "simple", all movement paths are displayed individually with no regard to the true coverage times. Time gaps will be skipped. Each frame displays several times at once, since each individual path has its own time. Default is "true_data".
#' @param paths_na.hold logical. If TRUE, last path location is being hold on frame for NA path locations. If FALSE, path disappears until next path non-NA location. Default is TRUE.
#' @param indi_names character. Optional vector of individual names. Length has to be equal to number of individuals in \code{m}. If NA, individual names are tried to be extracted from \code{m} using \code{\link{idData}}. Default is NA.
#' @param frames_layout matrix. Optional layout. Define, which plots should be placed where using a matrix representing the GIF/video frame. Matrix elements can be the following plot identifiers: "map" for the spatial plot, "st_all", "st_per" for the overall and periodic stats plot or "st_allR", "st_perR", "st_allG", "st_perG", "st_allB", "st_perB" for the overall and periodic stats plots per band, when using \code{layer_type = "RGB"}, and 'st_leg' for a stats legend. Alternatively, integers from 1 to 8 corresponding to the described order can be used. Plots not mentioned using \code{frames_layout} identifiers are not displayed. If set to 0, layout is generated automatically. Default is 0.
#' @param frames_nmax numeric. Number of maximum frames. If set, the animation will be stopped, after the specified number of frames is reached. Default is 0 (displaying all frames).
#' @param frames_fps numeric. Frames to display per second (FPS). Note that the \code{gif} format only can handle FPS out of 100, e.g. 25. In that case, \code{frames_fps} input is rounded. Default is 25.
#' @param frames_nres numeric. Interval of which frames of all frames should be used (nth elements). Default is 1 (every frame is used). If set to 2, only every second frame is used.
#' @param frames_tres numeric. Defines temporal output resolution in seconds, 'm' should be interpolated to (linear interpolation). If 0, temporal resolution is detected automatically. Default is 0.
#' @param frames_width numeric. Number of pixels of frame width. Default is 600 (with stats plots 1000).
#' @param frames_height numeric. Number of pixels of frame height. Defualt is 600.
#' @param frames_pixres numeric. Resolution of output file in pixel in ppi. The higher the ppi, the higher frames_height and frames_width should be to avoid large fonts and overlaps. Default is 80.
#' @param out_name character. Name of the output file. Default is "moveVis".
#' @param out_format character. Output format, e.g. "gif", "avi", "3gp", "mov", "mpeg", "mp4". Use \code{\link{get_formats}} to get all supported file formats on your system. "gif" is recommended for short animations only (recommended max. frame number around 200 frames; GIF frames are unlimited, but compution time will be very long). Use a video format for long animations. Format "gif" requires ImageMagick, all other video formats require FFmpeg ('ffmpeg') or libav ('avconv') to be installed. For that, also see \code{\link{get_libraries}}.
#' @param overwrite logical. If TRUE, files with equal file names to \code{out_name} will be overwritten. Default is FALSE.
#' @param log_level numeric. Level of console output given by the function. There are three log levels. If set to 3, no messages will be displayed except erros that caused an abortion of the process. If set to 2, warnings and errors will be displayed. If set to 1, a log showing the process activity, wanrnings ans errors will be displayed.
#' @param log_logical logical. For large processing schemes. If \code{TRUE}, the function returns \code{TRUE} when finished processing succesfully.
#' @param stats_create logical. \code{TRUE} to create statistic plots side by side with the spatial plot. Use the arguments explained for \code{\link{animate_stats}} to adjust the plotting behaviour. Default is \code{FALSE}.
#' @param conv_cmd character. Recommended for expert use only. Passes additional command line options to the conversion command, e.g. with a \code{convert} call adding '-limit' for memory ressource handling. For details, see check the documentations of ImageMagick \code{convert}, FFmpeg \code{ffmpeg} and libav \code{avconv}.
#' @param conv_frames numeric. Recommended for expert use only. Only used, if \code{out_format = "gif"}. Number of frames to be used for creating GIF segments that will be assembled to a final GIF file. Correct number depends on system performance and total frames number. Default is 100. Ignored, if \code{out_format} is not "gif".
#' @param ... optional arguments. All arguments taken by \code{\link{animate_stats}} can be handed over to \code{\link{animate_move}} as well to create sidy-by-side spatial and statistic plot animations (see \code{\link{animate_stats}}).
#' 
#' @return None or logical (see \code{log_logical}). The output file is written to the ouput directory.
#' 
#' @details Make sure you have run \code{\link{get_libraries}} before you use moveVis for the first time: Depending on the selected output format (\code{out_format}, \code{animate_move} either needs the \code{convert} tool of the ImageMagick software package (.gif format) or either \code{ffmpeg} from FFmpeg or \code{avconv} from libav (video formats). The command or directory to the convert tool needs to be provided with \code{conv_dir}. Please use \code{\link{get_libraries}} to search for the needed libraries and command/tool directories on your system or to automatically download and install the required software. See \code{\link{get_libraries}} and \code{out_format} and \code{conv_dir} for details.
#' 
#' \code{animate_move} preprocesses your move data depending on the state of the data (see \code{paths_mode} and \code{frames_tres}). \code{animate_move} is based on \code{ggplot2}. 
#' 
#' @examples
#' 
#' \dontrun{
#' #Load move and moveVis packages
#' library(move)
#' library(moveVis)
#' 
#' #Get the sample data from the moveVis package (a data.frame)
#' data("move_data")
#' move_data$dt <- as.POSIXct(strptime(move_data$dt, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
#'
#' #Create moveStack object including multiple individuals from the data.frame
#' #alternatively, use the move package to download data directly from movebank.org
#' m <- move(move_data$lon, move_data$lat, proj=CRS("+proj=longlat +ellps=WGS84"),
#'                  time = move_data$dt, animal=move_data$individual, data=move_data)
#' 
#' #Find the command or directory to convert tool of ImageMagick
#' conv_dir <- get_libraries()
#' 
#' #Specify the output directory, e.g.
#' out_dir <- paste0(getwd(),"/test")
#' 
#' #Specify some optional appearance variables
#' img_title <- "Movement of the white stork population at Lake Constance, Germany"
#' img_sub <- paste0("including individuals ",paste(rownames(idData(m)), collapse=', '))
#' img_caption <- "Projection: Geographical, WGS84; Sources: Movebank 2013; Google Maps"
#' 
#' #Call animate_move() with an automatic basemap from Google, maximum frames at 50
#' #output format "gif"
#' animate_move(m, out_dir, conv_dir, tail_elements = 10,
#'              paths_mode = "true_data", frames_nmax = 50,
#'              img_caption = img_caption, img_title = img_title,
#'              img_sub = img_sub, log_level = 1, extent_factor = 0.0002,
#'              out_format = "gif")
#'  
#' #Improve your animation by adding a static points layer
#' static_data <- data.frame(x = c(8.94,8.943), y = c(47.75,47.753), names = c("Site 1","Site 2"))
#' 
#' #Call animate_move() with "static_data" added
#' #use another output format, e.g. "mov"
#' animate_move(m, out_dir, conv_dir, tail_elements = 10,
#'              paths_mode = "true_data", frames_nmax = 50,
#'              img_caption = img_caption, img_title = img_title,
#'              img_sub = img_sub, log_level = 1, extent_factor = 0.0002, 
#'              static_data=static_data, out_format = "mov")
#'              
#' #Try a different paths_mode: Instead of "true_data" use "simple"
#' #output format "mp4". Longer videos then 100-200 frames should not be GIFs
#' animate_move(m, out_dir, conv_dir, tail_elements = 10,
#'              paths_mode = "simple", frames_nmax = 50,
#'              img_caption = img_caption, img_title = img_title,
#'              img_sub = img_sub, log_level = 1, extent_factor = 0.0002,
#'              static_data=static_data, out_format = "mp4")
#'  
#' #Use your own basemap by adding lists of rasters and of timestamps
#' data("basemap_data")
#' layer = basemap_data[[1]] #this is a example MODIS NDVI dataset
#' layer_dt = basemap_data[[2]] #this is a corresponding date/time list
#'  
#' #Call animate_move with NDVI data as basemap
#' #layer_type is "gradient", since NDVI values are continuous
#' animate_move(m, out_dir, conv_dir, tail_elements = 10, layer_type = "gradient",
#'              paths_mode = "true_data", frames_nmax = 50, layer =layer, layer_dt = layer_dt,
#'              img_caption = img_caption, img_title = img_title,
#'              img_sub = img_sub, log_level = 1, extent_factor = 0.0002)
#'              
#' #How do your moving individuals interact with their environments?
#' #Use "stats_create" to create statistics plots
#' animate_move(m, out_dir, conv_dir, tail_elements = 10, layer_type = "gradient",
#'              paths_mode = "true_data", frames_nmax = 50, layer =layer, layer_dt = layer_dt,
#'              img_caption = img_caption, img_title = img_title,
#'              img_sub = img_sub, log_level = 1, extent_factor = 0.0002,
#'              stats_create = TRUE)
#'
#' #If you just want those stats plots, use animate_stats()
#' 
#' #Use "frames_layout" to change the layout of your animation
#' #e.g. change the position of st_all and st_per
#' frames_layout <-  rbind(c("map","map","map","st_all","st_leg"),
#'                         c("map","map","map","st_per","st_leg"))
#' 
#' #or equalize the sizes of spatial map and stats plots
#' frames_layout <-  rbind(c("map","st_all","st_per","st_leg"))
#' 
#' animate_move(m, out_dir, conv_dir, tail_elements = 10, layer_type = "gradient",
#'              paths_mode = "true_data", frames_nmax = 50, layer =layer, layer_dt = layer_dt,
#'              img_caption = img_caption, img_title = img_title,
#'              img_sub = img_sub, log_level = 1, extent_factor = 0.0002,
#'              stats_create = TRUE, frames_layout=frames_layout)
#' }
#'
#' @author Jakob Schwalb-Willmann
#' @seealso \code{\link{get_libraries}}, \code{\link{animate_stats}}, \code{\link{animate_raster}}
#' 
#' @import ggplot2
#' @importFrom raster nlayers crs extent projectRaster raster getValues setValues rasterToPoints res crop extract unstack sampleRegular brick ncell stack
#' @importFrom sp SpatialPointsDataFrame spTransform coordinates proj4string proj4string<-
#' @importFrom geosphere distGeo
#' @importFrom dismo gmap
#' @importFrom maptools gcDestination
#' @importFrom rasterVis gplot
#' @importFrom grid arrow unit
#' @importFrom move move moveStack split idData timestamps timeLag interpolateTime n.indiv
#' @importFrom grDevices png dev.off rgb colorRampPalette
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom methods is as
#' @importFrom stats na.omit setNames ecdf quantile
#' @importFrom gridExtra grid.arrange
#' @importFrom graphics plot
#' @importFrom simecol approxTime
#' @importFrom plyr ldply
#' @importFrom zoo na.locf
#' @importFrom lubridate seconds_to_period hour minute second
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom pbapply timerProgressBar getTimerProgressBar setTimerProgressBar closepb
#' @importFrom RStoolbox ggRGB
#'  
#' @export

animate_move <- function(m, out_dir, conv_dir = "",
                         paths_mode = "true_data",  paths_na.hold = TRUE, paths_col = "auto", paths_alpha = 1, indi_names = NA,
                         layer = "basemap", layer_dt = "basemap", layer_int = FALSE, layer_type = "", layer_stretch = "none",
                         layer_col = c("sandybrown","white","darkgreen"), layer_nacol = "white", map_type="satellite", stats_create = FALSE, static_data = NA, static_gg = NA,
                         extent_factor = 0.0001, tail_elements = 10, tail_size = 4,
                         img_title = 'title', img_sub = 'subtitle', img_caption = "caption", img_labs = "labs",
                         legend_title = "", legend_limits = NA, legend_labels = "auto",
                         map_elements = TRUE, time_scale = TRUE, time_bar_col = "grey", scalebar_col = "white", scalebar_dist = "auto", north_col = "white", 
                         frames_layout = 0, frames_nmax =  0, frames_fps = 25, frames_nres = 1, frames_tres = 0, frames_width = NA, frames_height = NA, frames_pixres = 80,
                         out_name = "moveVis", out_format = "gif", overwrite = FALSE, log_level = 1, log_logical = FALSE, ..., conv_cmd = "", conv_frames = 100){
  
  #Start clock
  run.start <- Sys.time()
  
  #Checking for additional arguments
  arg <- list(...)
  s_try <- try(arg$stats_only)
  if(class(s_try) == "NULL"){stats_only <-  FALSE}else{stats_only <- arg$stats_only}
  s_try <- try(arg$stats_type)
  if(class(s_try) == "NULL"){stats_type  <- ""}else{stats_type <- arg$stats_type}
  s_try <- try(arg$stats_gg)
  if(class(s_try) == "NULL"){stats_gg  <- ""}else{stats_gg <- arg$stats_gg}
  s_try <- try(arg$stats_digits)
  if(class(s_try) == "NULL"){
    if(layer_type == "gradient"){stats_digits  <- 1}
    if(layer_type == "RGB"){stats_digits  <- 1}
    if(layer_type == "discrete"){stats_digits <- 0}   
  }else{stats_digits <- arg$stats_digits}
  s_try <- try(arg$stats_tframe)
  if(class(s_try) == "NULL"){stats_tframe  <- 5}else{stats_tframe <- arg$stats_tframe}
  s_try <- try(arg$stats_title)
  if(class(s_try) == "NULL"){stats_title  <- ""}else{stats_title <- arg$stats_title}
  s_try <- try(arg$raster_only); if(class(s_try) == "NULL"){raster_only  <- ""}else{raster_only <- arg$raster_only}
  s_try <- try(arg$data_ani); if(class(s_try) != "NULL"){m <- arg$data_ani}
  
  #shiny arguments
  s_try <- try(arg$shiny_mode); if(class(s_try) != "NULL"){shiny_mode <- arg$shiny_mode}else{shiny_mode = FALSE} #prev or ani
  s_try <- try(arg$shiny_session); if(class(s_try)[1] != "NULL"){shiny_session <- arg$shiny_session}else{shiny_session = FALSE}
  if(shiny_mode == FALSE){Progress <- NULL} #for CRAN checks
  
  #parallel processing set-up
  s_try <- try(arg$par); if(class(s_try) != "NULL"){par <- arg$par}else{par <- FALSE}
  s_try <- try(arg$par_cores); if(class(s_try)[1] != "NULL"){par_cores <- arg$par_cores}else{par_cores = NA}
  

  ## FUNCTION DEFINITION
  
  #Define output handling
  out <- function(input,type = 1, ll = log_level, msg = shiny_mode){
    signs <- c("", "")
    if(type == 2 & ll <= 2){warning(paste0(signs[2],input), call. = FALSE, immediate. = TRUE)}
    else{if(type == 3){stop(input,call. = FALSE)}else{if(ll == 1){
      if(msg == FALSE){cat(paste0(signs[1],input),sep="\n")
      }else{message(paste0(signs[1],input))}}}}
  }
  
  #get legend
  g_legend<-function(a.gplot){ 
    tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
    legend <- tmp$grobs[[leg]] 
    return(legend)
  } 
  
  #Calculate start/stop times
  st_times <- function(data){
    for(i in 1:length(data)){
      if(i == 1){
        start_dt <- data[[i]]$dt[1]
        stop_dt <- data[[i]]$dt[length(data[[i]]$dt)]
      }else{
        start_dt <- c(start_dt, data[[i]]$dt[1])
        stop_dt <- c(stop_dt, data[[i]]$dt[length(data[[i]]$dt)])
      }
    }
    return(list(start_dt,stop_dt))
  }
  
  get_indi <- function(data, names = indi_names){
    if(is.na(names[1]) == FALSE){return(names)
    }else{
      indi <- idData(data)
      if(length(rownames(indi)) == 0){indi <- paste0("individual ",seq(1,NROW(indi),by=1))}
      else{if(length(indi) != 1){indi <- as.character(rownames(indi))}else{indi <- as.character(indi)}}
      return(indi)
    }
  }
  

  #+++++++++++++++++++++++++++++++++++++++ MAIN ++++++++++++++++++++++++++++++++++++++++++++++
  
  ## PREREQUISITES
  out("Checking prerequisites...",type=1)
  
  #Plattform dependences
  if(.Platform$OS.type == 'windows'){cmd.fun <- shell}else{cmd.fun <- system}

  if(overwrite == FALSE & file.exists(paste0(out_dir,"/",out_name,".",out_format))){out("Output file already exists. Change 'out_name' or set 'overwrite' to TRUE.",type = 3)}
  if(raster_only != TRUE){ #m not needed for animate_raster()
    if(missing(m)){
      out("Argument 'm' is missing. Please specify the input movement data.",type=3)
    }else{
      if(is.list(m) != TRUE){
        if(is(m,"Move")){m <- list(m)}
        else{
          if(is(m,"MoveStack")){
            m.stack <- m
            m <- split(m)
            #for(i in 1:length(split(m))){
            #  m_temp[[i]] <- split(m)[[i]] 
            #}
            #m <- m_temp
          }else{out("Argument 'm' needs to be either a list object containing a 'move' object for each individual  or a 'moveStack' object.",type=3)
          }
        }
      }else{
        for(i in 1:length(m)){
          if(is(m[[i]],"Move") == FALSE){out("Elements in list object 'm' need to be of the 'move' class.",type=3)}
        }
      }
    }
  }else{
    tail_elements <- 0
  }
  if(is.na(indi_names[1]) == FALSE){
    if(is.character(indi_names) == FALSE){out("'indi_names' needs to be of type 'character'.",type=3)
    }else{if(length(indi_names) != length(m)){out("'indi_names' needs to be of same length as number of input individuals.",type=3)}}
  }
  if(missing(out_dir)){
    out("Argument 'out_dir' is missing. Please specify the output directory.", type=3)
  }else{
    if(is.character(out_dir) != TRUE){
      out("Argument 'out_dir' needs to be a character object.",type=3)
    }else{
      if(!dir.exists(out_dir)){out(paste0("'out_dir' '",out_dir,"' is not existing. Create or change 'out_dir'."),type = 3)}
      user_wd <- getwd()
      temp_dir <- paste0(tempdir(),"/moveVis")
      quiet(dir.create(temp_dir))
      setwd(temp_dir)
    }
  }
  if(is.character(out_format) == FALSE){out("Argument 'out_format' needs to be a character object.", type = 3)}
  if(shiny_mode == FALSE){
    if(is.character(conv_dir) == FALSE){
      out("Argument 'conv_dir' needs to be a character object.",type=3)
    }else{
      if(out_format == "gif"){
        if(length(conv_dir) > 1){
          if(length(grep("convert",conv_dir)) != 0){
            conv_dir <- conv_dir[grep("convert",conv_dir)]
          }else{
            out(paste0("Could not detect the 'convert' tool from 'conv_dir'. '",paste0(conv_dir, collapse = ", "), "' cannot be used for this output file format: '",out_format,"'"),type=2)
          }
        }
        if(conv_dir == ""){conv_dir <- get_libraries(lib.tool = "convert", nodownload = TRUE, log_level = 3)}
        tryit <- try(cmd.fun(conv_dir,ignore.stdout = TRUE,ignore.stderr = TRUE))
        if(tryit != 1){out(paste0("'",conv_dir,"' could not be executed. Use get_libraries() to search for 'convert' on your system or to install the required library (ImageMagick)."),type=3)
        }else{out(paste0("Detected 'conv_dir' executable on this system: '",conv_dir,"'"),type=1)}
      }else{
        if(length(conv_dir) > 1){
          if(length(grep("ffmpeg",conv_dir)) != 0){
            conv_dir <- conv_dir[grep("ffmpeg",conv_dir)]
          }else{
            if(length(grep("avconc",conv_dir)) != 0){
              conv_dir <- conv_dir[grep("avconv",conv_dir)]
            }else{
              out(paste0("Could not detect the 'ffmpeg' or 'avconv' tool from 'conv_dir'. '",paste0(conv_dir, collapse = ", "), "' cannot be used for this output file format: '",out_format,"'"),type=2)
            }
          }
        }
        if(conv_dir == ""){conv_dir <- get_libraries(lib.tool = c("ffmpeg","avconv"), nodownload = TRUE, log_level = 3)[1]}
        tryit <- try(cmd.fun(conv_dir,ignore.stdout = TRUE,ignore.stderr = TRUE))
        if(tryit > 1){out(paste0("'", conv_dir, "' could not be executed. Use get_libraries() to search for 'ffmpeg'/'avconv' on your system or to install the required libraries (FFmpeg, libav)."),type=3)
        }else{out(paste0("Detected 'conv_dir' executable on this system: '",conv_dir,"'"),type=1)}
        
        formats.search <- which(get_formats(conv_dir) == out_format)
        if(length(formats.search) == 0){out(paste0("This system's '",conv_dir,"' installation seems to not support '",out_format,"'. Use another format."),type=3)}
      }
    }
  }
  
  if(is.numeric(frames_nres) == FALSE){
    out("Keyword 'frames_nres' needs to be numeric. Setting 'frames_nres' to 1.",type=2)
    frames_nres <- 1
  }
  if(is.numeric(frames_tres) == FALSE){
    out("Keyword 'frames_tres' needs to be numeric. Setting 'frames_nres' to 0.",type=2)
    frames_tres <- 0
  }
  if(is.character(paths_col) == FALSE){out("Keword 'paths_col' needs to be character or character vector.",type=3)}
  if(is.character(layer) == FALSE){
    if(length(grep("aster",class(layer))) == 1){layer = list(layer)}
    if(raster_only == TRUE & length(layer) == 1){out("'layer' needs to be a list of more then one raster object, if animate_raster() is used.", type = 3)}
    if(is.character(layer_dt) == TRUE){
      if(length(layer) > 1){out("Argument 'layer_dt' needs to be specified if using 'layer'. Please provide time data with same length of 'layer'.",type=3)}
    }else{
      if(is.list(layer_dt)){
        layer_dt <- as.POSIXct(sapply(layer_dt, function(x){as.character(x)}),tz = "UTC")
      }
    }
    if(length(layer) != length(layer_dt)){
      out("Arguments 'layer' and 'layer_dt' need to be lists of equal lengths.",type=3)
    }
    if(raster_only != TRUE){
      if(unlist(strsplit(as.character(crs(m[[1]]))," "))[1] != unlist(strsplit(as.character(crs(layer[[1]]))," "))[1]
         & unlist(strsplit(as.character(crs(m[[1]]))," "))[2] != unlist(strsplit(as.character(crs(layer[[1]]))," "))[2]){
        out("Arguments 'layer' and 'm' have different projections. Please provide movement data and
            background layer data with equal projection or do not use 'layer'.",type=3)
      }
    }
    if(is(layer[[1]],"RasterBrick") == TRUE){out("Layer input of class 'RasterBrick' is not supported. Please use 'RasterStack' instead.",type=3)}
    if(layer_type != "gradient" & layer_type != "discrete" & layer_type != "RGB"){out("'layer_type' must be specified (either 'gradient', 'discrete' or 'RGB').",type = 3)}
    }else{
      if(layer != "basemap"){out(paste0("Unknown input '",layer, "'. Argument 'layer' needs to be either a list of raster objects, a single raster object or a string containing 'basemap'."),type=3)}
      layer_type <- ""
    }
  if(layer_int == TRUE){if(layer_type == "discrete"){
    layer_int <- FALSE
    out("'layer_int' = TRUE ignored, since 'layer_type' = 'discrete'. Filled gaps instead interpolating.",type=2)
  }}
  if(stats_create == TRUE){
    if(layer[1] == "basemap"){
      out("Stats cannot be visualized for stadard Google Maps basemaps. Please use 'layer' to provide single layer basemap data.",type = 3)
    }else{
      if(stats_type == ""){
        if(layer_type == "gradient"){stats_type <- "line"}
        if(layer_type == "RGB"){stats_type <- "line"}
        if(layer_type == "discrete"){stats_type <- "bar"}
      }else{
        if(stats_type != "line" & stats_type != "bar"){
          out("Unkown input. 'stats_type' can either be 'line' or 'bar'. Use 'stats_gg' for advanced plotting.",type = 3)
        }
      }
    }
    if(paths_mode == "simple"){if(length(layer) > 1){out("Creating statistics using paths_mode = 'simple' only makes sense with static base layer!",type = 2)}}
  }
  if(stats_title == ""){
    if(layer_type == "RGB"){
      stats_title <- list(
        c("Map values (all frames)\nband 1",
          paste0("Map values (",as.character(stats_tframe)," frames)\nband 1")),
        c("Map values (all frames)\nband 2",
          paste0("Map values (",as.character(stats_tframe)," frames)\nband 2")),
        c("Map values (all frames)\nband 3",
          paste0("Map values (",as.character(stats_tframe)," frames)\nband 3"))
      )
    }else{
      stats_title <- list(c("Map values acummulated\nover all frames",
                            paste0("Map values acummulated\nover a ",as.character(stats_tframe)," frames period")))
    }
  }else{
    if(layer_type == "RGB"){
      stats_title <- list(unlist(stats_title[1:2]),unlist(stats_title[3:4]),unlist(stats_title[5:6]))
    }else{
      stats_title <- list(unlist(list(stats_title)))
    }
  }
  if(is.na(frames_width)){
    if(stats_create == TRUE){
      if(stats_only == FALSE){
        if(layer_type != "RGB"){frames_width <- 1200}else{frames_width <- 1600}
      }else{
        frames_width <- 1000
      }
    }else{frames_width <- 600}
  }
  if(is.na(frames_height)){
    if(stats_only == TRUE & layer_type == "RGB"){frames_height <- 900}else{frames_height <- 600}
  }
  if(frames_width > 1600 | frames_height > 1000){out("High resolution ouptut causes time intensive frame creation.",type = 2)}
  if(frames_layout[1] == 0){
    if(raster_only == TRUE | stats_create == FALSE){frames_layout <- rbind(c(1))}
    else{
      if(stats_only == TRUE){
        if(layer_type == "RGB"){frames_layout =  rbind(c(2,4,6,8),c(3,5,7,8))}else{frames_layout = rbind(c(2,3,8))}
      }else{
        if(layer_type == "RGB"){frames_layout = rbind(c(1,1,2,4,6,8),c(1,1,3,5,7,8))}else{frames_layout = rbind(c(1,1,2,8),c(1,1,3,8))} #frames_layout = rbind(c(1,1,9,2,4,6,8),c(1,1,9,3,5,7,8))}else{frames_layout = rbind(c(1,1,9,2,8),c(1,1,9,3,8))}
      }
    }
  }else{
    quiet(res <- matrix(c(1,2,3,2,3,4,5,6,7,8)[match(frames_layout, c("map","st_per","st_all","st_perR","st_allR","st_perG","st_allG","st_perB","st_allB","st_leg"))], 3))
    res <- ifelse(is.na(res), frames_layout, res)
    frames_layout <- apply(res, 2, as.numeric)
    
    if(stats_only == TRUE & length(which(frames_layout == 1)) !=0 ){out("'frames_layout' cannot contain 'map' with animate_stats() or if 'stats_only' is set to TRUE.",type = 3)}
    if(stats_create == FALSE & length(which(frames_layout > 1)) !=0){out("'frames_layout' cannot contain any stats layers or legends, if 'stats_create' is set to FALSE.",type=3)}
  }
  if(!is.na(static_data[1])[1]){
    if(length(static_data$x)==0){out("'static_data' must contain a column 'x' containing the x coordinates.",type=3)}
    if(length(static_data$y)==0){out("'static_data' must contain a column 'y' containing the y coordinates.",type=3)}
    if(length(static_data$names)==0){out("'static_data' must contain a variable 'names' containing the points' namings.",type=3)}
  }
  if(scalebar_dist != "auto"){
    if(is.numeric(scalebar_dist) == FALSE){out("'scalebar_dist' needs to be either set to 'auto' or of type 'numeric' (scalebar in kilometers).",type=3)}
  }
  
  if(par == TRUE){
    n_cores <- detectCores()
    if(is.na(par_cores) == FALSE){
      if(par_cores > n_cores){
        par_cores <- NA
        out("'par_cores' is exceeding number of available cores. Number of cores will be defined automatically.")
      }else{
        n_cores <- par_cores
      }
    }
    if(is.na(par_cores)){
      if(n_cores > 2){n_cores <- n_cores - 2}else{
        par <- FALSE
        out(paste0("'par' set to FALSE, since there are not enough cores for stable parallel processing [cores: ",toString(n_cores),"]"))
      }
    }
  }
  
  
  ## Preprocess move data
  disp <- FALSE
  if(raster_only != TRUE){ #101: exclude for raster_only
    global.crs <- crs(m[[1]]) #get crs
    global.crs.str <- as.character(global.crs) #get crs str
    if(length(grep("+proj=longlat +ellps=WGS84",global.crs.str)) == 0){m <- lapply(m,function(x){spTransform(x, crs("+proj=longlat +ellps=WGS84"))})}
    m.stack <- moveStack(m)
    
    uni.stamps.c <- unique(as.numeric(sapply(strsplit(as.character(timestamps(m.stack)),":"), function(x){x[3]}))) #identify unfirom seconds digits
    if(length(uni.stamps.c) > 1){uni.stamps <- FALSE
    }else{
      uni.lag <- unique(unlist(timeLag(m.stack,units ="secs"))) #identify timelags
      #if(length(unique(timeLag(m.stack,units ="secs"))) == 1){uni.stamps <- TRUE}else{uni.stamps <- FALSE} #check for uniform timestamps
      if(length(unique(sapply(strsplit(as.character(uni.lag/min(uni.lag)),"[.]"), function(x){x[2]}))) == 1){uni.stamps <- TRUE #check, if all timelags can be divided by minimum lag
      }else{uni.stamps <- FALSE}
    }
    
    if(uni.stamps == TRUE & frames_tres == 0){
      #nothing must be done except finding out the resolution
      frames_tres <- min(sapply(timeLag(m.stack,units = "secs"), min))
      out(paste0("Detected minimum temporal resolution of ", as.character(frames_tres) ," seconds and uniform timestamps, no paths interpolation applied."))
    }else{
      if(frames_tres != 0){
        out("Paths interpolation will be applied, since 'frames_tres' is user-defined.")
      }else{
        frames_tres <- min(sapply(timeLag(m.stack,units = "secs"), min))
        out(paste0("Detected minimum temporal resolution of ", as.character(frames_tres) ," seconds, but non-uniform timestamps, paths interpolation will be forced."),type=2)
        out(paste0("'frames_tres' set to ",as.character(frames_tres)," seconds, since undefined."), type=2)
        out("Linear paths interpolation can massively distort movement, particularly non-movement periods!",type=2)
      }
      out("Computing uniform frame times from timestamps..."); disp <- TRUE
      m <- lapply(m, function(x, y = frames_tres){
        dt.start <- as.POSIXct(format(strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + ceiling(as.numeric(min(timestamps(x)))/y)*y,"%Y-%m-%d %H:%M:%S"),tz = "UTC")
        dt.stop <-  as.POSIXct(format(strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + floor(as.numeric(max(timestamps(x)))/y)*y,"%Y-%m-%d %H:%M:%S"),tz="UTC")
        dt.new <- seq(from = dt.start, to = dt.stop, by = y) #modify 'by' for output resolution, here 3600 is 1 hour (60*60)
        interpolateTime(x,time = dt.new,spaceMethod = "greatcircle")
      }) #equalizing resolutions
      m.stack <- moveStack(m)
    }
    if(disp == FALSE){out("Computing uniform frame times from timestamps...")}
    
    m.df <- lapply(m, function(x){
      t <- cbind(data.frame(coordinates(x)),data.frame(timestamps(x)))
      colnames(t) <- c("x","y","dt")
      return(t)
    }) #get df list
    m.lengths <- sapply(m.df, function(x){length(x[,1])}) #get lengths vector
    
    if(length(m) > 1){
      if(paths_mode == "simple"){
        m.df <- lapply(m.df, function(x, m = max(m.lengths)){
          if(length(x[,1]) < m){
            t <- data.frame(matrix(nrow = m-length(x[,1]) , ncol = length(x)))
            names(t) <- names(x)
            rbind(x,t)
          }else{x}
        })
      }
      if(paths_mode == "true_time" | paths_mode == "true_data"){
        times.start <- min(timestamps(m.stack))
        times.stop <- max(timestamps(m.stack))
        times.seq <- seq(from = times.start, to = times.stop,by = frames_tres)
        
        t <- data.frame(matrix(nrow = length(times.seq) , ncol = length(m.df[[1]])))
        colnames(t) <- c("x","y","dt")
        t$dt <- times.seq
        
        m.df <- lapply(m.df, function(x, t.df = t){
          t.df[match(x$dt,t.df$dt),] <- x
          return(t.df)
        })
        if(paths_mode == "true_data"){
          t <- do.call(cbind, m.df)
          t <- t[-seq(3,length(t),by=3)]
          t.sub.rm <- as.numeric(which(apply(t,1,function(x) return(length(which(is.na(as.numeric(x)))) == length(x)))))
          if(length(t.sub.rm) > 0){m.df <- lapply(m.df, function(x, rm.sub = t.sub.rm){x[-rm.sub,]})}
        }
      }
    } #creates data.frames by paths mode
    
    if(frames_nmax != 0){m.df <- lapply(m.df, function(x, l = frames_nmax){x[1:l,]})} #take first frames, if wanted
    if(frames_nres != 1){m.df <- lapply(m.df, function(x, l = frames_nres){x[seq(1,length(x[,1]),by = l),]})} #take every nth element, if wanted, BUG! include NA hold or not hold to avoid disappearance
    
    if(paths_na.hold == TRUE){m.df <- lapply(m.df, function(x){
      t <- x
      t[,1] <- na.locf(t[,1], na.rm = FALSE)
      t[,2] <- na.locf(t[,2], na.rm = FALSE)
      return(t)
      })
    }
    
    sub.val.true <- sapply(m.df, function(x){t <- apply(x, 2, function(x){is.na(x)})
      if(length(unique(t[,1])) > 1){return(TRUE)}else{if(unique(t[,1]) == TRUE){return(FALSE)}else{return(TRUE)}}
    })
    m.df <- m.df[which(sub.val.true == TRUE)] #select those df which have values instead of NAs only
    m.stack <- m.stack[[which(sub.val.true)]] #only for selected individuals, no uniform times applied!
    if(is.na(indi_names[1]) == FALSE){indi_names <- indi_names[which(sub.val.true == TRUE)]} #select indi names of individuals that are used
    global.times <- m.df[[1]]$dt #get global frame times
    
    
    ## Define global extent
    
    m.ext.list <- sapply(m.df,function(x){c(min(x$x,na.rm = TRUE),max(x$x,na.rm = TRUE),min(x$y,na.rm = TRUE),max(x$y,na.rm = TRUE))})
    m.ext <- extent(as.numeric(c(min(m.ext.list[1,]),max(m.ext.list[2,]),min(m.ext.list[3,]),max(m.ext.list[4,])))) #create move extent object
    m.corners <- rbind(c(m.ext@xmin,m.ext@ymin),c(m.ext@xmin,m.ext@ymax),c(m.ext@xmax,m.ext@ymax),c(m.ext@xmax,m.ext@ymin)) #create corner coordinates of move extet
    
    if(length(grep("+proj=longlat +ellps=WGS84", global.crs.str)) == 0){
      t <- SpatialPointsDataFrame(coords = m.corners, data = data.frame(m.corners), proj4string = global.crs)
      t <- spTransform(t, crs("+proj=longlat +ellps=WGS84"))
      m.corners <- coordinates(t)
    } #project corners, if necessary
    
    m.axis.dist <- c(distGeo(m.corners[1,],m.corners[4,]),distGeo(m.corners[1,],m.corners[2,]))
    m.axis.diff <- c(m.ext@xmax - m.ext@xmin,m.ext@ymax - m.ext@ymin) #compute distance and difference between coordinates
    if(m.axis.dist[1] < m.axis.dist[2]){
      global.ext <- extent((m.ext@xmin-(((m.axis.diff[1]/m.axis.dist[1])*m.axis.dist[2])-m.axis.diff[1])/2),
                           m.ext@xmax+(((m.axis.diff[1]/m.axis.dist[1])*m.axis.dist[2])-m.axis.diff[1])/2,
                           m.ext@ymin,m.ext@ymax)
    }else{
      global.ext <- extent(m.ext@xmin,m.ext@xmax,(m.ext@ymin-(((m.axis.diff[2]/m.axis.dist[2])*m.axis.dist[1])-m.axis.diff[2])/2),
                           m.ext@ymax+(((m.axis.diff[2]/m.axis.dist[2])*m.axis.dist[1])-m.axis.diff[2])/2)
    } #calculate a square extent by the factor of difference between distance and difference of coordinates
    
    #Define layer_dt, if single layer raster is used without defined layer_dt
    if(length(grep("aster",class(layer[[1]]))) == 1 & length(layer) == 1 & is.character(layer_dt[1])){layer_dt <- global.times[round(length(global.times)/2)]}
    
    # if(ext != 0){
    #   m.spdf <- lapply(m, function(x, y = global.crs){SpatialPointsDataFrame(coordinates(x),data=data.frame(timestamps(x)),proj4string = y)})
    #   m.spdf.crop <- lapply(m.spdf, function(x, y = ext){crop(x,y)})
    #   m <- lapply(m.spdf.crop, function(x, y = global.crs){
    #     if(is.null(x)){return(NULL)}else{move(x = coordinates(x)[,1], y = coordinates(x)[,2], time = c(x@data[,1]),proj = y)}})
    # }
  }else{
    global.crs <- crs(layer[[1]])
    global.crs.str <- as.character(global.crs)
    global.ext <- extent(layer[[1]])
    if(frames_nmax != 0){global.times <- layer_dt[1:frames_nmax]}else{global.times <- layer_dt}
  } #exclude, if animate_raster is called
  if(extent_factor != 0.0001){global.ext <- global.ext*(1+extent_factor)}
  
  ## Accesing and assigning/interpolating basemap raster per frame
  out("Assigning base layers per frame ticks...")
  
  if(raster_only != TRUE){
    if(layer[1] == "basemap"){
      try <- try(bm.gmap <- gmap(x = global.ext, exp=1, scale =1, type = map_type, lonlat = TRUE, rgb = TRUE, size=c(500,500))) #raster base layer
      if(class(try) == "try-error"){out("Download from Google failed. Please check your internet connection.",type=3)}
      if(length(grep("+proj=longlat +ellps=WGS84",global.crs.str)) == 0){bm.gmap <- projectRaster(from = bm.gmap, crs = global.crs)}
      bm.df <- data.frame(rasterToPoints(bm.gmap))
      bm.rgb <- rgb(bm.df$red,bm.df$green,bm.df$blue, maxColorValue = 255)
      bm.frames <- replicate(bm.gmap, n =length(global.times))
    }else{
      global.ext.r <- raster(xmn = global.ext@xmin, xmx = global.ext@xmax, ymn=global.ext@ymin, ymx = global.ext@ymax, resolution = res(layer[[1]]))
      global.ext.r <- setValues(global.ext.r,1)
      bm.crop <- lapply(layer, function(x,y=global.ext.r){crop(x,y,snap = "out")})
      bm.subs <- unlist(lapply(global.times, function(x, y=layer_dt){
        t <- abs(difftime(x,y,units = "mins"))
        which(t == min(t))
      }))
      if(layer_int == FALSE){
        bm.frames <- bm.crop[bm.subs]
      }else{
        bm.int.subs <- bm.subs[c(1,which(diff(bm.subs) != 0)+1)] #getting bm subs to be interpolated
        if(length(bm.int.subs) == 1){
          bm.frames <- bm.crop[bm.subs]
        }else{
          bm.int.in <- t(sapply(bm.crop[bm.int.subs], function(x){getValues(x)})) #building df for interpolation
          bm.int.in <- cbind(as.numeric(layer_dt[bm.int.subs]),bm.int.in)
          
          int.n = length(global.times)
          int.templ <- cbind(as.numeric(global.times), rep(NA, int.n))
          colnames(int.templ) <- c("time","x")
          
          bm.int.out <- apply(bm.int.in, MARGIN = 2, function(x, time = bm.int.in[,1], templ = int.templ){
            t <- which(is.na(x))
            if(length(t) == 0){
              i <- cbind(time,x)
              return(approxTime(i, as.numeric(templ[,1]))[,2])
            }else{
              if(length(t) == length(x)){return(templ[,2])}
              if(length(t) == length(x)-1){
                o <- templ[,2]
                o[which(x == is.numeric(x))] <- x[which(x == is.numeric(x))]
                return(o)
              }
            }
          }) #fill NA, fill one value or interpolate
          
          bm.int.out <- apply(bm.int.out, MARGIN = 2, function(x){
            t <- which(is.na(x))
            if(length(t)!= 0){x[t] <- x[min(t)-1]}
            return(x)
          }) #filling NAs with last interpolation value
          
          bm.frames <- apply(bm.int.out[,-1], MARGIN = 1, function(x, y = bm.crop[[1]]){setValues(y,x)}) #building back raster frames
        } #do not interpolate, if only one layer is nearest
      }
    }
  }else{
    if(frames_nmax != 0){bm.frames <- layer[1:frames_nmax]}else{bm.frames <- layer}
  }
  if(layer_type == "discrete"){bm.df <- lapply(bm.frames, function(x){
    t <- data.frame(rasterToPoints(x))
    colnames(t) <- c("x","y","value")
    return(t)
  })}
  
  #Recalculate extent on basis of basemap
  global.ext <- extent(bm.frames[[1]])

  
  
  ## Calculating legend breaks
  
  if(layer_type == "gradient" | layer_type == "discrete"){
    bm.stack <- stack(bm.frames)
    bm.stack.vals <- getValues(bm.stack)
    if(layer_type == "discrete"){
      legend_breaks <- seq(min(round(bm.stack.vals), na.rm = TRUE),max(round(bm.stack.vals),  na.rm = TRUE))
    }else{legend_breaks <- pretty(round(bm.stack.vals))}
    
    if(is.na(legend_limits[1])){legend_limits <- c(legend_breaks[1],legend_breaks[length(legend_breaks)])
    }else{legend_breaks <- legend_breaks[which(legend_breaks >= legend_limits[1] & legend_breaks <= legend_limits[2])]}
    if(legend_labels[1] == "auto"){legend_labels <- as.character(legend_breaks)
    }else{
      if(length(legend_labels) != length(legend_breaks)){
        legend_labels <- as.character(legend_breaks)
        out("Number of values to be displayed and length of 'legend_labels' are different. Legend labels are ignored therfore.",type=2)
      }
    }
    plt_limits <- "limits=c(legend_limits[1], legend_limits[2])"
  }
  if(layer_type == "RGB"){
    legend_breaks <- c(min(sapply(bm.frames, function(x){min(getValues(x), na.rm = TRUE)})),
                       max(sapply(bm.frames, function(x){max(getValues(x), na.rm = TRUE)})))
                        
  }
  
  ## static data
  
  if(!is.na(static_data[1])[1]){
    #Convert static_data to SPDF and crop it, then convert it back to a df
    static_spdf <- SpatialPointsDataFrame(coords = cbind(static_data$x,static_data$y),data = static_data,proj4string = global.crs)
    static_spdf <- crop(static_spdf,extent(bm.frames[[1]]))
    
    #Extract static data to df
    static_data <- as.data.frame(static_spdf)
    if(length(static_data) == 0){
      static_data <- NA
      out("'static_data' cannot be plotted, since they are out of the subset's extent.",type=2)
    }else{static_data <- static_data[1:(length(static_data)-2)]}
  }
  
  
  ## colour ramp, size vector etc.
  
  if(raster_only != TRUE){
    #Calculate path colour ramp
    if(paths_col == "auto"){collist <- c("red","green","blue","yellow","darkgreen","orange","deepskyblue", "darkorange","deeppink","navy")
    }else{collist <- paths_col}
    n_tail <- tail_elements-1
    
    if(length(m.df) > length(collist)){
      col_recycle <- as.integer(length(m.df)/length(collist))
      collist <- rep(collist, 1+col_recycle)
    }
    
    #Tail colours
    colours <- t(sapply(collist[1:length(m.df)], function(x, y = tail_elements){
      f <- colorRampPalette(c(x,"white"))
      rev(f(y+4)[1:y])
    }))
    
    #Size vector calculation depending on tail_elements
    size_divid <- round(tail_size/(length(colours[1,])+4),4)
    for(i in 1:length(colours[1,])){
      if(i==1){line_size <- tail_size}else{line_size[i] <- line_size[i-1]-size_divid}
    }
    line_size <- rev(line_size)
    
    #Calculate layer colour ramp
    if(layer_type == "discrete"){
      if(length(layer_col) != length(legend_breaks)){
        colfunc <- colorRampPalette(layer_col)
        layer_col <- colfunc(length(legend_breaks))
      }
    }
  }
  
  
  ## calculating stats

  if(raster_only != TRUE){
    if(stats_create == TRUE & layer[1] != "basemap"){
      
      out("Calculating statistics...")
      
      #Calc limits
      stats_limits <- round(legend_breaks,digits = stats_digits)
      if(stats_digits == 0){stats_floor_mp <- 1}else{stats_floor_mp <- 1/(as.numeric(paste(c("1",as.character(rep(0,stats_digits))),collapse = "")))}
      stats_empty <- data.frame(cbind(seq(min(stats_limits), max(stats_limits), by = stats_floor_mp), 0.))
      
      #Change stats_tframe for calculating stats per period
      stats_tframe <- stats_tframe+1
      
      stats_obj <- list()
      if(length(idData(m.stack)) == 0){out("Could not find individual names through calling move::idData. Names are assigned by the order of appearance.",type=2)}
      for(l in 1:nlayers(bm.frames[[1]])){
        bm.frames.lay <- lapply(bm.frames, function(x, b = l){x[[b]]})
        stats.val.freq.a <- lapply(m.df, function(x, b = bm.frames.lay, d = stats_digits){
          t <- NULL
          for(i in 1:length(b)){t <- rbind(t,round(extract(b[[i]],x[i,][1:2]),digits = d))}
          v <- c(na.omit(unique(t[,1])))
          freq <- sapply(v, function(x, a = t){
            o <- a; o[,1] <- NA
            o[which(a[,1] == x)] <- seq(1,length(which(a[,1] == x)),by = 1)
            
            #Set everyhing below first occurence to 0 and fill NAs with last frequency value
            o[,1][c(1:which(o[,1] == 1)-1)] <- 0
            o[,1] <- na.locf(o[,1])
            
            return(o)
          })
          colnames(freq) <- as.character(v)
          return(freq)
        })
        
        stats.val.freq.p <- lapply(stats.val.freq.a, function(x, st = stats_tframe){
          op <- apply(x, MARGIN = 2, FUN = function(x, tframe = st){
            o.period <- x[tframe:length(x)]-x[1:(length(x)-tframe+1)]
            o.period <- c(x[1:tframe-1],o.period)
            return(o.period)
          })
        })
        stats.val.freq <- list(stats.val.freq.a, stats.val.freq.p)
        
        
        #Build list of data frames
        stats.val.list <- list()
        for(i.p in 1:2){
          stats.val.list[[i.p]] <- lapply(seq(1,length(stats.val.freq[[i.p]])), function(x, v = stats.val.freq[[i.p]], temp = stats_empty, indi = get_indi(m.stack), cl = collist){
            temp[,2] <- indi[x]
            col <- cl[x]
            return(apply(v[[x]], MARGIN = 1,FUN = function(xx, t = temp, c = col){
              t[,3] <- 0
              t[,3][match(names(xx), as.character(t[,1]))] <- as.numeric(xx)
              t[,4] <- c
              colnames(t) <- c("val", "variable", "value", "cols")
              return(t)
            }))
          })
        }
        
        #Create stats data frame per frame
        for(i.p in 1:2){
          if(l == 1){stats_obj[[i.p]] <- list()}
          stats_obj[[i.p]][[l]] <- lapply(seq(1,length(global.times)), function(x, d = stats.val.list[[i.p]], temp = stats_empty, b = bm.frames, sd = stats_digits){
            t <- ldply(lapply(d, `[[`, x))
            
            #Create sum for all individuals
            sum <- temp; sum[,2] <- "sum"; sum[,3] <- 0
            sum[,3] <- apply(sapply(lapply(d, `[[`, x), function(x){c(x[,3])}), MARGIN = 1, sum)
            sum[,4] <- "black"
            colnames(sum) <- c("val", "variable", "value", "cols")
            
            if(i.p == 1){ #add histogram only for overall stats
              #Create histogram for this frame
              hist <- temp; hist[,2] <- "hist"; hist[,3] <- 0
              hist.v <- data.frame(table(round(getValues(b[[x]]),digits = sd)))
              hist[,3][as.numeric(na.omit(match(as.character(hist.v[,1]), as.character(hist[,1]))))] <- hist.v[,2][which(match(as.character(hist.v[,1]), as.character(hist[,1])) >0)]
              hist[,4] <- "grey"
              colnames(hist) <- c("val", "variable", "value","cols")
              rbind(t, sum, hist)
            }else{
              rbind(t, sum)
            }
          })
        }
      }
      stats_max <- sapply(stats_obj, function(x){sapply(x, function(x){max(sapply(x, function(x1){max(x1$value)}))})})
      stats_max <- stats_max + round(stats_max*0.1)
      cols <- stats_obj[[1]][[1]][[1]]$cols[seq(1,(n.indiv(m.stack)+2)*length(stats_empty[,1]), by = length(stats_empty[,1]))]
      vals <- unique(stats_obj[[1]][[1]][[1]]$variable)
      names(cols) <- vals
    }
  }
  
  
  ## define map elements
  out("Building map elements...",type=1)
  
  #Variables for scale bar and north arrow
  y_bm <- c(global.ext@ymin,global.ext@ymax)
  x_bm <- c(global.ext@xmin,global.ext@xmax)
  
  y_dist <- max(y_bm) - min(y_bm)
  y_dist_bar <- y_dist/80
  
  y_down <- min(y_bm)+((max(y_bm)-min(y_bm))/13)
  x_left <- min(x_bm)+((max(x_bm)-min(x_bm))/13)
  rec1_leftdown <- data.frame(x_left,y_down)
  colnames(rec1_leftdown) <- c("x","y")
  
  #Transform to latlon
  if(length(grep("+proj=longlat +ellps=WGS84",global.crs.str)) == 0){
    #Transform complete extent
    ext.ll <- as(global.ext, "SpatialPolygons")
    proj4string(ext.ll) <- global.crs.str
    ext.ll <- extent(spTransform(ext.ll, crs("+proj=longlat +ellps=WGS84")))
    
    #Transform scale bar leftdown coordinate
    rec1_leftdown_trans <- SpatialPointsDataFrame(coords = rec1_leftdown[,1:2], data = rec1_leftdown,
                                                  proj4string = global.crs)
    rec1_leftdown_trans <- spTransform(rec1_leftdown_trans, CRSobj = crs("+proj=longlat +ellps=WGS84"))
    rec1_leftdown_ll <- data.frame(extent(rec1_leftdown_trans)@xmin); rec1_leftdown_ll <- cbind(rec1_leftdown_ll,extent(rec1_leftdown_trans)@ymin)
    colnames(rec1_leftdown_ll) <- c("x","y")
  }else{
    ext.ll <- global.ext
    rec1_leftdown_ll <- rec1_leftdown
  }
  
  x_dist <- (distGeo(c(ext.ll@xmin,ext.ll@ymin),
                     c(ext.ll@xmax,ext.ll@ymin)))/1000 #in km
  if(scalebar_dist == "auto"){
    x_dist_bar <- round((x_dist/8)*4)/4
    if(x_dist_bar == 0){x_dist_bar <- round(((x_dist/8)*4),1)/4}
  }else{x_dist_bar <- scalebar_dist/2}
  
  rec1_rightdown_ll <- data.frame(gcDestination(lon = rec1_leftdown_ll$x, lat = rec1_leftdown_ll$y, bearing = 90, dist = x_dist_bar, dist.units = "km", model = "WGS84"))
  
  if(length(grep("+proj=longlat +ellps=WGS84",global.crs.str)) == 0){  
    x_right <- (spTransform(SpatialPointsDataFrame(coords = rec1_rightdown_ll[,1:2], data = rec1_rightdown_ll,proj4string = crs("+proj=longlat +ellps=WGS84")),
                            CRSobj = global.crs))$long
  }else{x_right <- rec1_rightdown_ll$long}
  
  y_up <- y_down+y_dist_bar
  rec1_rightup <- data.frame(x_right,y_up)
  colnames(rec1_rightup) <- c("x","y")
  rec1 <- data.frame(rec1_leftdown); rec1 <- rbind(rec1,c(rec1_rightup$x,rec1_leftdown$y),rec1_rightup,c(rec1_leftdown$x,rec1_rightup$y))
  
  rec2_leftdown <- data.frame(rec1_rightup$x,rec1_leftdown$y); colnames(rec2_leftdown) <- c("x","y")
  rec2_rightup <- data.frame((rec1_rightup$x+(rec1_rightup$x - rec1_leftdown$x)),rec1_rightup$y); colnames(rec2_rightup) <- c("x","y")
  rec2 <- data.frame(rec2_leftdown); rec2 <- rbind(rec2,c(rec2_rightup$x,rec2_leftdown$y),rec2_rightup,c(rec2_leftdown$x,rec2_rightup$y))
  
  leg_dist <- y_dist_bar*1.8
  leg_y <- rec1_rightup$y+leg_dist
  leg_coords <- data.frame(rec1_leftdown$x, leg_y); leg_coords <- rbind(leg_coords,c(rec2_leftdown$x,leg_y),c(rec2_rightup$x, leg_y))
  colnames(leg_coords) <- c("x","y")
  leg_text <- c(0,x_dist_bar,x_dist_bar*2)
  
  x_arrow <- global.ext@xmin+((global.ext@xmax-global.ext@xmin)/1.1)
  y_arrow_start <- min(y_bm)+((max(y_bm)-min(y_bm))/11)
  y_arrow_end <- global.ext@ymin+((global.ext@ymax-global.ext@ymin)/8)
  arrow <- data.frame(x_arrow,y_arrow_start); arrow <- rbind(arrow,c(x_arrow,y_arrow_end))
  colnames(arrow) <- c("x","y")
  
  #Create ggplot object to get deviating plot extent
  if(is.character(layer) == FALSE){bm.gg <- gplot(bm.frames[[1]]) + geom_tile(aes_(fill = ~value)) +
    scale_fill_gradientn(colours = layer_col, guide=guide_colourbar(title = legend_title, label.vjust = 0.9, title.hjust = 0, title.vjust = 0)) +
    scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) + theme(aspect.ratio=1)
  }else{bm.gg <- ggplot(data=bm.df,aes_(x=~x, y=~y)) + geom_tile(aes(fill = bm.rgb)) + scale_fill_identity() +
    scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) + theme(aspect.ratio=1)}
  bm.gg.build <- ggplot_build(bm.gg)
  
  #Variables for progress bar
  n_loop <- length(bm.frames)-tail_elements
  prog_bar_length <- n_loop-2
  prog_bar_elem_length <- (max(bm.gg.build$data[[1]]$xmax)-min(bm.gg.build$data[[1]]$xmin))/(prog_bar_length+1)
  for(i in 1:prog_bar_length){
    if(i == 1){prog_x_st <- min(bm.gg.build$data[[1]]$xmin)}
    else{prog_x_st <- c(prog_x_st, prog_x_st[i-1]+prog_bar_elem_length)}
  }
  prog_x_end <- c(prog_x_st[1:prog_bar_length],(prog_x_st[prog_bar_length]+prog_bar_elem_length),max(bm.gg.build$data[[1]]$xmax))
  prog_y <- max(bm.gg.build$data[[1]]$ymax)
  
  #time_scale annotation
  global.times.str <- as.character(global.times)
  
  
  
  ## creating plot strings
  
  #Define labs
  if(img_labs[1] != "labs"){labs_x <- img_labs[1]; labs_y <- img_labs[2]
  }else{
    if(length(grep("+proj=longlat",global.crs.str)) != 0){
      labs_x <- "Longitude"; labs_y <- "Latitude"
    }else{labs_x <- "x"; labs_y <- "y"}
  }
  
  if(img_title != "title"){
    plt.title <- paste0('+ labs(x = "',labs_x,'", y="',labs_y,'", title="',img_title,'", label=c("123")) + theme(plot.title = element_text(hjust = 0.5))')
    if(img_sub != "subtitle"){
      
      #Seperating lines due to input length
      line_break <- 70
      if(nchar(img_sub)>line_break){
        delim <- round(nchar(img_sub)/line_break)
        img_sub_splt <- unlist(strsplit(img_sub, " "))
        delim_part <- round(length(unlist(img_sub_splt))/delim)
        
        for(i in 1:length(img_sub_splt)){
          if(i == 1){img_sub_breaked <- img_sub_splt[i]}
          else{
            if(i == delim_part & i != length(img_sub_splt)){
              img_sub_breaked <- paste(img_sub_breaked, img_sub_splt[i],"\n")
              delim_part <- delim_part+delim_part
            }
            else{img_sub_breaked <- paste(img_sub_breaked, img_sub_splt[i])}
          }
        }
        img_sub <- img_sub_breaked
      }
      plt.title <- paste0('+ labs(x = "',labs_x,'", y="',labs_y,'", title="',img_title,'", subtitle="',img_sub,'", label=c("123","456"))+
                          theme(plot.title = element_text(hjust = 0.5),
                          plot.subtitle = element_text(hjust = 0.5))')
    }
    if(img_caption != "caption"){
      if(img_sub != "subtitle"){
        plt.title <- paste0('+ labs(x = "',labs_x,'", y="',labs_y,'", title="',img_title,'", subtitle="',img_sub,'", caption="',img_caption,'",label=c("123","456","789"))+
                            theme(plot.title = element_text(hjust = 0.5),
                            plot.subtitle = element_text(hjust = 0.5),
                            plot.caption = element_text(hjust = 0.5))')
      }else{
        plt.title <- paste0('+ labs(x = "',labs_x,'", y="',labs_y,'", title="',img_title,'", caption="',img_caption,'",label=c("123","456"))+
                            theme(plot.title = element_text(hjust = 0.5), 
                            plot.caption = element_text(hjust = 0.5))')
      }
      }
      }else{
        if(img_caption != "caption"){plt.title <- paste0('+ labs(x = "',labs_x,'", y="',labs_y,'", caption="',img_caption,'",label=c("123"))+
                                                         theme(plot.caption = element_text(hjust = 0.5))')
        }else{plt.title <- paste0('+ labs(x = "',labs_x,'", y="',labs_y,'")')}
        }
  
  #Defining map elements
  if(map_elements == TRUE){
    plt_scale_north <- '+ geom_polygon(data = ld[[8]], aes_(x = ~x, y = ~y), fill = "white", colour = "black") + geom_polygon(data = ld[[9]], aes_(x = ~x, y = ~y), fill = "black", colour = "black") + annotate("text", label = paste(leg_text, " km", sep=""), x = leg_coords$x, y = leg_coords$y, size = 3, colour = scalebar_col) + geom_line(arrow=arrow(length=unit(3.7,"mm")),data = arrow, aes_(x=~x, y=~y), colour=north_col,size=1.06) + annotate(x=x_arrow, y=y_down, label="N", colour=north_col, geom="text", size=6.5)'
  }else{plt_scale_north <- ''}
  if(time_scale == TRUE){ #leg_coords$x[2]  rec1_leftdown$y-leg_dist
    plt_time_scale <- paste0('+ annotate("text", label = ld[[7]][[x]], x = ',as.character(x_bm[[1]]+(x_bm[[2]]-x_bm[[1]])/2),', y = ',as.character(y_bm[[2]]-(abs(leg_dist)*1.5)),', size = 3, colour = "',scalebar_col,'")')
  }else{plt_time_scale <- ''}
  plt_progress <- '+ geom_line(data = prog_bar, aes_(x=~x,y=~y),colour="grey",size=1.8)'
  
  
  #Defining static_data
  if(!is.na(static_data[1])[1]){
    if(is.na(static_gg)){plt_static <- '+geom_point(data = static_data, aes_(x= ~x, y= ~y),shape=23, fill="white", color="black", size=3) + geom_label(data = static_data, aes_(x= ~x, y= ~y, label= ~names),size=3,colour="black",position = position_nudge(y = -(leg_dist*1.5)))' #scalebar_col
    }else{plt_static <- paste0("+ ",static_gg)}
  }else{plt_static <- ''}
  
  
  #basemap plot
  if(layer_type == ""){
    plt.bm <- "ld[[2]]"
  }
  if(layer_type == "gradient"){
    plt.bm <- paste0('gplot(ld[[10]][[x]]) + geom_tile(aes_(fill = ~value)) + scale_fill_gradientn(colours =c(', paste0('"',layer_col,'"',collapse = ", "),'), na.value = "', as.character(layer_nacol) ,'", ',plt_limits,', guide=guide_colourbar(title = "',legend_title, '", label.vjust = 0.9, title.hjust = 0, title.vjust = 0)) + scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) + theme(aspect.ratio=1)')
  }
  
  if(layer_type == "discrete"){
    plt.bm <- paste0('ggplot(data=ld[[10]][[x]], aes_(x=~x, y=~y)) + geom_tile(aes(fill = factor(value))) + scale_fill_manual(values = c(setNames(ld[[11]], 1:length(ld[[11]]))), labels = c(', paste0('"',legend_labels,'"',collapse = ", "),'), drop = FALSE, na.value = "', as.character(layer_nacol) ,'", guide = guide_legend(title = "',legend_title ,'", label = TRUE, label.vjust = 0.9, title.hjust = 0, title.vjust =0)) + scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) + theme(aspect.ratio=1)')
  }
  if(layer_type == "RGB"){
    plt.bm <- paste0('ggRGB(ld[[10]][[x]],r=3,g=2,b=1,stretch="',layer_stretch,'") + scale_fill_identity() + scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) + theme(aspect.ratio=1)')
  }
  
  #stats plot function
  if(stats_create == TRUE){
    if(stats_gg != ""){
      plt_stats_parse = parse(text = stats_gg)
    }else{
      if(stats_type == "line"){
        plt_stats_parse = parse(text = 'ggplot(data = stats_obj[[k]][[b]][[x]], aes_(x = ~val, y = ~value, colour = ~variable)) + 
                                geom_line() + geom_point() + theme_bw() + theme(aspect.ratio=1) +
                                scale_y_continuous(expand = c(0,0),limits = c(0,stats_max[k])) +
                                scale_x_continuous(expand = c(0,0)) + 
                                scale_color_manual(name= "",values = cols) +
                                labs(x = "Basemap Value", y="Frequency", title=stats_title[[b]][[k]], label=c("123","456"))+
                                theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))')
      }
      if(stats_type == "bar"){
        plt_stats_parse = parse(text = 'ggplot(data = stats_obj[[k]][[b]][[x]], aes_(x = ~val, y = ~value, fill = ~variable)) + 
                                scale_fill_manual(name= "",values = cols, breaks = names(cols)) +
                                geom_bar(stat = "identity", position = "dodge") + theme_bw() + theme(aspect.ratio=1) +
                                scale_y_continuous(expand = c(0,0),limits = c(0,stats_max[k])) +
                                scale_x_continuous(expand = c(0,0)) +
                                labs(x = "Basemap Value", y="Frequency", title=stats_title[[b]][[k]], label=c("123","456"))+
                                theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))')
      }
    }
  }
  
  #Extract stats legend
  if(stats_create == TRUE){k <- 1; b <- 1; x <- 1; stleg <- quiet(g_legend(eval(plt_stats_parse)))}
  
  #Compute plot identifiers
  frames_layout_id <- matrix(seq((min(frames_layout,na.rm = TRUE)-min(frames_layout,na.rm = TRUE)+1),max(frames_layout,na.rm = TRUE))[1:length(sort(unique(c(frames_layout))))][match(frames_layout, sort(unique(c(frames_layout))))], length(frames_layout[,1]))
  pl_id <- sort(unique(c(frames_layout)))[which(sort(unique(c(frames_layout)))!= 8)] #excluding st legend
  
  
  ani.duration.time <- seconds_to_period(round(length(global.times)/frames_fps))
  ani.duration.time <- sprintf('%02d:%02d:%02d', hour(ani.duration.time), minute(ani.duration.time), second(ani.duration.time))
  out(paste0("Duration of output animation file: ", toString(ani.duration.time), " (hh:mm:ss) [", toString(length(global.times)-tail_elements)," frames, ", toString(frames_fps)," fps]"), type = 1)
  
  
  ## create frames
  out("Drawing frames...", type=1)
  
  n_reloop <- ceiling(n_loop/conv_frames)
  #Calculate gif frame maxima for each gif
  for(r in 1:n_reloop){
    if(r == 1){index_list <- floor(n_loop/n_reloop)}
    else{
      if(r != n_reloop){index_list <- c(index_list,(floor(n_loop/n_reloop)+index_list[r-1]))}
      else{index_list <- c(index_list,n_loop)}
    }
  }
  if(log_level == 1 & shiny_mode == FALSE){p.out <- timerProgressBar(min = 0, max = n_loop-1+n_reloop, width = (getOption("width")-25), style = 3, char = "=")} #txtProgressBar #pboptions(type = "timer", char = "=", txt.width = (getOption("width")-25))}
  if(shiny_mode == "ani"){
    progress <- Progress$new(shiny_session, min=1, max=n_loop-1+n_reloop)
    progress$set(message = 'Animating data\n', detail = 'This may take a while...')
  }
  
  #Draw plots per frame
  if(raster_only != TRUE){
    in.data.list <- list(m.df, bm.gg, n_tail, colours, line_size, paths_alpha, global.times.str,
                         rec1, rec2, if(layer_type == "discrete"){bm.df}else{bm.frames}, layer_col,
                         if(stats_create == TRUE){stats_obj}else{NA}, pl_id, frames_layout, if(stats_create == TRUE){stleg}else{NA},
                         frames_layout_id, frames_width, frames_height, frames_pixres)
  }else{
    in.data.list <- list(NA,bm.gg, NA, NA, NA, NA, global.times.str,
                         rec1, rec2, if(layer_type == "discrete"){bm.df}else{bm.frames}, layer_col,
                         NA, pl_id, frames_layout, NA,
                         frames_layout_id, frames_width, frames_height, frames_pixres)
  }
  in.plt.list <- list(plt.bm, plt.title, plt_scale_north, plt_time_scale, plt_progress, plt_static, if(stats_create == TRUE){plt_stats_parse}else{NA})
  in.cond.list <- list(raster_only, stats_only, stats_create)
  
  #Initiate cluster for par processing
  #if(par == TRUE){cl <- makeCluster(n_cores)}
  
  p.dir <- quiet(sapply(seq(1:(length(global.times)-tail_elements)), function(x, ld = in.data.list, lp = in.plt.list, lc = in.cond.list, dir = temp_dir){ #length(global.times)
    if(log_level == 1 & shiny_mode == FALSE){setTimerProgressBar(p.out, x)}
    if(shiny_mode == "ani"){progress$set(value = x)}

    prog_bar <- data.frame(prog_x_st[1],prog_y); prog_bar <- rbind(prog_bar,c(prog_x_end[x],prog_y))
    colnames(prog_bar) <- c("x","y")

    if(lc[[1]] == TRUE){plt.path <- ""}else{
      t.str <- c('geom_path(data = data[[',']][,1:2],aes_(x = data[[',']]$x, y = data[[',']]$y, alpha = ld[[6]]), lineend = "round", linejoin = "round", colour = ld[[4]][',',][1:length(data[[',']]$x)], size = ld[[5]][1:length(data[[',']]$x)],na.rm=TRUE, show.legend = FALSE)')
      plt.path <- "+ "
      data <- list()

      #Calcualte plot string per individual
      for(g in 1:length(ld[[1]])){
        if(x < ld[[3]]){data[[g]] <- ld[[1]][[g]][1:x,]}else{data[[g]] <- ld[[1]][[g]][(x-ld[[3]]):x,]}
        xs <- as.character(g)
        plt.path <- paste0(plt.path, t.str[1], xs, t.str[2], xs, t.str[3], xs, t.str[4], xs, t.str[5], xs, t.str[6], xs, t.str[7], if(g < length(ld[[1]])){' + '})
      }
    }

    #Create plot string
    plt.fin <- paste0(lp[[1]], plt.path, lp[[2]] , lp[[3]], lp[[4]], lp[[5]], lp[[6]])

    plt.parse <- parse(text = plt.fin)

    #Open PNG device
    fn <- paste0("p",as.character(x),".png")
    png(filename = fn,width = ld[[17]], height = ld[[18]],units = "px",bg = "white", res = ld[[19]])

    pl <- list()
    if(lc[[2]] == TRUE){pl[[1]] <- NA}else{pl[[1]] <- quiet(eval(plt.parse))} #spatial plot needed or not
    if(lc[[3]] == TRUE){
      for(b in 1:length(ld[[12]][[k]])){
        for(k in 1:length(ld[[12]])){
          pl[[length(pl)+1]] <- quiet(eval(lp[[7]]))
        }
      }
      if(length(pl) != 7){pl[length(pl)+1:7]=NA}
    }else{pl[2:7] <- NA}
    pl <- pl[ld[[13]]] #select plots from frames_layout
    if(lc[[2]] == FALSE){
      pl[[1]] <- ggplotGrob(pl[[1]])
      if(length(pl) > 1){pltemp <- pl[2:length(pl)]; pl[2:length(pl)] <- lapply(pltemp, FUN = function(pltemp){ggplotGrob(pltemp + theme(legend.position = "none"))})}
    }else{pl <- lapply(pl, FUN = function(pl){ggplotGrob(pl + theme(legend.position = "none"))})}
    if(length(which(ld[[14]]==8))!= 0){pl[[length(pl)+1]] <- ld[[15]]}
    quiet(do.call("grid.arrange",args = list(grobs=pl,layout_matrix=ld[[16]]))) #grobs=c(pl,stleg)

    dev.off()
    return(fn)
  }))
  
  #if(par == TRUE){stopCluster(cl)}
  
  p.dir.na <- which(match(p.dir,NA) == 1)
  if(length(p.dir.na) != 0){p.dir <- p.dir[-p.dir.na]}
  
  if(out_format == "gif"){
    og.dir <- sapply(seq(1:n_reloop), function(x, il = index_list, d = p.dir, cd = conv_dir, cm = conv_cmd, fi = frames_fps, n = n_loop){
      if(log_level == 1 & shiny_mode == FALSE){setTimerProgressBar(p.out, (n+x))}
      if(shiny_mode == "ani"){progress$set(value = n+x)}
      if(x == 1){range = c(0,il[x])}else{range = c(il[x-1], il[x])}
      
      batch <- paste0('"',cd,'" ', cm,' -loop 0 -delay ',toString(100%/%fi),' ',paste0(d[range[1]:range[2]],collapse = " "),' out',toString(x),'.', out_format)
      
      if(.Platform$OS.type == 'windows'){write(batch,"batch.bat"); quiet(cmd.fun("batch.bat >nul 2>1"))
      }else{write(batch,"batch.bat"); system("chmod +x batch.bat"); quiet(cmd.fun("./batch.bat"))}
      file.remove("batch.bat")
      return(paste0('out',toString(x),'.',out_format))
    })
    
    if(n_reloop > 1){
      batch <- paste0('"',conv_dir,'" ', conv_cmd,' -loop 0 -delay ',toString(100%/%frames_fps),' ',paste0(og.dir,collapse = " "),' ',out_name,'.', out_format)
      if(.Platform$OS.type == 'windows'){write(batch,"batch.bat"); quiet(cmd.fun("batch.bat >nul 2>1"))
      }else{write(batch,"batch.bat"); system("chmod +x batch.bat"); quiet(cmd.fun("./batch.bat"))}
      file.remove("batch.bat")
    }else{
      file.rename(paste0("out1.",out_format),paste0(out_name,".",out_format))
    }
  }else{
    quiet(cmd.fun(paste0(conv_dir, ' -i ',conv_cmd,' p%d.png -vcodec libx264 -pix_fmt yuv420p -r ',toString(frames_fps),' ',out_name,'.',out_format),ignore.stdout = TRUE,ignore.stderr = TRUE))
  }
  
  #Cleaning up
  file.copy(paste0(out_name,".",out_format),paste0(out_dir,"/",out_name,".",out_format), overwrite = TRUE)
  file.remove(list.files(temp_dir))
    
  if(log_level == 1 & shiny_mode == FALSE){closepb(p.out)}
  if(shiny_mode == "ani"){progress$close()}
  
  setwd(user_wd) #reset to user wd
  
  run.stop <- Sys.time()
  run.dur <- as.character(round(difftime(run.stop,run.start,units = "mins"),digits = 2))
  out(paste0("Total run time: ",run.dur," minutes"))
  
  if(file.exists(paste0(out_dir,'/',out_name,'.',out_format))){
    if(shiny_mode == FALSE){out(paste0("Done. '",out_name,".",out_format,"' has been saved to '",out_dir,"'."), type=1)}else{out("Done.")}
    if(log_logical == TRUE){return(TRUE)}
  }else{
    out("animate_move failed due to unknown error.",type=3)
  }
}