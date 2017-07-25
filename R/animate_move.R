#' Animate movement data
#'
#' \code{animate_move} animates movement data provided as \code{move} class objects or a list of them. The function creates an animated GIF file and saves it into the output directory. \code{animate_move} can be operated in different timing modes (see \code{paths_mode}) and with different background layer types (see \code{layer}, \code{layer_type} and \code{map_type}). 
#'
#' @param data_ani list or \code{moveStack} class object. Needs to contain one or several \code{move} class objects (one for each individual path to be displayed) containing point coordinates, timestamps and projection.
#' @param out_dir character. Output directory for the GIF file creation.
#' @param conv_dir character. Command or directory to call the ImageMagick convert tool (default to be \code{convert}). You can use \code{conv_dir = get_imconvert()} to search for the right command/tool directory and/or get the required software.
#' @param layer raster, list or character "basemap". Single raster object or list of raster objects to be used as (dynamically changing) basemap layer. Default is \code{"basemap"} to download a static basemap layer. Use a rasterBrick class object and set layer_type to "\code{RGB}" to compute a RGB basemap.
#' @param layer_dt POSIXct or list. Single POSIXct date/time stamp or list of POSIXct date/time stamps corresponding to the acquisition dates of the \code{layer} raster objects.
#' @param layer_int logical. Whether to interpolate the basemap layer objects over time, if several are provided (\code{TRUE}), or to display them one after another depending on the animation time frame that is displayed (\code{FALSE}). Default is \code{FALSE}.
#' @param layer_type charachter. Layer type. Can be either "\code{RGB}" (if layer is a rasterBrick class onejct), "\code{gradient}" or "\code{discrete}". Default is "\code{RGB}". Ignored, if \code{layer = "basemap"}.
#' @param layer_col character vector.  Two or more colours to be used for displaying the background layer. If \code{layer_type = "gradient"}, a colour ramp between the colous is calcualted. If \code{layer_type = "discrete"}, the colours will be used per value range. Ignored, if \code{layer_type = "RGB"}.
#' @param layer_nacol character. Colour to be displayed for NA values. Default is "white".
#' @param map_type character.  Static basemap type. Chosse from "roadmap", "satellite", "hybrid", "terrain".
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
#' @param map_elements logical. If \code{FALSE}, map elements (north arrow and scale bar) are hidden. Default is \code{TRUE}.
#' @param extent_factor numeric. Defines the distance between the spatial extents of the movement data set and the basemap as proportion of the axis distance. Default is 0.0001. The higher the value, the larger the basemap extent. Ignored, if \code{layer} = "basemap".
#' @param north_col character. Colour of the north arrow. Default is "white".
#' @param paths_col character vector. Colours of the individual animation paths. If set to "auto", a predfined colour set will be used. If single colour, all paths will be displayed by the same colour. If more individuals then colours, the colours are repeated.
#' @param paths_alpha numeric. Set transparency of pathes. If set to 0, path is invisible. Default is 1.
#' @param paths_mode character vector. Mode to be used for dealing with time information when displaying multiple individual paths. If set to "true_data", paths are displayed based on true coverage times, showing only time periods that are covered. Time gaps will be skipped. Each frame is linked to a specific true time. If set to "true_time",  paths are displayed based on true coverage times. Time gaps will be filled with non-movement frames. This mode is only recommended, if the dataset has no time gaps. Each frame is linked to a specific, true time. If set to "simple", all movement paths are displayed individually with no regard to the true coverage times. Time gaps will be skipped. Each frame displays several times at once, since each individual path has its own time. Default is "true_data".
#' @param frames_nmax numeric. Number of maximum frames. If set, the animation will be stopped, after the specified number of frames is reached. Default is 0 (displaying all frames).
#' @param frames_interval numeric. Duration, each frame is displayed (in seconds). Default is .04.
#' @param frames_nres numeric. Interval of which frames of all frames should be used (nth elements). Default is 1 (every frame is used). If set to 2, only every second frame is used.
#' @param frames_width numeric. Number of pixels of frame width. Default is 600 (with stats plots 1000).
#' @param frames_height numeric. Number of pixels of frame height. Defualt is 600.
#' @param out_name character. Name of the output file. Default is "final_gif".
#' @param log_level numeric. Level of console output given by the function. There are three log levels. If set to 3, no messages will be displayed except erros that caused an abortion of the process. If set to 2, warnings and errors will be displayed. If set to 1, a log showing the process activity, wanrnings ans errors will be displayed.
#' @param log_logical logical. For large processing schemes. If \code{TRUE}, the function returns \code{TRUE} when finished processing succesfully.
#' @param stats_create logical. \code{TRUE} to create statistic plots side by side with the spatial plot. Use the arguments explained for \code{\link{animate_stats}} to adjust the plotting behaviour. Default is \code{FALSE}.
#' @param ... optional arguments. All arguments taken by \code{\link{animate_stats}} can be handed over to \code{\link{animate_move}} as well to create sidy-by-side spatial and statistic plot animations (see \code{\link{animate_stats}}).
#' 
#' @return None or logical (see \code{log_logical}. The output GIF file is written to the ouput directory.
#' 
#' @details \code{animate_move} is based on \code{ggplot2} and partly based on the \code{animation} package. It needs the \code{convert} tool of the \code{ImageMagick} software package to assemble GIF files. The command or directory to the convert tool needs to be provided with \code{conv_dir}. Please use \code{\link{get_imconvert}} to search for the convert command/tool directory on your system or to automatically download and install the required software. See \code{\link{get_imconvert}} for details.
#' 
#' @examples
#' #Load move and moveVis packages
#' library(move)
#' library(moveVis)
#' 
#' #Get the sample data from the moveVis package
#' data("move_data")
#' move_data$dt <- as.POSIXct(strptime(move_data$dt, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
#'
#' #Create move class object
#' data_ani <- split(move(move_data$lon, move_data$lat, proj=CRS("+proj=longlat +ellps=WGS84"),
#' 	time = move_data$dt, animal=move_data$individual, data=move_data))
#' 
#' #Find command or directory to convert tool of ImageMagick
#' conv_dir <- get_imconvert()
#' 
#' #Specify output directory
#' out_dir <- "/out/test"
#' 
#' #Specify some optional appearance variables
#' img_title <- "Movement of the white stork population at Lake Constance, Germany"
#' img_sub <- paste0("including individuals ",names(data_ani))
#' img_caption <- "Projection: Geographical, WGS84; Sources: Movebank 2013; Google Maps"
#' 
#' #Call animate_move()
#' animate_move(data_ani, out_dir, conv_dir, tail_elements = 10,
#'              paths_mode = "simple", frames_nmax = 50,
#'              img_caption = img_caption, img_title = img_title,
#'              img_sub = img_sub, log_level = 1)
#'
#' @author Jakob Schwalb-Willmann
#' @seealso \code{\link{get_imconvert}}, \code{\link{animate_stats}}, \code{\link{animate_raster}}
#' 
#' @import ggplot2
#' @importFrom animation ani.options
#' @importFrom raster crs extent projectRaster raster getValues setValues rasterToPoints res crop extract
#' @importFrom xts align.time
#' @importFrom sp SpatialPointsDataFrame spTransform
#' @importFrom geosphere distGeo
#' @importFrom dismo gmap
#' @importFrom maptools gcDestination
#' @importFrom rasterVis gplot
#' @importFrom grid arrow unit
#' @importFrom move move
#' @importFrom move idData
#' @importFrom grDevices dev.off rgb colorRampPalette
#' @importFrom utils head
#' @importFrom methods is
#' @importFrom stats approxfun na.omit setNames
#' @importFrom gridExtra grid.arrange
#' @importFrom reshape melt
#' @importFrom graphics plot
#' @export

animate_move <- function(data_ani, out_dir, conv_dir = "convert",
                         layer = "basemap", layer_dt = "basemap", layer_int = FALSE, layer_type = "",
                         layer_col = c("sandybrown","white","darkgreen"), layer_nacol = "white", map_type="satellite",
                         extent_factor = 0.0001, tail_elements = 10, tail_size = 4,
                         img_title = 'title', img_sub = 'subtitle', img_caption = "caption", img_labs = "labs",
                         legend_title = "", legend_limits = NA, legend_labels = "auto",
                         map_elements = TRUE, scalebar_col = "white", north_col = "white",
                         paths_col = "auto", paths_alpha = 1, paths_mode = "true_data", stats_create = FALSE, 
                         frames_nmax =  0, frames_interval = .04, frames_nres = 1, frames_width = NA, frames_height = NA,
                         out_name = "final_gif", log_level = 1, log_logical = FALSE, ...){
  
  #Define output handling
  out <- function(input,type = 1){
    signs <- c("[LOG]: ", "[WARNING]: ")
    if(type == 2 & log_level <= 2){print(paste(signs[2],input))}
    else{if(type == 3){stop(input,call. = FALSE)}else{if(log_level == 1){print(paste(signs[1],input))}}}
  }
  
  #Suppress messages and warnings
  quiet <- function(expr){
    return(suppressWarnings(suppressMessages(expr)))
  }

  
  #GIF creation function (adjusted code taken from the saveGIF function of the animation package)
  createGIF = function(
    expr, movie.name = 'animation.gif', img.name = 'R', img.dir = 'dir', out_dir = 'dir',
    cmd.fun, clean = TRUE, ...
  ) {
    oopt = ani.options(...)
    on.exit(ani.options(oopt))
    if(!dir.exists(dirname(movie.name))){
      dir.create(dirname(movie.name))
    }
    ## create images in the temp dir
    out("Creating PNG frame files (this may take a while; depending on the spatial resolution of the basemap)...",type=1)
    if(img.dir == 'dir'){img.dir <- tempdir()}
    if(out_dir == 'dir'){img.dir <- tempdir()}
    
    owd <- setwd(img.dir)
    on.exit(setwd(owd), add = TRUE)
    
    file.ext = ani.options('ani.type')
    
    ## clean up the files first
    unlink(paste(img.name, '*.', file.ext, sep = ''))
    
    ## draw the plots and record them in image files
    ani.dev = ani.options('ani.dev')
    if (is.character(ani.dev)) ani.dev = get(ani.dev)
    img.fmt = paste(img.name, '%d.', file.ext, sep = '')
    if ((use.dev <- ani.options('use.dev')))
      ani.dev(file.path(img.dir, img.fmt), width = ani.options('ani.width'),
              height = ani.options('ani.height'))
    in_dir <- function(dir, code) {
      old <- setwd(dir)
      on.exit(setwd(old))
      
      force(code)
    }
    in_dir(owd, expr)
    if (use.dev) dev.off()

    img.files <- sprintf(img.fmt, seq_len(length(list.files(
      pattern = paste(img.name, '[0-9]+\\.', file.ext, sep = '')
    ))))
    
    if (missing(cmd.fun))
      cmd.fun = if (.Platform$OS.type == 'windows') shell else system
    
    #Define arguments to be passed to convert.exe
    loop = paste0("-loop ", as.character(ifelse(isTRUE(ani.options('loop')), 0, ani.options('loop'))))
    interval = paste0("-delay ",as.character(head(ani.options('interval'), length(img.files))*100))
    str_img_files <- gsub(",", "", toString(img.files))
    
    #Assemble batch line and write to batch file
    out("Creating GIF file...",type=1)
    batch <- paste0('"',ani.options()$convert,'" ',loop,' ',interval,' ',str_img_files,' "',out_dir,'/',movie.name,'"')
    
    if(.Platform$OS.type == 'windows'){
      write(batch,"batch.bat")
      quiet(cmd.fun("batch.bat"))#,show.output.on.console = FALSE)
    }else{
      write(batch,"batch.bat")
      system("chmod +x batch.bat")
      quiet(cmd.fun("./batch.bat"))
    }
    file.remove(img.files)
    file.remove("batch.bat")
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
  
  #+++++++++++++++++++++++++++++++++++++++ MAIN ++++++++++++++++++++++++++++++++++++++++++++++
  
  #[1] PREREQUISITES
  out("Checking prerequisites...",type=1)
  
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
    if(layer_type == "discrete"){stats_digits <- 0}   
  }else{stats_digits <- arg$stats_digits}
  s_try <- try(arg$stats_tframe)
  if(class(s_try) == "NULL"){stats_tframe  <- 5}else{stats_tframe <- arg$stats_tframe}
  s_try <- try(arg$stats_lay)
  if(class(s_try) == "NULL"){stats_lay  <- 0}else{stats_lay <- arg$stats_lay}
  s_try <- try(arg$stats_title)
  if(class(s_try) == "NULL"){stats_title  <- ""}else{stats_title <- arg$stats_title}
  s_try <- try(arg$raster_only)
  if(class(s_try) == "NULL"){raster_only  <- ""}else{raster_only <- arg$raster_only}
  
  if(raster_only != TRUE){ #data_ani not needed for animate_raster()
    if(missing(data_ani)){
      out("Argument 'data_ani' is missing! Please specify the input movement data.",type=3)
    }else{
      if(is.list(data_ani) != TRUE){
        if(is(data_ani,"Move")){data_ani <- list(data_ani)} #Convert single move object to single element list
        else{
          if(is(data_ani,"MoveStack")){
            data_ani_temp <- list("empty")
            for(i in 1:length(split(data_ani))){
              data_ani_temp[[i]] <- split(data_ani)[[i]] 
            }
            data_ani <- data_ani_temp
          }else{out("Argument 'data_ani' needs to be a list object containing a 'move' object for each individual!",type=3)
          }
        }
      }else{
        for(i in 1:length(data_ani)){
          if(is(data_ani[[i]],"Move") == FALSE){out("Elements in list object 'data_ani' need to be of the 'move' class!",type=3)}
        }
      }
    }
  }
  if(missing(out_dir)){
    out("Argument 'out_dir' is missing! Please specify the output directory!", type=3)
  }else{
    if(is.character(out_dir) != TRUE){
      out("Argument 'out_dir' needs to be a character object!",type=3)
    }else{
      temp_dir <- paste0(tempdir(),"/moveVis")
      quiet(dir.create(temp_dir))
      setwd(temp_dir)
    }
  }
  if(is.character(conv_dir) == FALSE){
    out("Argument 'conv_dir' needs to be a character object!",type=3)
  }
  if(is.numeric(frames_nres) == FALSE){
    out("Keyword 'frames_nres' needs to be numeric! Setting 'frames_nres' to 1.",type=2)
    frames_nres <- 1
  }
  if(is.character(paths_col) == FALSE){out("Keword 'paths_col' needs to be character or character vector.",type=3)}
  if(is.character(layer) == FALSE){
    if(is.character(layer_dt) == TRUE){out("Argument 'layer_dt' needs to be specified if using 'layer'!
                                  Please provide time data with same length of 'layer'.",type=3)}
    if(length(layer) != length(layer_dt)){
      out("Arguments 'layer' and 'layer_dt' need to be lists of equal lengths.",type=3)
    }
    if(unlist(strsplit(as.character(crs(data_ani[[1]]))," "))[1] != unlist(strsplit(as.character(crs(layer[[1]]))," "))[1]
       & unlist(strsplit(as.character(crs(data_ani[[1]]))," "))[2] != unlist(strsplit(as.character(crs(layer[[1]]))," "))[2]){
      out("Arguments 'layer' and 'data_ani' have different projections! Please provide movement data and
          background layer data with equal projection or do not use 'layer'.",type=3)
    }
  }else{
    if(layer != "basemap"){out(paste0("Unknown input '",layer, "'. Argument 'layer' needs to be either a list of raster objects, a single raster object or a string containing 'basemap'."),type=3)}
  }
  if(stats_create == TRUE){
    if(layer[1] == "basemap"){
      out("Stats cannot be visualized for stadard Google Maps basemaps! Please use 'layer' to provide single layer basemap data!",type = 3)
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
  }
  if(stats_title == ""){stats_title <- c("Map values acummulated\nover all frames",
                                         paste0("Map values acummulated\nover a ",as.character(stats_tframe)," frames period"))}
  if(is.na(frames_width)){
    if(stats_create == TRUE & stats_only == FALSE){frames_width <- 1000}else{frames_width <- 600}
  }
  if(is.na(frames_height)){frames_height = 600}
  if(frames_width >= 1200 | frames_height >= 1200){out("High resolution ouptut causes time intensive frame creation!",type = 2)}
  if(stats_lay[1] == 0){
    if(stats_only == TRUE){stats_lay = rbind(c(1,2))}else{stats_lay = rbind(c(1,1,2),c(1,1,3))}
  }
  
  #Plattform dependences
  if(.Platform$OS.type == 'windows'){cmd.fun <- shell}else{cmd.fun <- system}
  

  
  #[2] PREPRO DATA DEPENDING ON MODE
  out("Preprocessing data depending on selected mode...",type=1)
  
  #CRS
  crs_input_cr <- paste0(unlist(strsplit(as.character(crs(data_ani[[1]]))," "))[1], " ",
                      unlist(strsplit(as.character(crs(data_ani[[1]]))," "))[2])
  crs_input <- crs(data_ani[[1]])
  
  #Convert move class elements to dataframe
  data_ani_df <- list()
  for(i in 1:length(data_ani)){
    data_ani_df[[i]] <- data.frame(data_ani[[i]]@coords[,1])
    data_ani_df[[i]] <- cbind(data_ani_df[[i]],data_ani[[i]]@coords[,2],data_ani[[i]]@timestamps)
    colnames(data_ani_df[[i]]) <- c("x","y","dt")
  }

  #Save original subset lengths
  for(i in 1:length(data_ani_df)){
    if(i == 1){sl_orig <- length(data_ani_df[[i]]$dt)}
    else{sl_orig <- c(sl_orig,length(data_ani_df[[i]]$dt))}
  }
  
  if(length(data_ani) > 1){
    if(paths_mode == "simple"){
      out("Preparing data for paths_mode 'simple' ...",type=1)
      #Subset lengths
      for(i in 1:length(data_ani_df)){
        if(i == 1){subset_lengths <- length(data_ani_df[[i]]$x)}
        else{subset_lengths <- c(subset_lengths,length(data_ani_df[[i]]$x))}
      }
      
      #Missing lines adding
      for(i in 1:length(data_ani_df)){
        if(length(data_ani_df[[i]]$x) < max(subset_lengths)){
          diff_lengths <- max(subset_lengths)-length(data_ani_df[[i]]$x)
          add_row <- data_ani_df[[i]][1,]
          add_row$x <- NA; add_row$y <- NA; add_row$dt <- NA
          for(a in 1:diff_lengths){
            data_ani_df[[i]] <- rbind(data_ani_df[[i]],add_row)
          }
        }
      }
    }
    
    if(paths_mode == "true_time" | paths_mode == "true_data"){
      out("Preparing data for true mode...",type=1)
      #Determine sets with earliest and latest dt for start/stop of data_ani_df
      start_dt <- st_times(data_ani_df)[[1]]
      stop_dt <- st_times(data_ani_df)[[2]]
      
      start_earliest_set <- which(start_dt == min(start_dt))
      start_latest_set <- which(start_dt == max(start_dt))
      stop_earliest_set <- which(stop_dt == min(stop_dt))
      stop_latest_set <- which(stop_dt == max(stop_dt))
      
      #Creating template data frame from earliest start to latest stop time
      time_range <- align.time(
        seq(min(start_dt)-60,max(stop_dt)-60,
            length.out = as.integer(difftime(max(stop_dt),min(start_dt),units = "mins"))+1),n=60)
      template <- data.frame(time_range)
      template <- cbind(template, x=vector(mode="double", length=length(time_range)),
                        y=vector(mode="double", length=length(time_range)))
      colnames(template) <- c("dt","x","y")
      
      #Filling template
      for(i in 1:length(data_ani_df)){
        if(i == 1){
          data_ani_df_true <- list(template)
        }else{
          data_ani_df_true[i] <- list(template)
        }
        match <- match(data_ani_df_true[[i]]$dt,data_ani_df[[i]]$dt)
        match <- which(is.na(match) == FALSE)
        data_ani_df_true[[i]]$x[match] <- data_ani_df[[i]]$x
        data_ani_df_true[[i]]$x[which(data_ani_df_true[[i]]$x == 0)] <- NA
        data_ani_df_true[[i]]$y[match] <- data_ani_df[[i]]$y
        data_ani_df_true[[i]]$y[which(data_ani_df_true[[i]]$y == 0)] <- NA
        
        for(j in 2:length(match)){
          if(match[j]-1 != match[j-1]){
            data_ani_df_true[[i]]$x[(match[j-1]+2):match[j]-1] <- data_ani_df_true[[i]]$x[match[j-1]]
            data_ani_df_true[[i]]$y[(match[j-1]+2):match[j]-1] <- data_ani_df_true[[i]]$y[match[j-1]]
          }
        }
      }
      
      if(paths_mode != "true_data"){
        data_ani_df <- data_ani_df_true
      }else{
        #Remove lines with no movement
        for(j in 1:length(data_ani_df_true)){
          if(j == 1){data_dupl <- data_ani_df_true[[j]][2:3]}
          else{data_dupl <- cbind(data_dupl,data_ani_df_true[[j]][2:3])}
        }
        for(j in 1:length(data_ani_df_true)){
          data_ani_df[[j]] <- data_ani_df_true[[j]][-which(duplicated(data_dupl)),]
        }
      }
  
      #Subset lengths
      for(i in 1:length(data_ani_df)){
        if(i == 1){subset_lengths <- length(data_ani_df[[i]]$x)}
        else{subset_lengths <- c(subset_lengths,length(data_ani_df[[i]]$x))}
      }
    }
  }else{
    #Subset lengths
    for(i in 1:length(data_ani_df)){
      if(i == 1){subset_lengths <- length(data_ani_df[[i]]$x)}
      else{subset_lengths <- c(subset_lengths,length(data_ani_df[[i]]$x))}
    }
  }
  
  #Subset, if specified
  if(frames_nmax != 0){
    out("Shortening movement data...")
    for(j in 1:length(data_ani_df)){
      if(length(data_ani_df[[j]][,1]) < frames_nmax){frames_nmax <- length(data_ani_df[[j]][,1])}
      data_ani_df[[j]] <- data_ani_df[[j]][1:frames_nmax,]
    }
  }
  
  
  #Remove individuals out of time range (NA)
  i = 1
  while(i <= length(data_ani_df)){
    if(length(unique(data_ani_df[[i]]$x)) == 1){
      if(is.na(unique(data_ani_df[[i]]$x))){
        data_ani_df <- data_ani_df[-i]
      }else{i = i + 1}
    }else{i = i + 1}
  }


  
  #[3] RECALCULATE RESOLUTION
   out("Recalculating dataset on specified output resolution...",type=1)

  #Calculate resolution
  n_lines <- length(data_ani_df[[1]]$x)
  if(frames_nres == 1){frames_nres <- max(subset_lengths)}
  if(n_lines > frames_nres){
    for(i in 1:length(data_ani_df)){
      if(i == 1){
        data_res <- list(data_ani_df[[i]][seq(1, length(data_ani_df[[i]]$x),frames_nres),])
      }else{
        data_res[i] <- list(data_ani_df[[i]][seq(1, length(data_ani_df[[i]]$x),frames_nres),])
      }
    }
    n_loop <- length(data_res[[1]]$x)
  }else{
    n_loop <- n_lines
    #Hier interpolation/smoothing fuer den Fall einbauen, dass gefragte Resolution groesser ist als input length!!!
    data_res <- data_ani_df
  }
  
  
  
  #[4] PREPARE RASTER BASE LAYER
  out("Calculating extent...",type=1)
  
  #Compute overall min and max y and x
  for(i in 1:length(data_res)){
    if(i == 1){
      min_y <- min(data_res[[i]]$y,na.rm = TRUE)
      min_x <- min(data_res[[i]]$x,na.rm = TRUE)
      max_y <- max(data_res[[i]]$y,na.rm = TRUE)
      max_x <- max(data_res[[i]]$x,na.rm = TRUE)
    }else{
      min_y <- c(min_y,min(data_res[[i]]$y,na.rm = TRUE))
      min_x <- c(min_x,min(data_res[[i]]$x,na.rm = TRUE))
      max_y <- c(max_y,max(data_res[[i]]$y,na.rm = TRUE))
      max_x <- c(max_x,max(data_res[[i]]$x,na.rm = TRUE))
    }
  }
  
  if(extent_factor != 0.0001){
    min_x <- min_x-(min_x*extent_factor)
    max_x <- max_x+(max_x*extent_factor)
    min_y <- min_y-(min_y*extent_factor)
    max_y <- max_y+(max_y*extent_factor)
  }
  
  #Write out extent data and y/x diffs
  vr_ext <- data.frame(min(min_y)); vr_ext <- cbind(vr_ext,min(min_x),max(max_y),max(max_x))
  colnames(vr_ext) <-  c("y_min","x_min","y_max","x_max")
  
  if(crs_input_cr != "+proj=longlat +ellps=WGS84"){
    #Transform complete extent
    vr_ext_tans <- data.frame(vr_ext[,2]); vr_ext_tans <- cbind(vr_ext_tans,vr_ext[,1])  
    vr_ext_tans <- rbind(vr_ext_tans,c(vr_ext[,4],vr_ext[,3]))
    colnames(vr_ext_tans) <- c("x","y")
    vr_ext_tans <- SpatialPointsDataFrame(coords = vr_ext_tans[,1:2], data = vr_ext_tans,
                                            proj4string = crs_input)
    vr_ext_tans <- spTransform(vr_ext_tans, CRSobj = crs("+proj=longlat +ellps=WGS84"))
    vr_ext_ll <- data.frame(extent(vr_ext_tans)@ymin); vr_ext_ll <- cbind(vr_ext_ll,extent(vr_ext_tans)@xmin,extent(vr_ext_tans)@ymax,extent(vr_ext_tans)@xmax)
    colnames(vr_ext_ll) <- c("y_min","x_min","y_max","x_max")
  }else{vr_ext_ll <- vr_ext}
  
  y_diff_ll <- vr_ext_ll$y_max - vr_ext_ll$y_min
  x_diff_ll <- vr_ext_ll$x_max - vr_ext_ll$x_min
  
  #Write out corner points
  p1 <- data.frame(vr_ext_ll$x_min); p1 <- cbind(p1, vr_ext_ll$y_min)
  p2 <- data.frame(vr_ext_ll$x_min); p2 <- cbind(p2, vr_ext_ll$y_max)
  p3 <- data.frame(vr_ext_ll$x_max); p3 <- cbind(p3, vr_ext_ll$y_min)
  
  #Calculate true axes distances & square extent
  dist_y_ll <- distGeo(p1,p2)
  dist_x_ll <- distGeo(p1,p3)
  
  map_ext_ll <- vr_ext_ll
  if(dist_x_ll < dist_y_ll){
    map_ext_ll$x_min <- vr_ext_ll$x_min-(((x_diff_ll/dist_x_ll)*dist_y_ll)-x_diff_ll)/2
    map_ext_ll$x_max <-  vr_ext_ll$x_max+(((x_diff_ll/dist_x_ll)*dist_y_ll)-x_diff_ll)/2
  }else{
    map_ext_ll$y_min <- vr_ext_ll$y_min-(((y_diff_ll/dist_y_ll)*dist_x_ll)-y_diff_ll)/2
    map_ext_ll$y_max <-  vr_ext_ll$y_max+(((y_diff_ll/dist_y_ll)*dist_x_ll)-y_diff_ll)/2
  }
  
  if(crs_input_cr != "+proj=longlat +ellps=WGS84"){
    #Transform complete extent
    map_ext_ll_trans <- data.frame(map_ext_ll[,2]); map_ext_ll_trans <- cbind(map_ext_ll_trans,map_ext_ll[,1])  
    map_ext_ll_trans <- rbind(map_ext_ll_trans,c(map_ext_ll[,4],map_ext_ll[,3]))
    colnames(map_ext_ll_trans) <- c("x","y")
    map_ext_ll_trans <- (spTransform(SpatialPointsDataFrame(coords = map_ext_ll_trans[,1:2], data = map_ext_ll_trans,proj4string = crs("+proj=longlat +ellps=WGS84")),
                 CRSobj = crs_input))
    map_ext <- data.frame(extent(map_ext_ll_trans)@ymin); map_ext <- cbind(map_ext,extent(map_ext_ll_trans)@xmin,extent(map_ext_ll_trans)@ymax,extent(map_ext_ll_trans)@xmax)
    colnames(map_ext) <- c("y_min","x_min","y_max","x_max")
  }else{map_ext <- map_ext_ll}
  
  #Define raster base layer rbl
  if(is.character(layer) == TRUE){
    out("Creating static base layer...")
    s_try <- try(rbl <- gmap(x = extent(map_ext_ll$x_min,map_ext_ll$x_max,map_ext_ll$y_min,map_ext_ll$y_max), exp=1, scale =1,type = map_type,lonlat = TRUE, rgb = TRUE,size=c(500,500))) #raster base layer
    if(class(s_try) == "try-error"){out("Connecting to Google failed. Please check your internet connection.",type=3)}
    if(crs_input_cr != "+proj=longlat +ellps=WGS84"){rbl <- projectRaster(from = rbl, crs = crs_input)}
    
    rbl_df <- data.frame(rasterToPoints(rbl))
    rbl_rgb <- rgb(rbl_df$red,rbl_df$green,rbl_df$blue, maxColorValue = 255)
    
    #Recalculate map_extent due to pixel-size related extent differences
    map_ext$y_min <- rbl@extent@ymin
    map_ext$x_min <- rbl@extent@xmin
    map_ext$y_max <- rbl@extent@ymax
    map_ext$x_max <- rbl@extent@xmax
  }
  
  if(is.character(layer) == FALSE){
    out("Creating dynamic base layer...")
    
    #Crop layer data to square extent
    vr_mask <- raster(xmn=map_ext$x_min,xmx=map_ext$x_max,ymn=map_ext$y_min,ymx=map_ext$y_max,
                      resolution = res(layer[[1]]))
    for(i in 1:length(layer)){
      if(i == 1){layer_crop <- list(crop(layer[[i]], vr_mask))
      }else{layer_crop[[i]] <- crop(layer[[i]], vr_mask)}
    }
    
    #Either interpolate per track time frame or just assign to time frames
    if(layer_int == FALSE){
      #Find nearest dated layer for each time frame
      dr_dt <- data.frame(as.numeric(data_res[[which(sl_orig == max(sl_orig,na.rm = TRUE))[1]]]$dt))
      ly_dt <- data.frame(as.numeric(layer_dt))
      rbl <- unlist(apply(dr_dt,ly_dt, MARGIN = 1, FUN = function(dr_dt,ly_dt){
        layer_crop[which(abs(ly_dt-dr_dt) == min(abs(ly_dt-dr_dt)))]
      }))
      
    }else{
      #Convert raster to vector for faster processing
      for(i in 1:length(layer_crop)){
        if(i == 1){r_vec <- list(data.frame(getValues(layer_crop[[i]])))
        }else{r_vec[[i]] <- data.frame(getValues(layer_crop[[i]]))}
      }
      
      #Calculate output time sequence
      length_out <- length(data_res[[1]][,1])
      sim_start <- min(st_times(data_res)[[1]])
      sim_end <- max(st_times(data_res)[[2]], na.rm = TRUE)
      
      t <- seq(sim_start, sim_end, length.out = length_out)
      
      #Create data.frame with each row representing one pixel and each col representing one time step
      pix_df <- data.frame(r_vec[[1]])
      for(i in 2:length(r_vec)){pix_df <- cbind(pix_df,r_vec[[i]])}
      
      #Write out non-NA rows with at least 1 true number
      for(i in 1:length(pix_df)){
        if(i == 1){sub_nona <- which(is.na(pix_df[i]) == FALSE)}
        else{sub_nona <- c(sub_nona,which(is.na(pix_df[i]) == FALSE))}
      }
      
      #Write out rows with at least two true numbers
      for(i in 1:length(pix_df)){
        if(i == 1){pix_nona <- data.frame(pix_df[i][as.numeric(names(which(table(sub_nona) >= 2))),])}
        else{pix_nona <- cbind(pix_nona,data.frame(pix_df[i][as.numeric(names(which(table(sub_nona) >= 2))),]))}
      }
      colnames(pix_nona) <- names(pix_df)
      
      #Interpolate per row to t elements
      out("Interpolating dynamic background layer...",type=1)
      pix_int <- t(apply(pix_nona, layer_dt, MARGIN = 1, FUN = function(pix_nona,layer_dt){
        f <- approxfun(layer_dt,pix_nona,rule = 2)
        t(matrix(f(t)))
      }))
      
      #Calculuate positions of true and NA numbers
      true_vals_pos <- as.numeric(names(which(table(sub_nona) >= 2)))
      all_pos <- which(is.na(pix_df[1]) | is.numeric(pix_df[[1]]))
      na_vals_pos <- which(!(all_pos %in% true_vals_pos))
      
      #Create templates
      r_vec_temp <- r_vec[[1]]
      r_vec_temp[] <- NA
      r_temp <- layer_crop[[1]]
      
      #Create a vector list for filling back to raster prep
      out("Creating dynamic background layer...",type=1)
      for(i in 1:length(t)){
        if(i == 1){
          r_vec_int <- list(r_vec_temp)
          r_vec_int[[i]][true_vals_pos,] <- pix_int[,i]
        }else{
          r_vec_int[[i]] <- r_vec_temp
          r_vec_int[[i]][true_vals_pos,] <- pix_int[,i]
        }
      }
      
      #Create raster objects list
      for(i in 1:length(r_vec_int)){
        if(i == 1){
          rbl <- list(setValues(r_temp,r_vec_int[[i]][,1]))
        }else{
          rbl[[i]] <- setValues(r_temp,r_vec_int[[i]][,1])
        }
      }
    }
    #Recalculate map_extent due to pixel-size related extent differences
    map_ext$y_min <- rbl[[1]]@extent@ymin
    map_ext$x_min <- rbl[[1]]@extent@xmin
    map_ext$y_max <- rbl[[1]]@extent@ymax
    map_ext$x_max <- rbl[[1]]@extent@xmax
    
    if(layer_type == "RGB"){
      for(i in 1:length(rbl)){
        if(i == 1){
          rbl_df <- list(data.frame(rasterToPoints(rbl[[i]])))
          rbl_rgb <- list(rgb(rbl_df[[i]]$red,rbl_df[[i]]$green,rbl_df[[i]]$blue, maxColorValue = 255))
        }else{
          rbl_df[[i]] <- data.frame(rasterToPoints(rbl[[i]]))
          rbl_rgb[[i]] <- rgb(rbl_df[[i]]$red,rbl_df[[i]]$green,rbl_df[[i]]$blue, maxColorValue = 255)
        }
      }
    }
    
    if(layer_type == "discrete"){
      for(i in 1:length(rbl)){
        if(i == 1){
          rbl_vals <- getValues(rbl[[i]])
          rbl_df <- list(data.frame(rasterToPoints(rbl[[i]])))
          colnames(rbl_df[[i]]) <- c("x","y","value")
        }else{
          rbl_vals <- c(rbl_vals, getValues(rbl[[i]]))
          rbl_df[[i]] <- data.frame(rasterToPoints(rbl[[i]]))
          colnames(rbl_df[[i]]) <- c("x","y","value")
        }
      }
      legend_breaks <- pretty(round(rbl_vals))
      if(legend_labels[1] == "auto"){legend_labels <- as.character(legend_breaks)
      }else{
        if(length(legend_labels) != length(legend_breaks)){
          legend_labels <- as.character(legend_breaks)
          out("Number of values to be displayed and length of 'legend_labels' are different. Legend labels are ignored therfore.",type=2)
        }
      }
    }
  }
  
  #Calcualte min/max basmap values
  if(is.character(layer) == FALSE){
    if(is.na(legend_limits[1])){
      val_x_limits <- data.frame(min(sapply(rbl, FUN = function(rbl){rbl@data@min})),
                                 max(sapply(rbl, FUN = function(rbl){rbl@data@max})))
      colnames(val_x_limits) <- c("x_min","x_max")
    }else{
      val_x_limits <- data.frame(legend_limits[1],legend_limits[2])
      colnames(val_x_limits) <- c("x_min","x_max")
    }
    plt_limits <- "limits=c(val_x_limits$x_min, val_x_limits$x_max)"
  }
  
  
  
  #[5] CALCULATE COLOUR RAMP AND SIZE VECTOR
  out("Calculating colour ramp and size vector...",type=1)
  
  #Calculate path colour ramp
  if(paths_col == "auto"){collist <- c("red","green","blue","yellow","darkgreen","orange","deepskyblue", "darkorange","deeppink","navy")
  }else{collist <- paths_col}
  n_tail <- tail_elements-1
  
  if(length(data_res) > length(collist)){
    col_recycle <- as.integer(length(data_res)/length(collist))
    for(i in 1:col_recycle){
      collist <- c(collist,collist)
    }
  }
  
  for(i in 1:length(data_res)){
    colfunc <- colorRampPalette(c(collist[i],"white"))
    if(i == 1){
      colours <- rbind(rev(colfunc(tail_elements+4)[1:tail_elements]))
    }else{
      colours <- rbind(colours,rev(colfunc(tail_elements+4)[1:tail_elements]))
    }
  }
  
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
      layer_col <- rev(colfunc(length(legend_breaks)))
    }
  }
  

  
  #[6] CREATE STATS OBJECT
  out("Computing stats...")
  
  if(stats_create == TRUE | layer[1] != "basemap"){
  
    #Calc limits
    if(stats_digits > 0){
      stats_floor_mp <- stats_digits*10
      stats_limits <- round(val_x_limits*stats_floor_mp)/10
    }else{
      stats_floor_mp = 1
      stats_limits <- round(val_x_limits)
    }
    
    #Extract values
    stats_pixvals <- list()
    for(i in 1:length(data_res)){
      for(j in 1:length(rbl)){
        if(j == 1){
          stats_pixvals[[i]] <- round(extract(rbl[[j]],matrix(c(data_res[[i]]$x,data_res[[i]]$y),ncol = 2))[j],digits = stats_digits)
        }else{
          stats_pixvals[[i]] <- c(stats_pixvals[[i]], round(extract(rbl[[j]],matrix(c(data_res[[i]]$x,data_res[[i]]$y),ncol = 2))[j], digits = stats_digits))
        }
      }
    }
    stats_pixvals <- lapply(stats_pixvals,FUN = function(stats_pixvals){
      data.frame(stats_pixvals)
    })
    
    #Derive frequence for each value
    stats_pixvals_freq <- lapply(stats_pixvals,FUN = function(stats_pixvals){
      data.frame(table(stats_pixvals))
    })
    
    #Acummulate values
    for(j in 1:length(stats_pixvals)){
      stats_pixvals[[j]] <- cbind(stats_pixvals[[j]],NA)
      colnames(stats_pixvals[[j]]) <- c("x","y")
      if(length(stats_pixvals_freq[[j]][,1]) != 0){
        for(i in 1:length(stats_pixvals_freq[[j]][,1])){
          stats_pixvals[[j]][,2][which(stats_pixvals[[j]][,1] == stats_pixvals_freq[[j]][,1][i])] <- seq(from = 1, to = stats_pixvals_freq[[j]]$Freq[i], by = 1)
        }
      }
      if(j==1){stats_indi <- 
	      rownames(idData(data_ani[[j]]))
      }else{stats_indi <- c(stats_indi,rownames(idData(data_ani[[j]])) )}
    }
    
    
    #{a} Create df per time frame with ALL values (acummulation)
    stats_df <- list()
    for(i in 1:length(rbl)){
      if(i == 1){
        stats_df[[i]] <- data.frame(seq(stats_limits$x_min,stats_limits$x_max,by=stats_floor_mp))
        stats_df[[i]] <- cbind(stats_df[[i]],data.frame(matrix(0,ncol = length(stats_pixvals),nrow = length(stats_df[[i]][,1]))))
        colnames(stats_df[[i]]) <- c("val",stats_indi)
      }else{
        stats_df[[i]] <- stats_df[[i-1]]
      }
      
      for(j in 1:length(stats_pixvals)){
        if(is.na(stats_pixvals[[j]][i,]$x) == FALSE){
          stats_df[[i]][which(as.character(stats_df[[i]]$val) == as.character(stats_pixvals[[j]][i,]$x)),][j+1] <- stats_pixvals[[j]][i,]$y
        }
      }
    }
  
    for(i in 1:length(rbl)){
      stats_df[[i]] <- cbind(stats_df[[i]],rowSums(stats_df[[i]][2:(length(stats_pixvals)+1)]))
      colnames(stats_df[[i]])[length(colnames(stats_df[[i]]))] <- "sum"
    }
    
    
    #{b} Create df per time frame period (live)
    stats_df_tframe <- stats_df
    for(i in 1:length(stats_df)){
      if(i > stats_tframe){
        stats_df_tframe[[i]][2:length(stats_df_tframe[[i]])] <- stats_df_tframe[[i]][2:length(stats_df_tframe[[i]])]-stats_df[[i-stats_tframe]][2:length(stats_df_tframe[[i]])]
      }
    }
    
    #convert data to long format
    pdat <- list()
    for(k in 1:2){
      pdat[[k]] <- list()
      if(k == 1){tomelt <- stats_df}else{tomelt <- stats_df_tframe}
      for(i in 1:length(rbl)){
        pdat[[k]][[i]] <- melt(tomelt[[i]], id.vars = "val")
        pdat[[k]][[i]] <- cbind(pdat[[k]][[i]],NA)
        colnames(pdat[[k]][[i]])[4] <- "cols"
        
        #prepare ggplot df
        for(j in 1:(length(data_res)+1)){
          if(j==1){
            pdat[[k]][[i]]$cols[which(pdat[[k]][[i]]$variable=="sum")] <- "black"
            cols<- "black"
            names <- "sum"
          }else{
            pdat[[k]][[i]]$cols[which(pdat[[k]][[i]]$variable==stats_indi[j-1])] <- colours[j-1]
            cols <- c(cols, as.character(colours[(j-1),tail_elements]))
            names <- c(names,stats_indi[j-1])
          }
        }
        cols <- setNames(cols,names)
      }
    }
    stats_max <- c((max(stats_df[[length(stats_df)]]$sum)+5),(max(stats_df_tframe[[length(stats_df_tframe)]]$sum)+5))
  
    
    #Add histogram data line
    for(i in 1:length(rbl)){
      h <-rbl[[i]]@data@values
      h <- round(h, digits = stats_digits)
      h <- data.frame(table(h))
      colnames(h) <- c("val","value")
      add_pdat <- cbind(stats_df[[i]],h[which(h$val == unique(stats_df[[1]]$val)),]$value)
      colnames(add_pdat)[length(colnames(add_pdat))] <- "hist"
      add_pdat <-  melt(add_pdat, id.vars = "val")
      pdat[[1]][[i]] <- cbind(add_pdat,c(pdat[[1]][[i]]$cols,rep("grey",length(unique(pdat[[1]][[i]]$val)))))
      colnames(pdat[[1]][[i]])[4] <- "cols"
    }
    cols <- list(cols); cols[[2]] <- cols[[1]]
    cols[[1]] <- c(cols[[1]],"grey");cols[[1]] <- setNames(cols[[1]],c(names,"hist"))
  }
  

  
  #[7] CALCULATE MAP ELEMENTS POSITIONS
  out("Calculating positions of map elements...",type=1)
  
  #Variables for scale bar and north arrow
  y_bm <- c(map_ext[,1],map_ext[,3])
  x_bm <- c(map_ext[,2],map_ext[,4])
  
  y_dist <- max(y_bm) - min(y_bm)
  y_dist_bar <- y_dist/80
  
  y_down <- min(y_bm)+((max(y_bm)-min(y_bm))/13)
  x_left <- min(x_bm)+((max(x_bm)-min(x_bm))/13)
  rec1_leftdown <- data.frame(x_left,y_down)
  colnames(rec1_leftdown) <- c("x","y")
  
  #Transform to latlon
  if(crs_input_cr != "+proj=longlat +ellps=WGS84"){
    #Transform complete extent
    map_ext_trans <- data.frame(map_ext[,2]); map_ext_trans <- cbind(map_ext_trans,map_ext[,1])  
    map_ext_trans <- rbind(map_ext_trans,c(map_ext[,4],map_ext[,3]))
    colnames(map_ext_trans) <- c("x","y")
    map_ext_trans <- SpatialPointsDataFrame(coords = map_ext_trans[,1:2], data = map_ext_trans,
                                            proj4string = crs_input)
    map_ext_trans <- spTransform(map_ext_trans, CRSobj = crs("+proj=longlat +ellps=WGS84"))
    map_ext_ll <- data.frame(extent(map_ext_trans)@ymin); map_ext_ll <- cbind(map_ext_ll,extent(map_ext_trans)@xmin,extent(map_ext_trans)@ymax,extent(map_ext_trans)@xmax)
    colnames(map_ext_ll) <- c("y_min","x_min","y_max","x_max")
    
    #Transform scale bar leftdown coordinate
    rec1_leftdown_trans <- SpatialPointsDataFrame(coords = rec1_leftdown[,1:2], data = rec1_leftdown,
                                                  proj4string = crs_input)
    rec1_leftdown_trans <- spTransform(rec1_leftdown_trans, CRSobj = crs("+proj=longlat +ellps=WGS84"))
    rec1_leftdown_ll <- data.frame(extent(rec1_leftdown_trans)@xmin); rec1_leftdown_ll <- cbind(rec1_leftdown_ll,extent(rec1_leftdown_trans)@ymin)
    colnames(rec1_leftdown_ll) <- c("x","y")
  }else{
    map_ext_ll <- map_ext
    rec1_leftdown_ll <- rec1_leftdown
  }
  
  x_dist <- (distGeo(c(map_ext_ll[,2],map_ext_ll[,1]),
                     c(map_ext_ll[,4],map_ext_ll[,1])))/1000 #in km
  x_dist_bar <- round((x_dist/8)*4)/4
  if(x_dist_bar == 0){x_dist_bar <- round(((x_dist/8)*4),1)/4}
  
  rec1_rightdown_ll <- data.frame(gcDestination(lon = rec1_leftdown_ll$x, lat = rec1_leftdown_ll$y, bearing = 90, dist = x_dist_bar, dist.units = "km", model = "WGS84"))
  
  if(crs_input_cr != "+proj=longlat +ellps=WGS84"){  
    x_right <- (spTransform(SpatialPointsDataFrame(coords = rec1_rightdown_ll[,1:2], data = rec1_rightdown_ll,proj4string = crs("+proj=longlat +ellps=WGS84")),
                            CRSobj = crs_input))$long
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
  
  x_arrow <- map_ext[,2]+((map_ext[,4]-map_ext[,2])/1.1)
  y_arrow_start <- min(y_bm)+((max(y_bm)-min(y_bm))/11)
  y_arrow_end <- map_ext[,1]+((map_ext[,3]-map_ext[,1])/8)
  arrow <- data.frame(x_arrow,y_arrow_start); arrow <- rbind(arrow,c(x_arrow,y_arrow_end))
  colnames(arrow) <- c("x","y")
  
  #Create ggplot object to get deviating plot extent
  if(is.character(layer) == FALSE){rbl_gg <- gplot(rbl[[1]]) + geom_tile(aes_(fill = ~value)) +
    scale_fill_gradientn(colours = layer_col, guide=guide_colourbar(title = legend_title, label.vjust = 0.9, title.hjust = 0, title.vjust = 0)) +
    scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) + theme(aspect.ratio=1)
  }else{rbl_gg <- ggplot(data=rbl_df,aes_(x=~x, y=~y)) + geom_tile(aes(fill = rbl_rgb)) + scale_fill_identity() +
    scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) + theme(aspect.ratio=1)}
  rbl_gg_build <- ggplot_build(rbl_gg)
  
  #Variables for progress bar
  prog_bar_length <- n_loop-1-n_tail
  prog_bar_elem_length <- (max(rbl_gg_build$data[[1]]$xmax)-min(rbl_gg_build$data[[1]]$xmin))/(prog_bar_length+1)
  for(i in 1:prog_bar_length){
    if(i == 1){prog_x_st <- min(rbl_gg_build$data[[1]]$xmin)}
    else{prog_x_st <- c(prog_x_st, prog_x_st[i-1]+prog_bar_elem_length)}
  }
  prog_x_end <- c(prog_x_st[2:prog_bar_length],max(rbl_gg_build$data[[1]]$xmax))
  prog_y <- max(rbl_gg_build$data[[1]]$ymax)
  
  
  
  #[8] PARSING PLOT FUNCTION
  out("Parsing plot function arguments..", type=1)
  
  #Define argument strings for the plot function to be called dynamically according to number of individuals
  plt_path_std <- 'geom_path(data = frame_l[[1]][,1:2],aes_(x = frame_l[[1]]$x, y = frame_l[[1]]$y,
    alpha = paths_alpha),lineend = "round", linejoin = "round", colour = colours[1,], size = line_size,na.rm=TRUE, show.legend = FALSE)'
  plt_path_c1 <- 'geom_path(data = frame_l[['
  plt_path_c2 <- ']][,1:2],aes_(x = frame_l[['
  plt_path_c3 <- ']]$x, y = frame_l[['
  plt_path_c4 <- ']]$y, alpha = paths_alpha),lineend = "round", linejoin = "round", colour = colours['
  plt_path_c5 <- ',], size = line_size,na.rm=TRUE,show.legend = FALSE)'
  
  #Assemble path plot functions
  for(i in 1:length(data_res)){
    if(i==1){plt_path <- plt_path_std
    }else{plt_path <- paste0(plt_path,"+",plt_path_c1,toString(i),plt_path_c2,toString(i),plt_path_c3,toString(i),plt_path_c4,toString(i),plt_path_c5)}
  }
  
  #Define labs
  if(img_labs != "labs"){labs_x <- img_labs[1]; labs_y <- img_labs[2]
  }else{
    if(crs_input_cr == "+proj=longlat +ellps=WGS84"){
      labs_x <- "Longitude"; labs_y <- "Latitude"
    }else{labs_x <- "x"; labs_y <- "y"}
  }
  
  #Define title, subtitle and caption
  if(img_title != "title"){
    plt_title <- paste0('labs(x = labs_x, y=labs_y, title="',img_title,'", label=c("123")) + theme(plot.title = element_text(hjust = 0.5))')
    if(img_sub != "subtitle"){
      
      #Seperating lines due to input length
      line_break <- 80
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
      plt_title <- paste0('labs(x = labs_x, y=labs_y, title="',img_title,'", subtitle="',img_sub,'", label=c("123","456"))+
                            theme(plot.title = element_text(hjust = 0.5),
                            plot.subtitle = element_text(hjust = 0.5))')
    }
    if(img_caption != "caption"){
      if(img_sub != "subtitle"){
        plt_title <- paste0('labs(x = labs_x, y=labs_y, title="',img_title,'", subtitle="',img_sub,'", caption="',img_caption,'",label=c("123","456","789"))+
                              theme(plot.title = element_text(hjust = 0.5),
                              plot.subtitle = element_text(hjust = 0.5),
                              plot.caption = element_text(hjust = 0.5))')
      }else{
        plt_title <- paste0('labs(x = labs_x, y=labs_y, title="',img_title,'", caption="',img_caption,'",label=c("123","456"))+
                              theme(plot.title = element_text(hjust = 0.5), 
                              plot.caption = element_text(hjust = 0.5))')
      }
    }
  }else{
    if(img_caption != "caption"){plt_title <- paste0('labs(x = labs_x, y=labs_y, caption="',img_caption,'",label=c("123"))+
                                                           theme(plot.caption = element_text(hjust = 0.5))')
    }else{plt_title <- 0}
  }
  
  #Defining map elements
  if(map_elements == TRUE){
    plt_scale_north <- 'geom_polygon(data = rec1, aes_(x = ~x, y = ~y), fill = "white", colour = "black") +
      geom_polygon(data = rec2, aes_(x = ~x, y = ~y), fill = "black", colour = "black") +
      annotate("text", label = paste(leg_text, " km", sep=""), x = leg_coords$x, y = leg_coords$y, size = 3, colour = scalebar_col) +
      geom_line(arrow=arrow(length=unit(3.7,"mm")),data = arrow, aes_(x=~x, y=~y), colour=north_col,size=1.06) +
      annotate(x=x_arrow, y=y_down, label="N", colour=north_col, geom="text", size=6.5)+'
  }else{plt_scale_north <- ''}
  plt_progress <- 'geom_line(data = prog_bar, aes_(x=~x,y=~y),colour="grey",size=1.8)+'
  
  
  #Parse argument string for plotting in the saveGIF function
  if(is.character(layer) == TRUE){
    plt_fin <- paste0("rbl_gg + ",
                      plt_scale_north,plt_progress,plt_path)
  }else{
    #if(is.na(legend_limits)){plt_limits <- "limits=c(rbl[[i]]@data@min, rbl[[i]]@data@max)"
    #}else{plt_limits <- "limits=legend_limits"}
    if(layer_type == "gradient"){
      plt_fin <- paste0('gplot(rbl[[i]]) + geom_tile(aes_(fill = ~value)) +
                          scale_fill_gradientn(colours = layer_col, ',plt_limits,', guide=guide_colourbar(title = legend_title, label.vjust = 0.9, title.hjust = 0, title.vjust = 0)) +
                          scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) + theme(aspect.ratio=1) +',
                        plt_scale_north,plt_progress,plt_path)
    }
    if(layer_type == "discrete"){
      plt_fin <- paste0('ggplot(data=rbl_df[[i]], aes_(x=~x, y=~y)) + geom_tile(aes(fill = factor(value))) +
                          scale_fill_manual(values = c(setNames(layer_col, 1:length(layer_col))), labels = legend_labels, drop = FALSE, na.value = layer_nacol, guide = guide_legend(title = legend_title, label = TRUE, label.vjust = 0.9, title.hjust = 0, title.vjust =0)) +
                          scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) + theme(aspect.ratio=1) +',
                        plt_scale_north,plt_progress,plt_path)
    }
    if(layer_type == "RGB"){
      plt_fin <- paste0('ggplot(data=rbl_df[[i]],aes_(x=~x, y=~y)) + geom_tile(aes_(fill = ~rbl_rgb[[i]])) + scale_fill_identity() +
                          scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) + theme(aspect.ratio=1) +',
                        plt_scale_north,plt_progress,plt_path)
    }
  }
  
  
  #Add title?
  if(plt_title != 0){plt_fin <- paste0(plt_fin,"+", plt_title)
  }else{plt_fin <- paste0(plt_fin)}
  
  plt_parse <- parse(text=plt_fin)
    
  #stats plot function
  if(stats_gg != ""){
    plt_stats_parse = parse(text = stats_gg)
  }else{
    if(stats_type == "line"){
      plt_stats_parse = parse(text = 'ggplot(data = pdat[[k]][[i]], aes_(x = ~val, y = ~value, colour = ~variable)) + 
      geom_line() + geom_point() + theme_bw() + theme(aspect.ratio=1) +
      scale_y_continuous(expand = c(0,0),limits = c(0,stats_max[k])) +
      scale_x_continuous(expand = c(0,0)) + 
      scale_color_manual(name="",values = cols[[k]]) +
      labs(x = "Basemap Value", y="Frequency", title=stats_title[[k]], label=c("123","456"))+
      theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))')
    }
    if(stats_type == "bar"){
      plt_stats_parse = parse(text = 'ggplot(data = pdat[[k]][[i]], aes(x = val, y = value, fill = variable)) + 
      scale_fill_manual(name="",values = cols[[k]]) +
      geom_bar(stat = "identity", position = "dodge") + theme_bw() + theme(aspect.ratio=1) +
      scale_y_continuous(expand = c(0,0),limits = c(0,stats_max[k])) +
      scale_x_continuous(expand = c(0,0)) +
      labs(x = "Basemap Value", y="Frequency", title=stats_title[[k]], label=c("123","456"))+
      theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))')
    }
  }
  
  
  
  #[8] CALCULATING ANIMATION
  out("Calculating animation...", type=1)
  
  #Decide, if to use one or several gifs for gif assembling
  max_frames <- 3000 #maximum frames which convert.exe can use
  n_reloop <- ceiling(n_loop/max_frames) #number of repeating gif building
  
  #Calculate gif frame maxima for each gif
  for(r in 1:n_reloop){
    if(r == 1){index_list <- floor(n_loop/n_reloop)}
    else{
      if(r != n_reloop){index_list <- c(index_list,(floor(n_loop/n_reloop)+index_list[r-1]))}
      else{index_list <- c(index_list,n_loop)}
    }
  }
  
  #Check, if stats should be visulized only
  if(stats_only == TRUE){
    for(r in 1:n_reloop){
      
      #gif frame loop start and stop
      if(r == 1){index_min <- 1}else{index_min <- index_list[r-1]+1}
      index_max <- index_list[r]
      
      #Define ani.options
      quiet(ani.options(interval=frames_interval, nmax=index_max, ani.width=frames_width,ani.height=frames_height, convert = conv_dir))
      
      createGIF(
        for(i in index_min:index_max){
          if((i+n_tail) <= index_max){
            k <- 2; p1 <- eval(plt_stats_parse)
            k <- 1; p2 <- eval(plt_stats_parse)
            quiet(grid.arrange(p1,p2,layout_matrix = stats_lay))
          }
        },movie.name = paste0("out_gif",toString(r),".gif"),img.name = "p",out_dir = temp_dir,img.dir = temp_dir
      )
    }
  }else{
    
    #Create gifs
    for(r in 1:n_reloop){
      
      #gif frame loop start and stop
      if(r == 1){index_min <- 1}else{index_min <- index_list[r-1]+1}
      index_max <- index_list[r]
      
      #Define ani.options
      quiet(ani.options(interval=frames_interval, nmax=index_max, ani.width=frames_width,ani.height=frames_height, convert = conv_dir))
      
      #Calcualte animation
      #s_try <- try(
      createGIF(
        for(i in index_min:index_max){
          if((i+n_tail) <= index_max){
            
            #Create frame and plot it on basemap
            for(a in 1:length(data_res)){
              if(a == 1){
                frame_l <- list(data_res[[a]][i:(i+n_tail),] )
              }else{
                frame_l[a] <- list(data_res[[a]][i:(i+n_tail),] )
              }
            }
            
            #Calculate progress bar end
            prog_bar <- data.frame(prog_x_st[1],prog_y); prog_bar <- rbind(prog_bar,c(prog_x_end[i],prog_y))
            colnames(prog_bar) <- c("x","y")
            
            #Execute parsed plotting function
            if(stats_create == FALSE){
              quiet(plot(eval(plt_parse)))
            }else{
              #plot with par side by side
              p1 <- eval(plt_parse)
              k <- 2; p2 <- eval(plt_stats_parse)
              k <- 1; p3 <- eval(plt_stats_parse)
              quiet(grid.arrange(p1,p2,p3,layout_matrix = stats_lay))
            }
          }
        },movie.name = paste0("out_gif",toString(r),".gif"),img.name = "p",out_dir = temp_dir,img.dir = temp_dir
      )
      #)
      #if(class(s_try) == "try-error"){
      #  out("Creation of animation failed.",type=3)
      #}
    }
  }
  
  #Compress and fusion gifs
  if(n_reloop == 1){
    file.copy("out_gif1.gif",paste0(out_name,".gif"), overwrite = TRUE)
    file.copy(paste0(out_name,".gif"),paste0(out_dir,"/",out_name,".gif"), overwrite = TRUE)
    file.remove("out_gif1.gif")
    file.remove(paste0(out_name,".gif"))
  }else{
    for(r in 1:n_reloop){
      if(r == 1){cmd_fusion <- paste0('"',conv_dir,'" -loop 0 -delay ',toString(frames_interval*100),' out_gif',toString(r),'.gif')}
      else{cmd_fusion <- paste0(cmd_fusion,' out_gif',toString(r),'.gif')}
    }
    cmd_fusion <- paste0(cmd_fusion, ' "',out_dir,'/',out_name,'.gif"')
    cmd.fun(cmd_fusion)
    file.remove(list.files(temp_dir)[grep("out_gif",list.files(temp_dir))])
  }
  out(paste0("Finished. Your animated GIF was saved to '",out_dir,"'."), type=1)
  if(log_logical == TRUE){return(TRUE)}
}
