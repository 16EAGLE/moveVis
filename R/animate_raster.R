#' Animate raster data
#'
#' \code{animate_raster} animates raster data provided as list of\code{raster} class objects. The function creates an animated GIF file and saves it into the output directory.
#'
#' @param layer raster, list or character. Single raster object or list of raster objects to be used as (dynamically changing) basemap layer. Default is \code{"basemap"} to download a static basemap layer. Use a rasterBrick class object and set layer_type to "\code{RGB}" to compute a RGB basemap.
#' @param layer_type charachter. Layer type. Can be either "\code{RGB}" (if layer is a rasterBrick class onejct), "\code{gradient}" or "\code{discrete}". Default is "\code{RGB}". Ignored, if \code{layer = "basemap"}.
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
#' @param out_name character. Name of the output file. Default is "final_gif".
#' @param log_level numeric. Level of console output given by the function. There are three log levels. If set to 3, no messages will be displayed except erros that caused an abortion of the process. If set to 2, warnings and errors will be displayed. If set to 1, a log showing the process activity, wanrnings ans errors will be displayed.
#' 
#' @return No return. The GIF file is written to the ouput directory.
#' 
#' @details \code{animate_raster} is partly based on the \code{animation} package and needs the \code{convert} tool of the \code{ImageMagick} software package to assemble the GIF file. The command or directory to the convert tool needs to be provided with \code{conv_dir}. Please use \code{\link{get_imconvert}} to search for the convert command/tool directory on your system or to automatically download and install the required software. See \code{\link{get_imconvert}} for details.
#' 
#' @author Jakob Schwalb-Willmann
#' @seealso \code{\link{get_imconvert}}
#' 
#' @import ggplot2
#' @importFrom animation ani.options
#' @importFrom raster crs extent projectRaster raster getValues setValues rasterToPoints res crop
#' @importFrom xts align.time
#' @importFrom sp SpatialPointsDataFrame spTransform
#' @importFrom geosphere distGeo
#' @importFrom dismo gmap
#' @importFrom maptools gcDestination
#' @importFrom rasterVis gplot
#' @importFrom grid arrow unit
#' @importFrom move move
#' @importFrom grDevices dev.off rgb colorRampPalette
#' @importFrom utils head
#' @importFrom methods is
#' @importFrom stats approxfun
#' @export

animate_raster <- function(layer, out_dir, conv_dir = "convert", layer_type = "", layer_col = c("sandybrown","white","darkgreen"), layer_nacol = "white", 
                         img_title = 'title', img_sub = 'subtitle', img_caption = "caption", img_labs = "labs", legend_title = "",
                         legend_limits = NA,legend_labels = "auto", map_elements = TRUE, scalebar_col = "white", north_col = "white",
                         frames_nmax =  0, frames_interval = .04, frames_nres = 1, out_name = "final_gif", log_level = 1){
  
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
  
  
  #[2] PREPARE RASTER LIST
  out("Preparing raster data...",type=1)
  
  rbl <- layer
  if(layer_type == "RGB"){
    for(i in 1:length(rbl)){
      if(i == 1){
        rbl_df <- list(data.frame(rasterToPoints(rbl[[i]])))
        rbl_rgb <- list(rgb(rbl_df[[i]][[1]],rbl_df[[i]][[2]],rbl_df[[i]][[3]], maxColorValue = 255))
      }else{
        rbl_df[[i]] <- data.frame(rasterToPoints(rbl[[i]]))
        rbl_rgb[[i]] <- rgb(rbl_df[[i]][[1]],rbl_df[[i]][[2]],rbl_df[[i]][[3]], maxColorValue = 255)
      }
    }
  }
  
  if(layer_type == "discrete"){
    for(i in 1:length(rbl)){
      if(i == 1){rbl_vals <- getValues(rbl[[i]])}else{rbl_vals <- c(rbl_vals, getValues(rbl[[i]]))}
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

  #Calculate layer colour ramp
  if(layer_type == "discrete"){
    if(length(layer_col) != length(legend_breaks)){
      colfunc <- colorRampPalette(layer_col)
      layer_col <- rev(colfunc(length(legend_breaks)))
    }
  }

  
  #[3] CALCULATE MAP ELEMENTS POSITIONS
  out("Calculating positions of map elements...",type=1)
  
  map_ext <- data.frame(extent(layer[[1]])@ymin); map_ext <- cbind(map_ext,extent(layer[[1]])@xmin,extent(layer[[1]])@ymax,extent(layer[[1]])@xmax)
  colnames(map_ext) <- c("y_min","x_min","y_max","x_max")
  
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
  if(as.character(crs(layer[[1]])) != "+proj=longlat +ellps=WGS84"){
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
  
  if(as.character(crs(layer[[1]])) != "+proj=longlat +ellps=WGS84"){  
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
  rbl_gg <- gplot(rbl[[1]]) + geom_tile(aes_(fill = ~value)) +
    scale_fill_gradientn(colours = layer_col, guide=guide_colourbar(title = legend_title, label.vjust = 0.9, title.hjust = 0, title.vjust = 0)) +
    scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) + theme(aspect.ratio=1)
  rbl_gg_build <- ggplot_build(rbl_gg)
  
  #Variables for progress bar
  n_loop <- length(layer)
  n_tail <- 0
  prog_bar_length <- n_loop-1-n_tail
  prog_bar_elem_length <- (max(rbl_gg_build$data[[1]]$xmax)-min(rbl_gg_build$data[[1]]$xmin))/(prog_bar_length+1)
  for(i in 1:prog_bar_length){
    if(i == 1){prog_x_st <- min(rbl_gg_build$data[[1]]$xmin)}
    else{prog_x_st <- c(prog_x_st, prog_x_st[i-1]+prog_bar_elem_length)}
  }
  prog_x_end <- c(prog_x_st[2:prog_bar_length],max(rbl_gg_build$data[[1]]$xmax))
  prog_y <- max(rbl_gg_build$data[[1]]$ymax)
  
  
  #[4] PARSING PLOT FUNCTION
  out("Parsing plot function arguments..", type=1)
  
  #Define labs
  if(img_labs != "labs"){labs_x <- img_labs[1]; labs_y <- img_labs[2]
  }else{
    if(as.character(crs(layer[[1]])) == "+proj=longlat +ellps=WGS84"){
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
  plt_scale_north <- 'geom_polygon(data = rec1, aes_(x = ~x, y = ~y), fill = "white", colour = "black") +
  geom_polygon(data = rec2, aes_(x = ~x, y = ~y), fill = "black", colour = "black") +
  annotate("text", label = paste(leg_text, " km", sep=""), x = leg_coords$x, y = leg_coords$y, size = 3, colour = scalebar_col) +
  geom_line(arrow=arrow(length=unit(3.7,"mm")),data = arrow, aes_(x=~x, y=~y), colour=north_col,size=1.06) +
  annotate(x=x_arrow, y=y_down, label="N", colour=north_col, geom="text", size=6.5)'
  plt_progress <- 'geom_line(data = prog_bar, aes_(x=~x,y=~y),colour="grey",size=1.8)'
  
  
  #Parse argument string for plotting in the saveGIF function
  if(is.na(legend_limits[1])){plt_limits <- "limits=c(rbl[[i]]@data@min, rbl[[i]]@data@max)"
  }else{plt_limits <- "limits=legend_limits"}
  if(map_elements == TRUE){
    if(layer_type == "gradient"){
      plt_fin <- paste0('quiet(plot(gplot(rbl[[i]]) + geom_tile(aes_(fill = ~value)) +
                          scale_fill_gradientn(colours = layer_col, ',plt_limits,', guide=guide_colourbar(title = legend_title, label.vjust = 0.9, title.hjust = 0, title.vjust = 0)) +
                          scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) + theme(aspect.ratio=1) +',
                        plt_scale_north,"+",plt_progress)
    }
    if(layer_type == "discrete"){
      plt_fin <- paste0('quiet(plot(gplot(rbl[[i]]) + geom_tile(aes(fill = factor(value))) +
                          scale_fill_manual(values = c(setNames(layer_col, 1:length(layer_col))), labels = legend_labels, drop = FALSE, na.value = layer_nacol, guide = guide_legend(title = legend_title, label = TRUE, label.vjust = 0.9, title.hjust = 0, title.vjust =0)) + 
                          scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) + theme(aspect.ratio=1) +',
                        plt_scale_north,"+",plt_progress)
    }
    if(layer_type == "RGB"){
      plt_fin <- paste0('quiet(plot(ggplot(data=rbl_df[[i]],aes_(x=~x, y=~y)) + geom_tile(aes_(fill = ~rbl_rgb[[i]])) + scale_fill_identity() +
                          scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) + theme(aspect.ratio=1) +',
                        plt_scale_north,"+",plt_progress)
    }
  }else{
    if(layer_type == "gradient"){
      plt_fin <- paste0('quiet(plot(gplot(rbl[[i]]) + geom_tile(aes_(fill = ~value)) +
                          scale_fill_gradientn(colours = layer_col, ',plt_limits,', guide=guide_colourbar(title = legend_title, label.vjust = 0.9, title.hjust = 0, title.vjust = 0)) +
                          scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) + theme(aspect.ratio=1) +',
                        "+",plt_progress)
    }
    if(layer_type == "discrete"){
      plt_fin <- paste0('quiet(plot(gplot(rbl[[i]]) + geom_tile(aes_(fill = factor(~value))) +
                          scale_fill_manual(values = c(setNames(layer_col, 1:length(layer_col))), labels = legend_labels, drop = FALSE, na.value = layer_nacol, guide = guide_legend(title = legend_title, label = TRUE, label.vjust = 0.9, title.hjust = 0, title.vjust =0)) + 
                          scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) + theme(aspect.ratio=1) +',
                        "+",plt_progress)
    }
    if(layer_type == "RGB"){
      plt_fin <- paste0('quiet(plot(ggplot(data=rbl_df[[i]],aes_(x=~x, y=~y)) + geom_tile(aes_(fill = ~rbl_rgb[[i]])) + scale_fill_identity() +
                          scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) + theme(aspect.ratio=1) +',
                        "+",plt_progress)
    }
  }
  
  #Add title?
  if(plt_title != 0){plt_fin <- paste0(plt_fin,"+", plt_title,"))")
  }else{plt_fin <- paste0(plt_fin,"))")}
  
  plt_parse <- parse(text=plt_fin)
  
  
  #[5] CALCULATING ANIMATION
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
  
  #Create gifs
  for(r in 1:n_reloop){
    
    #gif frame loop start and stop
    if(r == 1){index_min <- 1}else{index_min <- index_list[r-1]+1}
    index_max <- index_list[r]
    
    #Define ani.options
    quiet(ani.options(interval=frames_interval, nmax=index_max, ani.width=600,ani.height=600, convert = conv_dir))
    
    #Calcualte animation
    #s_try <- try(
    createGIF(
      for(i in index_min:index_max){
        if((i+n_tail) <= index_max){
          
          #Calculate progress bar end
          prog_bar <- data.frame(prog_x_st[1],prog_y); prog_bar <- rbind(prog_bar,c(prog_x_end[i],prog_y))
          colnames(prog_bar) <- c("x","y")
          
          #Execute parsed plotting function
          eval(plt_parse)
          
        }
      },movie.name = paste0("out_gif",toString(r),".gif"),img.name = "p",out_dir = temp_dir,img.dir = temp_dir
    )
    #)
    #if(class(s_try) == "try-error"){
    #  out("Creation of animation failed.",type=3)
    #}
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
  
  
  #[6] CLEAN UP
  out("Cleaning up environment...", type=1)
  out("Finished! Your animated GIF was saved in the specified output directory.", type=1)
}
