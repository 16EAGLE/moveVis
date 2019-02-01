#' Suppress messages and warnings
#' @noRd 
quiet <- function(expr){
  #return(expr)
  return(suppressWarnings(suppressMessages(expr)))
}

#' Outputs errors, warnings and messages
#'
#' @param input character
#' @param type numeric, 1 = message/cat, 2 = warning, 3 = error and stop
#' @param msg logical. If \code{TRUE}, \code{message} is used instead of \code{cat}. Default is \code{FALSE}.
#' @param sign character. Defines the prefix string.
#'
#' @keywords internal
#' @noRd

out <- function(input, type = 1, ll = NULL, msg = FALSE, sign = "", verbose = getOption("moveVis.verbose")){
  if(is.null(ll)) if(isTRUE(verbose)) ll <- 1 else ll <- 2
  if(type == 2 & ll <= 2){warning(paste0(sign,input), call. = FALSE, immediate. = TRUE)}
  else{if(type == 3){stop(input, call. = FALSE)}else{if(ll == 1){
    if(msg == FALSE){ cat(paste0(sign,input),sep="\n")
    } else{message(paste0(sign,input))}}}}
}

#' split movement by tail length
#' @importFrom plyr mapvalues
#' @importFrom sp coordinates
#' @importFrom move n.indiv timestamps trackId
#' @noRd 
.m2df <- function(m){
  
  ## create data.frame from m with frame time and colour
  m.df <- cbind(as.data.frame(coordinates(m)), id = as.numeric(mapvalues(as.character(trackId(m)), unique(as.character(trackId(m))), 1:n.indiv(m))),
        time = timestamps(m), time_chr = as.character(timestamps(m)), name = as.character(trackId(m)))
  colnames(m.df)[1:2] <- c("x", "y")
  m.df$frame <- as.numeric(mapvalues(m.df$time_chr, unique(m.df$time_chr), 1:length(unique(m.df$time_chr))))
  
  ## handle colours, either provided as a field in m or computed, if not
  m.info <- as.data.frame(m)
  if(!is.null(m.info$colour)){
    m.df$colour <- m.info$colour
  }else{
    def.colours <- c("red", "green", "blue", "yellow", "darkgreen", "orange", "deepskyblue", "darkorange", "deeppink", "navy")
    def.colours <- c(def.colours, sample(colours()[-sapply(def.colours, match, table = colours())]))
    m.df$colour <- mapvalues(m.df$id, unique(m.df$id), def.colours[1:n.indiv(m)])
  }
  return(m.df)
}

#' square it
#' @importFrom geosphere distGeo
#' @importFrom sf st_bbox
#' @noRd 
.squared <- function(ext, margin_factor = 1){
  
  # calculate corner coordinates
  corn <- rbind(c(ext[1], ext[2]), c(ext[1], ext[4]), c(ext[3], ext[2]), c(ext[3], ext[4]))
  colnames(corn) <- c("x", "y")
  
  # calculate difference and distance
  ax.dist <- c(distGeo(corn[1,], corn[3,]), distGeo(corn[1,], corn[2,]))
  ax.diff <- c(ext[3]-ext[1],ext[4]-ext[2])
  
  # add difference to match equal distances
  if(ax.dist[1] < ax.dist[2]){
    x.devi <- (ax.diff[1]/ax.dist[1])*((ax.dist[2]-ax.dist[1])*margin_factor)/2
    y.devi <- ((ax.diff[2]/ax.dist[2])*(ax.dist[2]*margin_factor))-ax.diff[2]
  } else{
    x.devi <- ((ax.diff[1]/ax.dist[1])*(ax.dist[1]*margin_factor))-ax.diff[2]
    y.devi <- (ax.diff[2]/ax.dist[2])*((ax.dist[1]-ax.dist[2])*margin_factor)/2
  }
  return(st_bbox(c(ext[1]-x.devi, ext[3]+x.devi, ext[2]-y.devi, ext[4]+y.devi)))
}

#' generate extent
#' @importFrom sf st_bbox st_crs st_intersects st_as_sfc
#' @importFrom sp proj4string
#' @noRd 
.ext <- function(m.df, ext, margin_factor){
  
  ## calcualte square or user extent
  m.ext <- st_bbox(c(xmin = min(m.df$x), xmax = max(m.df$x), ymin = min(m.df$y), ymax = max(m.df$y)), crs = st_crs(proj4string(m)))
  if(!is.null(ext)){
    gg.ext <- st_bbox(c(xmin = ext@xmin, xmax = ext@xmax, ymin = ext@ymin, ymax = ext@ymax), crs = st_crs(proj4string(m)))
    if(!quiet(st_intersects(st_as_sfc(gg.ext), st_as_sfc(m.ext), sparse = F)[1,1])) out("Argument 'ext' does not overlap with the extent of 'm'.", type = 3)
  }else gg.ext <- .squared(m.ext, margin_factor = margin_factor)
  return(gg.ext)
}

#' split movement by tail length
#' @importFrom pbapply pblapply
#' @noRd 
.split <- function(m.df, tail_length = 0, path_size = 1, tail_size = 1){
  
  # m.names <- unique(as.character(m.df$name))
  # dummy <- lapply(m.names, function(mn){
  #   y <- m.df[which(m.df$name == mn)[1],]
  #   y <- cbind(y, tail_colour = NA, tail_size = NA)
  #   y[,c("x", "y", "time", "time_chr")] <- NA
  #   return(y)
  # })
  # names(dummy) <- m.names
  
  pblapply(1:(max(m.df$frame)), function(i){ # , mn = m.names, d = dummy){
    
    i.range <- seq(i-tail_length, i)
    i.range <- i.range[i.range > 0]
    
    # extract all rows of frame time range
    y <- m.df[!is.na(match(m.df$frame,i.range)),]
    y <- y[order(y$id),]
    
    # compute colour ramp from id count
    y$tail_colour <- unlist(mapply(x = unique(y$colour), y = table(y$id), function(x, y){
      f <- colorRampPalette(c(x, "white"))
      rev(f(y+4)[1:y])
    }, SIMPLIFY = F))
    
    # compute tail size from id count
    y$tail_size <- unlist(lapply(table(y$id), function(x) seq(tail_size, path_size, length.out = x)))
    
    # add NA rows, if needed ---> WRONG WAY: DO THIS FOR THE DATA.FRAME ALREADY, THAN trim leading and trailing NAs
    # missing.names <- sapply(mn, function(x) x %in% y$name)
    # if(!all(missing.names)){
    #   add.rows <- do.call(rbind, lapply(d[!missing.names], function(x){
    #     x$frame <- max(y$frame)
    #     return(x)
    #   }))
    #   y <- rbind(y, add.rows)
    # }
    return(y)
  })
}

#' plot function
#' @importFrom ggplot2 geom_path aes theme scale_fill_identity scale_y_continuous scale_x_continuous
#' @noRd 
.gg_spatial <- function(m.split, gg.bmap, path_size = 3, path_end = "round", path_join = "round", squared = T, 
                        path_mitre = 10, path_arrow = NULL, print_plot = T){
  
  # frame plotting function
  gg.fun <- function(x, y){
    p <- y + geom_path(aes(x = x, y = y, group = id), data = x, size = x$tail_size, lineend = path_end, linejoin = path_join,
                       linemitre = path_mitre, arrow = path_arrow, colour = x$tail_colour) + 
      scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0))
    if(isTRUE(squared)) p <- p + theme(aspect.ratio = 1)
    if(isTRUE(print_plot)) print(p) else return(p)
  }
  
  if(length(gg.bmap) > 1) mapply(x = m.split, y = gg.bmap, gg.fun, SIMPLIFY = F, USE.NAMES = F) else lapply(m.split, gg.fun, y = gg.bmap[[1]])
}

#' add to frames
#' @noRd 
.addToFrames <- function(frames, eval) lapply(frames, function(x, y = eval){
  x + y
})

#' convert units
#' @noRd 
.convert_units <- function(unit){
  unit.c <- c("secs" = "%S", "mins" = "%M", "hours" = "%H", "days" = "%d")
  sub <- match(unit, unit.c)
  if(is.na(sub)){
    sub <- match(unit, names(unit.c))
    if(is.na(sub)) out(paste0("Unit '", unit, "' is not supported."), type = 3) else unit.c[sub]
  } else{
    return(names(unit.c)[sub])
  }
}

#' detect time gaps
#' @noRd 
.time_gaps <- function(m){
  ts.digits <- lapply(c("secs", "mins", "hours", "days"), function(x, ts = timestamps(m)) sort(unique(as.numeric(format(unique(timestamps(m)), .convert_units(x))))))
  ts.dl <- lapply(ts.digits, function(x) length(unique(diff(x))))
  sapply(ts.dl, function(x) x > 1)
}

#' get map
#' @importFrom slippymath bb_to_tg tg_composite
#' @importFrom curl curl_download
#' @importFrom raster projectRaster crs extent
#' @importFrom magick image_read image_write
#' @noRd 
.getMap <- function(gg.ext, map_service, map_type, map_token, map_dir, map_res){
  
  ## calculate needed slippy tiles using slippymath
  tg <- bb_to_tg(gg.ext, max_tiles = ceiling(map_res*20))
  images <- apply(tg$tiles, MARGIN = 1, function(x){
    file <- paste0(map_dir, map_service, "_", map_type, "_", x[1], "_", x[2], ".png")
    if(!isTRUE(file.exists(file))){
      
      ## download tiles
      if(map_service == "mapbox") curl_download(url = paste0(getOption("moveVis.map_api")$mapbox, getOption("moveVis.mapbox_types")[[map_type]], "/", tg$zoom, "/", x[1], "/", x[2], ".png", "?access_token=", map_token), destfile = file)
      if(map_service == "osm") curl_download(url = paste0(getOption("moveVis.map_api")$osm[[map_type]], tg$zoom, "/", x[1], "/", x[2], ".png"), destfile = file)
      
      ## covnert imagery
      image_write(image_convert(image_read(file), format = "PNG24"), file) # convert single channel png to multi channel png
    }
    return(file)
  })
  
  ## composite imagery
  r <- tg_composite(tg, images)
  list(crop(projectRaster(r, crs = crs(m)), extent(gg.ext[1], gg.ext[3], gg.ext[2], gg.ext[4])))
}

#' interpolate over NAs
#' @importFrom zoo na.approx
#' @noRd
.approxNA <- function(x) na.approx(x, rule = 2)

#' assign raster to frames
#' @importFrom raster nlayers unstack crop extent setValues stack approxNA
#' @importFrom RStoolbox ggRGB ggR
#' @importFrom pbapply pblapply
#' @noRd
.rFrames <- function(r_list, r_times, m.split, gg.ext, fade_raster = T, ...){
  
  if(!is.list(r_list)){
    r_list <- list(r_list)
    n <- 1
  } else n <- length(r_list)
  
  ## rearrange bandwise and crop
  r.nlay <- nlayers(r_list[[1]])
  if(r.nlay > 1) r_list <- lapply(1:r.nlay, function(i) lapply(r_list, "[[", i)) else r_list <- list(r_list)
  
  r.crop <- lapply(r_list, function(r.lay) lapply(r.lay, crop, y = extent(gg.ext[1], gg.ext[3], gg.ext[2], gg.ext[4]), snap = "out"))
  
  if(n > 1){
    
    ## calcualte time differences
    pos_diff <- lapply(r_times, function(x) sapply(lapply(m.split, function(x) max(unique(x$time))), difftime, time2 = x))
    pos_r <- sapply(pos_diff, which.min)
    
    ## create frame list, top list is bands, second list is times
    r.dummy <- setValues(r.crop[[1]][[1]], NA)
    r_list <- rep(list(rep(list(r.dummy), length(m.split))), r.nlay)
    
    if(!isTRUE(fade_raster)){
      
      ## assign rasters to all frames, hard changes with distance of frame times to raster times
      pos_frames <- c(head(pos_r, n=-1) + round(diff(pos_r)/2))
      pos_frames <- cbind(c(pos_r[1], pos_frames), c(pos_frames-1, length(m.split)), 1:length(r_times))
      if(pos_frames[1,1] != 1) pos_frames[1,1] <- 1
      pos_frames <- cbind(1:length(m.split), unlist(apply(pos_frames, MARGIN = 1, function(x) rep(x[3], diff(x[1:2])+1))))
    } else{
      
      ## assign rasters to frames only to frames with closest raster times
      pos_frames <- cbind(pos_r, 1:length(pos_r))
      if(pos_frames[1,1] != 1) pos_frames[1,1] <- 1
    }
    for(i in 1:r.nlay) r_list[[i]][pos_frames[,1]] <- r.crop[[i]][pos_frames[,2]]
    
    ## interpolate/extrapolate
    if(isTRUE(fade_raster)){
      
      for(i in 1:r.nlay) r_list[[i]] <- stack(r_list[[i]])
      r_list <- lapply(r_list, function(x) unstack(calc(x, .approxNA))) # 14 sec for >5000 single-layer frames interpolation
      
      #for(i in 1:r.nlay) r_list[[i]] <- stack(r_list[[i]])
      #r_list <- lapply(r_list, function(x) unstack(approxNA(x, rule = 2))) # unstack is super slow! This line slows down everything.
    }
  } else{r_list <- r.crop}
  return(r_list)
}

#' package startup
#' @importFrom pbapply pboptions
#' @noRd 
.onLoad <- function(libname, pkgname){
  pboptions(type = "timer", char = "=", txt.width = getOption("width")-30) # can be changed to "none"
  if(is.null(getOption("moveVis.verbose")))  options(moveVis.verbose = FALSE)
  if(is.null(getOption("moveVis.mapbox_types"))){
    options(moveVis.mapbox_types = list(satellite = "mapbox.satellite", streets = "mapbox.streets", streets_basic = "mapbox.streets-basic",
                                        hybrid = "mapbox.streets-satellite", light = "mapbox.light", dark = "mapbox.dark",
                                        high_contrast = "mapbox.high-contrast", outdoors = "mapbox.outdoors", hike = "mapbox.run-bike-hike",
                                        wheatpaste = "mapbox.wheatpaste", pencil = "mapbox.pencil", comic = "mapbox.comic",
                                        pirates = "mapbox.pirates", emerald = "mapbox.emerald" ))
  }
  if(is.null(getOption("moveVis.map_api"))){
    options(moveVis.map_api = list(mapbox = "https://api.mapbox.com/v4/",
                                   osm = list(streets = "https://tile.openstreetmap.org/",
                                              humanitarian = "http://a.tile.openstreetmap.fr/hot/",
                                              hike = "http://toolserver.org/tiles/hikebike/",
                                              #hillshade = "http://c.tiles.wmflabs.org/hillshading/",
                                              grayscale = "https://tiles.wmflabs.org/bw-mapnik/",
                                              no_labels = "https://tiles.wmflabs.org/osm-no-labels/",
                                              toner = "http://a.tile.stamen.com/toner/",
                                              watercolor = "http://c.tile.stamen.com/watercolor/")))
  }
}