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

#' split movement by tail length
#' @noRd 
.split <- function(m.df, tail_length, path_size, tail_size){
  lapply(1:(max(m.df$frame)), function(i){
    
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
    return(y)
  })
}

#' plot function
#' @importFrom ggplot2 geom_path aes theme scale_fill_identity scale_y_continuous scale_x_continuous
#' @noRd 
.gg <- function(m.split, gg.bmap, path_size = 3, path_end = "round", path_join = "round", squared = T, 
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
#' @noRd 
.getMap <- function(gg.ext, map_service, map_type, map_token, map_dir){

  ## calculate tiles and get map imagery
  tg <- bb_to_tg(gg.ext, max_tiles = 20)
  images <- apply(tg$tiles, MARGIN = 1, function(x){
    file <- paste0(map_dir, x[1], "_", x[2], ".jpg")
    if(!isTRUE(file.exists(file))){
      curl_download(url = paste0("https://api.mapbox.com/v4/mapbox.satellite/", tg$zoom, "/", x[1], "/", x[2], ".jpg90", "?access_token=", map_token))
    }
    return(file)
  })
  
  ## composite imagery
  r <- tg_composite(tg, images)
  list(crop(projectRaster(r, crs = crs(m)), extent(gg.ext[1], gg.ext[3], gg.ext[2], gg.ext[4])))
}

#' assign raster to frames
#' @importFrom raster nlayers unstack crop extent setValues stack approxNA
#' @importFrom RStoolbox ggRGB ggR
#' @noRd
.ggFrames <- function(r_list, r_times, r_type, m.split, gg.ext, fade_raster = T, ...){
  
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
      r_list <- lapply(r_list, function(x) unstack(approxNA(x, rule = 2)))
    }
  } else{ r_list <- r.crop}
  
  if(length(r_list) == 1){
    if(r_type == "gradient") gg.bmap <- lapply(r_list[[1]], ggR, ggObj = T, geom_raster = T)
    if(r_type == "discrete") gg.bmap <- lapply(r_list[[1]], ggR, ggObj = T, geom_raster = T, forceCat = T)
  } else{ gg.bmap <- lapply(1:length(r_list[[1]]), function(i) ggRGB(stack(lapply(r_list, "[[", i)),  r = 1, g = 2, b = 3, ggObj = T))}
  return(gg.bmap)
}

#' package startup
#' @noRd 
.onLoad <- function(libname, pkgname){
  if(is.null(getOption("moveVis.verbose")))  options(moveVis.verbose = FALSE)
}