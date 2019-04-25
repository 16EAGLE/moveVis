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

#' Outputs animation stats
#'
#' @param n.frames numeric
#' @param fps numeric
#' 
#' @importFrom lubridate dseconds
#' @keywords internal
#' @noRd
.stats <- function(n.frames, fps = 25){
  out(paste0("Approximated animation duration: ≈ ", as.character(dseconds(n.frames/fps)), " at ", toString(fps), " fps for ", toString(n.frames), " frames"))
}

#' verbose lapply
#'
#' @importFrom pbapply pblapply
#' @noRd 
.lapply <- function(X, FUN, ...){
  verbose = getOption("moveVis.verbose")
  if(isTRUE(verbose)) pblapply(X, FUN, ...) else lapply(X, FUN, ...)
}

#' split movement by tail length
#' @importFrom plyr mapvalues
#' @importFrom sp coordinates
#' @importFrom move n.indiv timestamps trackId 
#' 
#' @importFrom methods as
#' @importFrom grDevices colours
#' @noRd 
.m2df <- function(m, path_colours = NA){
  
  ## create data.frame from m with frame time and colour
  m.df <- cbind(as.data.frame(coordinates(m)), id = as.numeric(mapvalues(as.character(trackId(m)), unique(as.character(trackId(m))), 1:n.indiv(m))),
        time = timestamps(m), time_chr = as.character(timestamps(m)), name = as.character(trackId(m)))
  colnames(m.df)[1:2] <- c("x", "y")
  
  ## append data.frame by times missing per track
  # ts <- unique(m.df$time)
  # m.df <- do.call(rbind, lapply(unique(m.df$name), function(x){
  #   df <- m.df[m.df$name == x,]
  #   dummy <- df[1,]
  #   dummy[,c("x", "y", "time", "time_chr")] <- NA
  #   rbind(df, do.call(rbind, lapply(ts[!sapply(ts, function(y) y %in% df$time)], function(z, d = dummy){
  #     d$time <- z
  #     d$time_chr <- as.character(z)
  #     return(d)
  #   })))
  # }))
  
  m.df$frame <- as.numeric(mapvalues(m.df$time_chr, unique(m.df$time_chr), 1:length(unique(m.df$time_chr))))
  # m.df <- m.df[order(m.df$frame),]
  
  ## handle colours, either provided as a field in m or argument or computed randomly
  m.info <- as(m, "data.frame")
  if(all(!is.character(path_colours), !all(is.na(m.info$colour)))){
    
    ## get colours from column
    m.df$colour <- as.character(m.info$colour)
  } else{
    if(!is.character(path_colours)){
      
      path_colours <- c("red", "green", "blue", "yellow", "darkgreen", "orange", "deepskyblue", "darkorange", "deeppink", "navy")
      path_colours <- c(path_colours, sample(colours()[-sapply(path_colours, match, table = colours())]))
      #path_colours <- sample(rep(path_colours, ceiling(n.indiv(m) / length(path_colours))))
    }
    m.df$colour <- mapvalues(m.df$id, unique(m.df$id), path_colours[1:n.indiv(m)])
  }
  
  # if(!is.null(m.info$colour)){
  #   m.df$colour <- as.character(m.info$colour)
  # }else{
  #   if(is.na(path_colours)){
  #     path_colours <- c("red", "green", "blue", "yellow", "darkgreen", "orange", "deepskyblue", "darkorange", "deeppink", "navy")
  #     path_colours <- c(path_colours, sample(colours()[-sapply(path_colours, match, table = colours())]))
  #     path_colours <- rep(path_colours, ceiling(n.indiv(m) / length(path_colours)))
  #   }
  #   m.df$colour <- mapvalues(m.df$id, unique(m.df$id), path_colours[1:n.indiv(m)])
  # }
  
  #m.df$colour <- factor(as.character(m.df$colour), level = unique(as.character(m.df$colour)))
  m.df <- m.df[order(m.df$frame),]
  m.df$name <- factor(as.character(m.df$name), levels = unique(as.character(m.df$name)))
  return(m.df)
}

#' square it
#' @importFrom geosphere distGeo
#' @importFrom sf st_bbox st_transform st_as_sfc st_crs
#' @noRd 
.equidistant <- function(ext, margin_factor = 1){
  
  # lat lon extent
  ext.ll <- st_bbox(st_transform(st_as_sfc(ext), st_crs("+init=epsg:4326")))
  
  # calculate corner coordinates
  corn <- rbind(c(ext.ll[1], ext.ll[2]), c(ext.ll[1], ext.ll[4]), c(ext.ll[3], ext.ll[2]), c(ext.ll[3], ext.ll[4]))
  colnames(corn) <- c("x", "y")
  
  # calculate difference and distance
  ax.dist <- c(distGeo(corn[1,], corn[3,]), distGeo(corn[1,], corn[2,]))
  ax.diff <- c(ext.ll[3]-ext.ll[1], ext.ll[4]-ext.ll[2])
  
  # add difference to match equal distances
  if(ax.dist[1] < ax.dist[2]){
    x.devi <- (ax.diff[1]/ax.dist[1])*((ax.dist[2]-ax.dist[1])*margin_factor)/2
    y.devi <- ((ax.diff[2]/ax.dist[2])*(ax.dist[2]*margin_factor))-ax.diff[2]
  } else{
    x.devi <- ((ax.diff[1]/ax.dist[1])*ax.dist[1])-ax.diff[1]
    y.devi <- ((ax.diff[2]/ax.dist[2])*(ax.dist[1]-ax.dist[2])/2) 
  }
  ext.ll.sq <- st_bbox(c(ext.ll[1]-x.devi, ext.ll[3]+x.devi, ext.ll[2]-y.devi, ext.ll[4]+y.devi), crs = st_crs("+init=epsg:4326"))
  
  ## add margin
  if(margin_factor > 1){
    ax.diff <- c(ext.ll.sq[3]-ext.ll.sq[1], ext.ll.sq[4]-ext.ll.sq[2])
    x.devi <- ((ax.diff[1]*margin_factor)-ax.diff[1])
    y.devi <- ((ax.diff[2]*margin_factor)-ax.diff[2])
    ext.ll.sq <- st_bbox(c(ext.ll[1]-x.devi, ext.ll[3]+x.devi, ext.ll[2]-y.devi, ext.ll[4]+y.devi), crs = st_crs("+init=epsg:4326"))
  }
  return(st_bbox(st_transform(st_as_sfc(ext.ll.sq), st_crs(ext))))
}

#' generate extent
#' @importFrom sf st_bbox st_intersects st_as_sfc
#' @noRd 
.ext <- function(m.df, m.crs, ext = NULL, margin_factor = 1.1, equidistant = FALSE){
  
  ## calcualte ext
  m.ext <- st_bbox(c(xmin = min(m.df$x, na.rm = T), xmax = max(m.df$x, na.rm = T), ymin = min(m.df$y, na.rm = T), ymax = max(m.df$y, na.rm = T)), crs = m.crs)
  if(!is.null(ext)){
    
    # user extent
    gg.ext <- st_bbox(c(xmin = ext@xmin, xmax = ext@xmax, ymin = ext@ymin, ymax = ext@ymax), crs = m.crs)
    if(!quiet(st_intersects(st_as_sfc(gg.ext), st_as_sfc(m.ext), sparse = F)[1,1])) out("Argument 'ext' does not overlap with the extent of 'm'.", type = 3)
    margin_factor <- 1 # no margin since user extent set
  } else{
    gg.ext <- m.ext
  }
  
  # squared equidistant extent or not
  if(isTRUE(equidistant)){
    gg.ext <- .equidistant(gg.ext, margin_factor = margin_factor)
  }else{
    xy.diff <- (gg.ext[3:4] - gg.ext[1:2])/2
    gg.ext <- st_bbox(c(gg.ext[1:2] - (xy.diff*(-1+margin_factor)), gg.ext[3:4] + (xy.diff*(-1+margin_factor))), crs = m.crs)
  }
  return(gg.ext)
}

#' split movement by tail length
#' 
#' @importFrom grDevices colorRampPalette
#' @noRd 
.split <- function(m.df, tail_length = 0, path_size = 1, tail_size = 1, tail_colour = "white", trace_show = F, trace_colour = "grey", path_fade = F){
  
  # m.names <- unique(as.character(m.df$name))
  # dummy <- lapply(m.names, function(mn){
  #   y <- m.df[which(m.df$name == mn)[1],]
  #   y <- cbind(y, tail_colour = NA, tail_size = NA)
  #   y[,c("x", "y", "time", "time_chr")] <- NA
  #   return(y)
  # })
  # names(dummy) <- m.names
  
  #n.out <- max(m.df$frame, na.rm = T)
  #if(isTRUE(path_fade)) n.out <- n.out + tail_length
  
  .lapply(1:max(m.df$frame, na.rm = T), function(i){ # , mn = m.names, d = dummy){
    
    i.range <- seq(i-tail_length, i)
    i.range <- i.range[i.range > 0]
    
    # extract all rows of frame time range
    y <- m.df[!is.na(match(m.df$frame,i.range)),]
    y <- y[order(y$id),]
    
    # compute colour ramp from id count
    #y.colours <- sapply(unique(y$id), function(x) rev(unique(y[y$id == x,]$colour)), simplify = F)
    y.colours <- sapply(unique(y$id), function(x) y[y$id == x,]$colour, simplify = F)
    y.count <- as.vector(table(y$id))
    diff.max <- max(m.df$frame, na.rm = T)-max(i.range)
    
    y$tail_colour <- unlist(mapply(y.cols = y.colours, y.size = y.count, function(y.cols, y.size){
      
      if(all(isTRUE(path_fade), diff.max < tail_length, y.size > diff.max)){
        n <- diff.max+1
        v <- rep(tail_colour, (y.size-(n)))
        y.cols <- tail(y.cols, n = n)
      } else{
        n <- y.size
        v <- NULL
      }
      
      y.ramps <- lapply(unique(y.cols), function(x){
        f <- colorRampPalette(c(x, tail_colour))
        rev(f(n+4)[1:n])
      })
      
      c(v, mapply(i = 1:n, i.ramp = as.numeric(mapvalues(y.cols, unique(y.cols), 1:length(unique(y.cols)))), function(i, i.ramp){
        y.ramps[[i.ramp]][i]
      }, USE.NAMES = F))

    }, SIMPLIFY = F))
 
    # compute tail size from id count
    y$tail_size <- unlist(lapply(y.count, function(y.size){
      
      if(all(isTRUE(path_fade), diff.max < tail_length, y.size > diff.max)){
        n <- diff.max+1
        v <- rep(tail_size, (y.size-(n)))
      } else{
        n <- y.size
        v <- NULL
      }
      c(v, seq(tail_size, path_size, length.out = n))
    }))
    
    if(all(isTRUE(trace_show) & i > tail_size)){
      
      y.trace <- m.df[!is.na(match(m.df$frame,1:(min(i.range)-1))),]
      y.trace$colour <- y.trace$tail_colour <-  trace_colour
      y.trace$tail_size <- tail_size
      
      # join trace, reorder by frame and group by id
      y <- rbind(y, y.trace)
      y <- y[order(y$frame),]
      y <- y[order(y$id),]
    }
    #y$colour <- factor(as.character(y$colour), level = unique(as.character(m.df$colour)))
    #y$name <- factor(as.character(y$name), level = unique(as.character(m.df$name)))
    
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

#' spatial plot function
#' @importFrom ggplot2 geom_path aes_string theme scale_fill_identity scale_y_continuous scale_x_continuous scale_colour_manual theme_bw guides guide_legend coord_sf
#' @noRd 
.gg_spatial <- function(m.split, gg.bmap, m.df, m.crs, path_size = 3, path_end = "round", path_join = "round", path_alpha = 1, equidistant = T, 
                        path_mitre = 10, path_arrow = NULL, print_plot = T, path_legend = T, path_legend_title = "Names"){
  
  # frame plotting function
  gg.fun <- function(x, y){
    
    ## base plot
    p <- y + geom_path(data = x, aes_string(x = "x", y = "y", group = "id"), size = x$tail_size, lineend = path_end, linejoin = path_join,
                       linemitre = path_mitre, arrow = path_arrow, colour = x$tail_colour, alpha = path_alpha) +  theme_bw() +
      scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) + coord_sf(crs = m.crs, datum = m.crs)
    
    ## add legend?
    if(isTRUE(path_legend)){
      l.df <- cbind.data.frame(x = x[1,]$x, y = x[1,]$y, name = levels(m.df$name),
                               colour = as.character(m.df$colour[sapply(as.character(unique(m.df$name)), function(x) match(x, m.df$name)[1] )]), stringsAsFactors = F)
      l.df$name <- factor(l.df$name, levels = l.df$name)
      l.df <- rbind(l.df, l.df)
      
      p <- p + geom_path(data = l.df, aes_string(x = "x", y = "y", colour = "name", linetype = NA), size = path_size, na.rm = TRUE) + scale_colour_manual(values = as.character(l.df$colour), name = path_legend_title) + guides(color = guide_legend(order = 1))
    }    
    
    if(isTRUE(equidistant)) p <- p + theme(aspect.ratio = 1)
    if(isTRUE(print_plot)) print(p) else return(p)
  }
  
  if(length(gg.bmap) > 1) mapply(x = m.split, y = gg.bmap, gg.fun, SIMPLIFY = F, USE.NAMES = F) else lapply(m.split, gg.fun, y = gg.bmap[[1]])
}


#' flow stats plot function
#' @importFrom ggplot2 ggplot geom_path aes_string theme scale_fill_identity scale_y_continuous scale_x_continuous scale_colour_manual theme_bw coord_cartesian geom_bar
#' 
#' @noRd
.gg_flow <- function(m.split, gg.df, path_legend, path_legend_title, path_size, val_seq){

  ## stats plot function
  gg.fun <- function(x, y, pl, plt, ps, vs){
    
    ## generate base plot
    p <- ggplot(x, aes_string(x = "frame", y = "value")) + geom_path(aes_string(group = "id"), size = ps, show.legend = F, colour = x$colour) + 
      coord_cartesian(xlim = c(0, max(y$frame, na.rm = T)), ylim = c(min(vs, na.rm = T), max(vs, na.rm = T))) +
      theme_bw() + theme(aspect.ratio = 1) + scale_y_continuous(expand = c(0,0), breaks = vs) + scale_x_continuous(expand = c(0,0))
    
    ## add legend
    if(isTRUE(pl)){
      l.df <- cbind.data.frame(frame = x[1,]$frame, value = x[1,]$value, name = levels(y$name),
                               colour = as.character(y$colour[sapply(as.character(unique(y$name)), function(x) match(x, y$name)[1] )]), stringsAsFactors = F)
      l.df$name <- factor(l.df$name, levels = l.df$name)
      l.df <- rbind(l.df, l.df)
      p <- p + geom_path(data = l.df, aes_string(x = "frame", y = "value", colour = "name", linetype = NA), size = ps, na.rm = TRUE) + scale_colour_manual(values = as.character(l.df$colour), name = plt)
    }  
    return(p)
  }
  
  .lapply(1:length(m.split), function(i, x = m.split, y = gg.df, pl = path_legend, plt = path_legend_title, ps = path_size, vs = val_seq){
    gg.fun(do.call(rbind, x[1:i])[,c("frame", "value", "time_chr", "id", "colour", "name")], y, pl, plt, ps, vs)
  })
}


#' hist stats plot function
#' @importFrom ggplot2 ggplot geom_path aes_string theme scale_fill_identity scale_y_continuous scale_x_continuous scale_colour_manual theme_bw  coord_cartesian geom_bar
#' @noRd
.gg_hist <- function(l.hist, all.hist, path_legend, path_legend_title, path_size, val_seq, r_type){
  
  ## stats plot function
  gg.fun <- function(x, y, pl, plt, ps, vs, rt){
    
    ## generate base plot
    if(rt == "gradient") p <- ggplot(x, aes_string(x = "value", y = "count")) + geom_path(aes_string(group = "name"), size = ps, show.legend = F, colour = x$colour)
    if(rt == "discrete") p <- ggplot(x, aes_string(x = "value", y = "count", fill = "colour")) + geom_bar(stat = "identity", position = "dodge") + scale_fill_identity()
    
    p <- p + coord_cartesian(xlim = c(min(vs, na.rm = T), max(vs, na.rm = T)), ylim = c(min(y$count, na.rm = T), max(y$count, na.rm = T))) +
      theme_bw() + theme(aspect.ratio = 1) + scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0), breaks = vs)
    
    ## add legend
    if(isTRUE(pl)){
      l.df <- cbind.data.frame(value = x[1,]$value, count = x[1,]$count, name = levels(y$name),
                               colour = as.character(y$colour[sapply(as.character(unique(y$name)), function(x) match(x, y$name)[1] )]), stringsAsFactors = F)
      l.df$name <- factor(l.df$name, levels = l.df$name)
      l.df <- rbind(l.df, l.df)
      p <- p + geom_path(data = l.df, aes_string(x = "value", y = "count", colour = "name", linetype = NA), size = ps, na.rm = TRUE) + scale_colour_manual(values = as.character(l.df$colour), name = plt)
    }
    return(p)
  }
  
  .lapply(l.hist, function(x, y = all.hist, pl = path_legend, plt = path_legend_title, ps = path_size, vs = val_seq, rt = r_type){
    gg.fun(x = x, y = y, pl = pl, plt = plt, ps = ps, vs = vs, rt = rt)
  })
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
.time_conform <- function(m){
  
  m.indi <- if(inherits(m, "MoveStack")) split(m) else list(m)
  ts <- lapply(m.indi, timestamps)
  tl <- lapply(m.indi, timeLag, unit = "secs")
  
  ## check time lag
  uni.lag <- length(unique(unlist(tl))) <= 1
  if(!isTRUE(uni.lag)) out("The temporal resolution of 'm' is diverging. Use align_move() to align movement data to a uniform time scale with a consistent temporal resolution.", type = 3)
  
  ## check temporal consistence per individual (consider to remove, if NA timestamps should be allowed)
  uni.intra <- mapply(x = tl, y = ts, function(x, y) length(c(min(y, na.rm = T), min(y, na.rm = T) + cumsum(x))) == length(y))
  if(!all(uni.intra)) out("For at least one movement track, variating time lags have been detected. Use align_move() to align movement data to a uniform time scale with a consistent temporal resolution.", type = 3)
  
  ## check overall consistence of timestamps
  ts.art <- seq.POSIXt(min(do.call(c, ts), na.rm = T), max(do.call(c, ts), na.rm = T), by = unique(unlist(tl)))
  uni.all <- all(sapply(unique(timestamps(m)), function(x, ta = ts.art) x %in% ta))
  if(!isTRUE(uni.all)) out("For at least one movement track, timestamps diverging from those of the other tracks have been detected. Use align_move() to align movement data to a uniform time scale with a consistent temporal resolution.", type = 3)
  
  ## snippet:: 
  # ts.origin <- as.POSIXct(0, origin = min(ts), tz = tz(ls)) 
  # set.fun <- list("secs" = function(x) `second<-`(x, 0), "mins" = function(x) `minute<-`(x, 0),
  #                 "hours" = function(x) `hour<-`(x, 0), "days" = function(x) `day<-`(x, 1))
  # ts.origin <- lapply(names(set.fun), function(x, fun = set.fun, to = ts.origin) magrittr::freduce(to, fun[!(x == names(fun))]))
  
  ## former::
  # ts.digits <- lapply(c("secs", "mins", "hours", "days"), function(x, ts = timestamps(m)){
  #   sort(unique(as.numeric(format(unique(ts), .convert_units(x)))))
  # })
  # ts.dl <- lapply(ts.digits, function(x) length(unique(diff(x))))
  # sapply(ts.dl, function(x) x > 1)
}

#' get map
#' @importFrom slippymath bbox_to_tile_grid compose_tile_grid
#' @importFrom curl curl_download
#' @importFrom raster projectRaster extent res res<- projectExtent
#' @importFrom magick image_read image_write image_convert
#' @noRd 
.getMap <- function(gg.ext, map_service, map_type, map_token, map_dir, map_res, m.crs){
  
  ## calculate needed slippy tiles using slippymath
  gg.ext.ll <- st_bbox(st_transform(st_as_sfc(gg.ext), crs = st_crs("+init=epsg:4326")))
  tg <- bbox_to_tile_grid(gg.ext.ll, max_tiles = ceiling(map_res*20))
  images <- apply(tg$tiles, MARGIN = 1, function(x){
    file <- paste0(map_dir, map_service, "_", map_type, "_", x[1], "_", x[2], ".png")
    
    retry <- list(do = TRUE, count = 0)
    while(retry$do){
      
      ## download tiles
      url <- paste0(getOption("moveVis.map_api")[[map_service]][[map_type]], tg$zoom, "/", x[1], "/", x[2], ".png", if(map_service == "mapbox") paste0("?access_token=", map_token) else NULL)
      if(!file.exists(file)) curl_download(url = url, destfile = file)
      
      # test if file can be loaded
      catch <- try(image_read(file), silent = T)
      if(inherits(catch, "try-error")){
        unlink(file)
        retry$count <- retry$count+1
        if(retry$count < 10) retry$do <- TRUE else out(paste0("Base map download failed: ", catch), type = 3)
      } else{
        retry$do <- FALSE
      }
    }
    
    ## covnert imagery
    image_write(image_convert(image_read(file), format = "PNG24"), file) # convert single channel png to multi channel png
    return(file)
  })
  
  ## composite imagery
  r <- compose_tile_grid(tg, images)
  list(crop(projectRaster(r, crs = m.crs), extent(gg.ext[1], gg.ext[3], gg.ext[2], gg.ext[4]), snap = "out"))
  
  #projectRaster produces hidden warnings:
  # no non-missing arguments to max; returning -Inf
  # no non-missing arguments to min; returning -Inf
  # seems to be a bug
}

#' interpolate over NAs
#' @importFrom zoo na.approx
#' @noRd
#.approxNA <- function(x) if(all(is.na(x))) rep(NA, length(x)) else na.approx(x, rule = 2)
.approxNA <- function(x){
  y <- na.approx(x, rule = 2)
  
  if(length(y) == length(x)) y else{
    if(length(y) == 0) return(rep(NA, length(x)))
    if(length(y) == 1) return(rep(y, length(x)))
  }
}


#' assign raster to frames
#' @importFrom raster nlayers unstack crop extent stack calc raster setValues
#' @importFrom RStoolbox ggRGB ggR
#' 
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
    
    ## create frame list, top list is bands, second list is times
    r.dummy <- setValues(r.crop[[1]][[1]], NA) #produces warning during tests: no non-missing arguments to max; returning -Inf
    # r.dummy <- raster(r.crop[[1]][[1]]) # and then:
    #`values<-`(r.dummy, NA) # produces same warning. There seems no solution to this to avoid warnings
    r_list <- rep(list(rep(list(r.dummy), length(m.split))), r.nlay)
    
    ## calcualte time differences to r_times
    x <- lapply(m.split, function(y) max(unique(y$time, na.rm = T)))
    frame_times <- unlist(x)
    attributes(frame_times) <- attributes(x[[1]])
    diff.df <- as.data.frame(sapply(r_times, function(x) abs(difftime(frame_times, x, units = "secs"))))
    
    ## assign r_list positions per frame times
    pos.df <- data.frame(frame = 1:nrow(diff.df), pos_r = apply(diff.df, MARGIN = 1, which.min))
    if(isTRUE(fade_raster)) pos.df <- pos.df[apply(diff.df[,unique(pos.df[,2])], MARGIN = 2, which.min),] #pos.df[!duplicated(pos.df$pos_r, fromLast = T),]
    for(i in 1:r.nlay) r_list[[i]][pos.df[,1]] <- r.crop[[i]][pos.df[,2]]
    
    ## interpolate/extrapolate
    if(isTRUE(fade_raster)){
      
      for(i in 1:r.nlay) r_list[[i]] <- stack(r_list[[i]])
      r_list <- lapply(r_list, function(x) unstack(calc(x, fun = .approxNA)))
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
  
  options(moveVis.map_api = list(osm = list(streets = "https://tile.openstreetmap.org/",
                                            streets_de = "http://a.tile.openstreetmap.de/tiles/osmde/",
                                            streets_fr = "https://a.tile.openstreetmap.fr/osmfr/",
                                            humanitarian = "http://a.tile.openstreetmap.fr/hot/",
                                            topographic = "https://a.tile.opentopomap.org/",
                                            #cycle = "https://a.tile.thunderforest.com/cycle/",
                                            #transport = "https://a.tile.thunderforest.com/transport/",
                                            #transport_dark = "https://a.tile.thunderforest.com/transport-dark/",
                                            #landscape = "https://a.tile.thunderforest.com/landscape/",
                                            #outdoors = "https://a.tile.thunderforest.com/outdoors/",
                                            roads = "https://maps.heigit.org/openmapsurfer/tiles/roads/webmercator/",
                                            hydda = "https://a.tile.openstreetmap.se/hydda/full/",
                                            hydda_base = "https://a.tile.openstreetmap.se/hydda/base/",
                                            hike = "http://toolserver.org/tiles/hikebike/",
                                            #hillshade = "http://c.tiles.wmflabs.org/hillshading/",
                                            grayscale = "https://tiles.wmflabs.org/bw-mapnik/",
                                            no_labels = "https://tiles.wmflabs.org/osm-no-labels/",
                                            watercolor = "http://c.tile.stamen.com/watercolor/",
                                            toner = "https://stamen-tiles-a.a.ssl.fastly.net/toner/",
                                            toner_bg = "https://stamen-tiles-a.a.ssl.fastly.net/toner-background/",
                                            toner_lite = "https://stamen-tiles-a.a.ssl.fastly.net/toner-lite/",
                                            terrain = "http://tile.stamen.com/terrain/",
                                            terrain_bg = "http://tile.stamen.com/terrain-background",
                                            mtb = "http://tile.mtbmap.cz/mtbmap_tiles/"),
                                 carto = list(light = "https://a.basemaps.cartocdn.com/light_all/",
                                              light_no_labels = "https://a.basemaps.cartocdn.com/light_nolabels/",
                                              light_only_labels = "https://a.basemaps.cartocdn.com/light_only_labels/",
                                              dark = "https://a.basemaps.cartocdn.com/dark_all/",
                                              dark_no_labels = "https://a.basemaps.cartocdn.com/dark_nolabels/",
                                              dark_only_labels = "https://a.basemaps.cartocdn.com/dark_only_labels/",
                                              voyager = "https://a.basemaps.cartocdn.com/rastertiles/voyager/",
                                              voyager_no_labels = "https://a.basemaps.cartocdn.com/rastertiles/voyager_nolabels/",
                                              voyager_only_labels = "https://a.basemaps.cartocdn.com/rastertiles/voyager_only_labels/",
                                              voyager_labels_under = "https://a.basemaps.cartocdn.com/rastertiles/voyager_labels_under/"),
                                 mapbox = lapply(c(satellite = "mapbox.satellite",
                                                   streets = "mapbox.streets",
                                                   streets_basic = "mapbox.streets-basic",
                                                   hybrid = "mapbox.streets-satellite",
                                                   light = "mapbox.light",
                                                   dark = "mapbox.dark",
                                                   high_contrast = "mapbox.high-contrast",
                                                   outdoors = "mapbox.outdoors",
                                                   hike = "mapbox.run-bike-hike",
                                                   wheatpaste = "mapbox.wheatpaste",
                                                   pencil = "mapbox.pencil",
                                                   comic = "mapbox.comic",
                                                   pirates = "mapbox.pirates",
                                                   emerald = "mapbox.emerald"), function(x) paste0("https://api.mapbox.com/v4/", x, "/"))))
                                 # esri = list(streets = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/",
                                 #             #delorme = "https://server.arcgisonline.com/ArcGIS/rest/services/Specialty/DeLorme_World_Base_Map/MapServer/tile/",
                                 #             topo = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/",
                                 #             satellite = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/",
                                 #             terrain = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Terrain_Base/MapServer/tile/",
                                 #             relief = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Shaded_Relief/MapServer/tile/",
                                 #             #physical = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Physical_Map/MapServer/tile/",
                                 #             ocean = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean_Basemap/MapServer/tile/",
                                 #             natgeo = "https://server.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/",
                                 #             grey = "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/")))
}