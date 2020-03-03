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
  out(paste0("Approximated animation duration: \u2248 ", as.character(dseconds(n.frames/fps)), " at ", toString(fps), " fps for ", toString(n.frames), " frames"))
}

#' Replace value
#'
#' @param data vector to be replaced by values
#' @param x values which to replace
#' @param y values to replace to
#'
#' @keywords internal
#' @noRd
repl_vals <- function(data, x, y){
  for(i in 1:length(x)) data[data == x[i]] <- y[i]
  data <- methods::as(data, class(y))
  return(data)
}

#' verbose lapply
#'
#' @importFrom pbapply pblapply
#' @noRd 
.lapply <- function(X, FUN, ..., moveVis.verbose = NULL, moveVis.n_cores = NULL, moveVis.export = NULL){
  if(is.null(moveVis.verbose)) moveVis.verbose <- getOption("moveVis.verbose")
  if(is.null(moveVis.n_cores)) moveVis.n_cores <- getOption("moveVis.n_cores")
  
  # with parallelization
  if(moveVis.n_cores > 1){
    cl <- parallel::makeCluster(moveVis.n_cores)
    if(!is.null(moveVis.export)) parallel::clusterExport(cl, moveVis.export)
    y <- try(parallel::parLapply(cl = cl, X, FUN, ...)) # ensures that cluster is stopped appropriately
    parallel::stopCluster(cl)
    if(inherits(y, "try-error")) out(y, type = 3) else return(y)
  
  # without parallelization
  }else if(isTRUE(moveVis.verbose)) pblapply(X, FUN, ...) else lapply(X, FUN, ...)
}

#' verbose apply
#'
#' @importFrom pbapply pbapply
#' @noRd 
.apply <- function(X, MARGIN, FUN, ...){
  verbose = getOption("moveVis.verbose")
  if(isTRUE(verbose)) pbapply(X, MARGIN, FUN, ...) else apply(X, MARGIN, FUN, ...)
}

#' split movement by tail length
#' @importFrom move n.indiv timestamps trackId 
#' @noRd 
.m2df <- function(m, path_colours = NA){
  
  ## create data.frame from m with frame time and colour
  m.df <- cbind(as.data.frame(m@coords), id = repl_vals(as.character(trackId(m)), unique(as.character(trackId(m))), 1:n.indiv(m)),
        time = timestamps(m), time_chr = as.character(timestamps(m)), name = as.character(trackId(m)))
  colnames(m.df)[1:2] <- c("x", "y")
  
  m.df$frame <- sapply(m.df$time, function(x) which(sort(unique(m.df$time)) == x))
  
  ## handle colours, either provided as a field in m or argument or computed randomly
  m.info <- methods::as(m, "data.frame")
  if(all(!is.character(path_colours), !all(is.na(m.info$colour)))){
    
    ## get colours from column
    m.df$colour <- as.character(m.info$colour)
  } else{
    if(!is.character(path_colours)){
      
      path_colours <- c("red", "green", "blue", "yellow", "darkgreen", "orange", "deepskyblue", "darkorange", "deeppink", "navy")
      path_colours <- c(path_colours, sample(grDevices::colours()[-sapply(path_colours, match, table = grDevices::colours())]))
      #path_colours <- sample(rep(path_colours, ceiling(n.indiv(m) / length(path_colours))))
    }
    m.df$colour <- repl_vals(m.df$id, unique(m.df$id), path_colours[1:n.indiv(m)])
  }
  
  m.df <- m.df[order(m.df$frame),]
  m.df$name <- factor(as.character(m.df$name), levels = unique(as.character(m.df$name)))
  return(m.df)
}

#' square it
#' @importFrom sf st_bbox st_transform st_as_sfc st_crs st_distance st_sfc st_point
#' @noRd 
.equidistant <- function(ext, margin_factor = 1){
  
  # lat lon extent
  ext.ll <- st_bbox(st_transform(st_as_sfc(ext), st_crs("+init=epsg:4326")))
  
  # calculate corner coordinates
  corn <- list(c(ext.ll[1], ext.ll[2]), c(ext.ll[1], ext.ll[4]), c(ext.ll[3], ext.ll[2]), c(ext.ll[3], ext.ll[4]))
  corn <- lapply(corn, function(x) st_sfc(st_point(x), crs = st_crs("+init=epsg:4326")))
  
  # calculate difference and distance
  ax.dist <- as.numeric(c(suppressPackageStartupMessages(st_distance(corn[[1]], corn[[3]])), suppressPackageStartupMessages(st_distance(corn[[1]], corn[[2]]))))
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
.ext <- function(m.df, m.crs, ext = NULL, margin_factor = 1.1, equidistant = FALSE, cross_dateline = FALSE){
  
  ## calcualte ext
  m.ext <- st_bbox(c(xmin = min(m.df$x, na.rm = T), xmax = max(m.df$x, na.rm = T), ymin = min(m.df$y, na.rm = T), ymax = max(m.df$y, na.rm = T)), crs = m.crs)
  if(!is.null(ext)){
    
    # user extent
    gg.ext <- st_bbox(c(xmin = ext[1], xmax = ext[2], ymin = ext[3], ymax = ext[4]), crs = m.crs)
    if(!quiet(st_intersects(st_as_sfc(gg.ext), st_as_sfc(m.ext), sparse = F)[1,1])) out("Argument 'ext' does not overlap with the extent of 'm'.", type = 3)
    margin_factor <- 1 # no margin since user extent set
  } else{
    gg.ext <- m.ext
  }
  
  xy.diff <- if(isTRUE(cross_dateline)){
    c(abs(abs(max(m.df$x[m.df$x < 0])) - min(m.df$x[m.df$x > 0])), gg.ext[4]-gg.ext[2])/2
  }else (gg.ext[3:4] - gg.ext[1:2])/2
  
  
  # squared equidistant extent or not
  if(isTRUE(cross_dateline)){
    
    # split extents for both dateline sides
    gg.ext <- list("west" = gg.ext, "east" = gg.ext)
    
    # cut extents and add margins to x components
    gg.ext$west[[1]] <- -180 #xmin
    gg.ext$west[[3]] <- max(m.df$x[m.df$x < 0]) + xy.diff[1]*(-1+margin_factor) #xmax
    gg.ext$east[[1]] <- min(m.df$x[m.df$x > 0]) - xy.diff[1]*(-1+margin_factor) #xmin
    gg.ext$east[[3]] <- 180 #xmax
    
    # add margins to y components
    gg.ext$west[[2]] <- gg.ext$west[[2]] - xy.diff[2]*(-1+margin_factor) #ymin
    gg.ext$west[[4]] <- gg.ext$west[[4]] + xy.diff[2]*(-1+margin_factor) #ymax
    gg.ext$east[[2]] <- gg.ext$east[[2]] - xy.diff[2]*(-1+margin_factor) #ymin
    gg.ext$east[[4]] <- gg.ext$east[[4]] + xy.diff[2]*(-1+margin_factor) #ymax
    
  } else{
    
    # equidistant currently not supported for cross_dateline
    if(isTRUE(equidistant)){
      gg.ext <- .equidistant(gg.ext, margin_factor = margin_factor)
    }else{
      gg.ext <- st_bbox(c(gg.ext[1:2] - (xy.diff*(-1+margin_factor)), gg.ext[3:4] + (xy.diff*(-1+margin_factor))), crs = m.crs)
    }
    
    # cut by longlat maximums
    if(isTRUE(m.crs$epsg == 4326)){
      if(gg.ext[1] < -180) gg.ext[1] <- -180
      if(gg.ext[3] > 180) gg.ext[3] <- 180
      if(gg.ext[2] < -90) gg.ext[2] <- -90
      if(gg.ext[4] > 90) gg.ext[4] <- 90
    }
  }
  return(gg.ext)
}

#' calculate x labels from breaks
#' @noRd 
.x_labels <- function(x){
  x.num <- x
  
  # remove NAs
  x[is.na(x.num)] <- ""
  x.num[is.na(x.num)] <- 0
  
  # shift dateline crossings
  x.num[x.num < -180] <- x.num[x.num < -180]+360
  x.num[x.num > 180] <- x.num[x.num > 180]-360
  
  
  x <- as.character(abs(x.num))
  x <- paste0(x, "°")
  
  # assign Northing/Southing
  x[x.num > 0] <- paste0(x[x.num > 0], "E")
  x[x.num < 0] <- paste0(x[x.num < 0], "W")
  return(x)
}

#' calculate y labels from breaks
#' @noRd 
.y_labels <- function(x){
  x.num <- x
  x <- as.character(abs(x.num))
  x <- paste0(x, "°")
  
  # remove NAs
  x[is.na(x.num)] <- ""
  x.num[is.na(x.num)] <- 0
  
  # assign Northing/Southing
  x[x.num > 0] <- paste0(x[x.num > 0], "N")
  x[x.num < 0] <- paste0(x[x.num < 0], "S")
  return(x)
}


#' create paths data.frame for gg on the fly per frame
#' @noRd 
.df4gg <- function(m.df, i, tail_length = 0, path_size = 1, tail_size = 1, tail_colour = "white", trace_show = F, trace_colour = "grey", path_fade = F){
  
  # calc range
  i.range <- seq(i-tail_length, i)
  i.range <- i.range[i.range > 0]
  
  # extract all rows of frame time range
  paths <- m.df[!is.na(match(m.df$frame,i.range)),]
  paths <- paths[order(paths$id),]
  
  # compute colour ramp from id count
  #paths.colours <- sapply(unique(paths$id), function(x) rev(unique(paths[paths$id == x,]$colour)), simplify = F)
  paths.colours <- sapply(unique(paths$id), function(x) paths[paths$id == x,]$colour, simplify = F)
  paths.count <- as.vector(table(paths$id))
  diff.max <- max(m.df$frame, na.rm = T)-max(i.range)
  
  paths$tail_colour <- unlist(mapply(paths.cols = paths.colours, paths.size = paths.count, function(paths.cols, paths.size){
    
    if(all(isTRUE(path_fade), diff.max < tail_length, paths.size > diff.max)){
      n <- diff.max+1
      v <- rep(tail_colour, (paths.size-(n)))
      paths.cols <- utils::tail(paths.cols, n = n)
    } else{
      n <- paths.size
      v <- NULL
    }
    
    paths.ramps <- lapply(unique(paths.cols), function(x){
      f <- grDevices::colorRampPalette(c(x, tail_colour))
      rev(f(n+4)[1:n])
    })
    
    c(v, mapply(i = 1:n, i.ramp = repl_vals(paths.cols, unique(paths.cols), 1:length(unique(paths.cols))), function(i, i.ramp){
      paths.ramps[[i.ramp]][i]
    }, USE.NAMES = F))
    
  }, SIMPLIFY = F))
  
  # compute tail size from id count
  paths$tail_size <- unlist(lapply(paths.count, function(paths.size){
    
    if(all(isTRUE(path_fade), diff.max < tail_length, paths.size > diff.max)){
      n <- diff.max+1
      v <- rep(tail_size, (paths.size-(n)))
    } else{
      n <- paths.size
      v <- NULL
    }
    c(v, seq(tail_size, path_size, length.out = n))
  }))
  
  paths$trace <- FALSE
  if(all(isTRUE(trace_show) & i > tail_size)){
    
    paths.trace <- m.df[!is.na(match(m.df$frame,1:(min(i.range)))),]
    paths.trace$colour <- paths.trace$tail_colour <-  trace_colour
    paths.trace$tail_size <- tail_size
    paths.trace$trace <- TRUE
    
    # join trace, reorder by frame and group by id
    paths <- rbind(paths, paths.trace)
    paths <- paths[order(paths$frame),]
    paths <- paths[order(paths$id),]
  }
  return(paths)
}

#' spatial plot function
#' @importFrom ggplot2 ggplot geom_path aes_string theme scale_fill_identity scale_y_continuous scale_x_continuous scale_colour_manual theme_bw guides guide_legend coord_sf expr geom_raster geom_tile
#' @importFrom raster aggregate ncell
#' @noRd 
.gg_spatial <- function(r_list, r_type, m.df, path_size = 3, path_end = "round", path_join = "round", path_alpha = 1, equidistant = T, 
                        path_mitre = 10, path_arrow = NULL, print_plot = T, path_legend = T, path_legend_title = "Names",
                        tail_length = 0, tail_size = 1, tail_colour = "white", trace_show = F, trace_colour = "grey", path_fade = F, ...){
  
  # frame plotting function
  gg.fun <- function(x, y){
    
    ## scale plot to extent and set na.rm to TRUE to avoid warnings
    y$layers[[1]]$geom_params$na.rm <- T
    
    x_path <- x[!x$trace,]
    x_trace <- x[x$trace,]
  
    ## trace plot
    if(nrow(x_trace) > 1){
      p <- y + geom_path(data = x_trace, aes_string(x = "x", y = "y", group = "id"), size = x_trace$tail_size, lineend = path_end, linejoin = path_join,
                          linemitre = path_mitre, arrow = path_arrow, colour = x_trace$tail_colour, alpha = path_alpha, na.rm = T)
    } else p <- y
    
    ## base plot
    p <- p + geom_path(data = x_path, aes_string(x = "x", y = "y", group = "id"), size = x_path$tail_size, lineend = path_end, linejoin = path_join,
                       linemitre = path_mitre, arrow = path_arrow, colour = x_path$tail_colour, alpha = path_alpha, na.rm = T) + 
      theme_bw() + x$coord[[1]] + x$scalex[[1]] + x$scaley[[1]]
    
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
  
  # create base maps
  gg.bmap <- function(r, r_type, ...){
    extras <- list(...)
    if(!is.null(extras$maxpixels)) maxpixels <- extras$maxpixels else maxpixels <- 500000
    if(!is.null(extras$alpha)) alpha <- extras$alpha else alpha <- 1
    if(!is.null(extras$maxColorValue)) maxColorValue <- extras$maxColorValue else maxColorValue <- NA
    
    # aggregate raster if too large
    if(maxpixels < ncell(r)) r <- aggregate(r, fact = ceiling(ncell(r)/maxpixels))
    
    # transform into data.frame
    df <- data.frame(raster::as.data.frame(r, xy = T))
    colnames(df) <- c("x", "y", paste0("val", 1:(ncol(df)-2)))
    
    # factor if discrete to show categrocial legend
    df$fill <- df$val1
    if(r_type == "discrete") df$fill <- as.factor(df$fill)
    
    # transform to RGB colours
    if(r_type == "RGB"){
      if(is.na(maxColorValue)) maxColorValue <- max(c(df$val1, df$val2, df$val3), na.rm = T)
      
      if(maxColorValue < max(c(df$val1, df$val2, df$val3), na.rm = T)){
        out("maxColorValue < maximum raster value. maxColorValue is set to maximum raster value.", type = 2)
        maxColorValue <- max(c(df$val1, df$val2, df$val3), na.rm = T)
      }
      
      # remove NAs
      na.sel <- is.na(df$val1) & is.na(df$val2) & is.na(df$val3)
      if(any(na.sel)) df <- df[!na.sel,]
      
      df$fill <- grDevices::rgb(red = df$val1, green = df$val2, blue = df$val3, maxColorValue = maxColorValue)
    } else{
      
      # remove NAs
      na.sel <- is.na(df$val1)
      if(any(na.sel)) df <- df[!na.sel,]
    }
    gg <- ggplot(df)
    
    # if NA gaps are there, use geom_tile, otherwise make it fast using geom_raster
    if(any(na.sel)){
      gg <- gg + geom_tile(aes_string(x = "x", y = "y", fill = "fill"), alpha = alpha)
    } else{
      gg <- gg + geom_raster(aes_string(x = "x", y = "y", fill = "fill"), alpha = alpha)
    }
    
    if(r_type == "RGB") gg <- gg + scale_fill_identity() 
    return(gg)
  }
  
  # create frames
  i <- NULL # needs to be defined for checks
  if(length(r_list) > 1){
    frames <- .lapply(1:max(m.df$frame), function(i) gg.fun(x = .df4gg(m.df, i = i, tail_length = tail_length, path_size = path_size, tail_size = tail_size, tail_colour = tail_colour,
                                                                       trace_show = trace_show, trace_colour = trace_colour, path_fade = path_fade),
                                                            y = gg.bmap(r = r_list[[i]], r_type, ...)), moveVis.n_cores = 1)
  } else{
    bmap <- gg.bmap(r = r_list[[1]], r_type, ...)
    frames <- .lapply(1:max(m.df$frame), function(i) gg.fun(x = .df4gg(m.df, i = i, tail_length = tail_length, path_size = path_size, tail_size = tail_size, tail_colour = tail_colour, 
                                                                       trace_show = trace_show, trace_colour = trace_colour, path_fade = path_fade),
                                                            y = bmap), moveVis.n_cores = 1)
  }
  return(frames)
}


#' flow stats plot function
#' @importFrom ggplot2 ggplot geom_path aes_string theme scale_fill_identity scale_y_continuous scale_x_continuous scale_colour_manual theme_bw coord_cartesian geom_bar
#' 
#' @noRd
.gg_flow <- function(m.df, path_legend, path_legend_title, path_size, val_seq){

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
  
  .lapply(1:max(m.df$frame), function(i, x = m.df, pl = path_legend, plt = path_legend_title, ps = path_size, vs = val_seq){
    gg.fun(x = m.df[m.df$frame <= i,], y = m.df, pl = path_legend, plt = path_legend_title, ps = path_size, vs = val_seq)
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

#' convert units
#' @noRd 
.convert_units <- function(unit){
  unit.c <- c("secs" = "%S", "mins" = "%M", "hours" = "%H", "days" = "%d")
  sub <- match(unit, unit.c)
  if(is.na(sub)){
    sub <- match(unit, names(unit.c))
    if(is.na(sub)) out(paste0("Unit '", unit, "' is not supported."), type = 3) else unit.c[sub]
  }
  return(unit.c[sub])
}

#' detect time gaps
#' @noRd 
.time_conform <- function(m){
  
  m.indi <- if(inherits(m, "MoveStack")) split(m) else list(m)
  ts <- .lapply(m.indi, timestamps, moveVis.verbose = F)
  tl <- .lapply(m.indi, timeLag, unit = "secs", moveVis.verbose = F)
  
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

#' combine two extents into one
#' @noRd 
.combine_ext <- function(ext.both){
  ext.combi <- ext.both[[1]]
  ext.combi@xmin <- min(c(ext.both[[1]]@xmin, ext.both[[2]]@xmin))
  ext.combi@xmax <- max(c(ext.both[[1]]@xmax, ext.both[[2]]@xmax))
  return(ext.combi)
}

#' expand two extents by the one with larger x range
#' @noRd 
.expand_ext <- function(ext.both, rg){
  if(which.min(rg) == 1){
    ext.both[[which.min(rg)]]@xmin <- -180+ext.both[[which.min(rg)]]@xmin-180
    ext.both[[which.min(rg)]]@xmax <- -180+ext.both[[which.min(rg)]]@xmax-180
  } else{
    ext.both[[which.max(rg)]]@xmin <- 180+ext.both[[which.max(rg)]]@xmin+180
    ext.both[[which.max(rg)]]@xmax <- 180+ext.both[[which.max(rg)]]@xmax+180
  }
  return(ext.both)
}

#' shift two extents by 180 lon degrees
#' @noRd 
.shift_ext <- function(ext.both){
  
  shift <- function(x) if(x >= 0) x-180 else x +180
  
  lapply(ext.both, function(x){
    x@xmin <- shift(x@xmin)
    x@xmax <- shift(x@xmax)
    return(x)
  })
}

#' get map
#' @importFrom slippymath bbox_to_tile_grid compose_tile_grid
#' @importFrom raster projectRaster extent extent<- resample extend merge
#' @importFrom magick image_read image_write image_convert
#' @importFrom curl curl_download
#' @noRd 
.getMap <- function(gg.ext, map_service, map_type, map_token, map_dir, map_res, m.crs){
  
  if(inherits(gg.ext, "bbox")) gg.ext <- list(gg.ext)
  
  ## calculate needed slippy tiles using slippymath
  r <- lapply(gg.ext, function(y){
    gg.ext.ll <- st_bbox(st_transform(st_as_sfc(y), crs = st_crs("+init=epsg:4326")))
    tg <- bbox_to_tile_grid(gg.ext.ll, max_tiles = ceiling(map_res*20))
    images <- .apply(tg$tiles, MARGIN = 1, function(x){
      file <- paste0(map_dir, map_service, "_", map_type, "_", x[1], "_", x[2], ".png")
      
      retry <- list(do = TRUE, count = 0)
      while(retry$do){
        
        ## download tiles
        url <- paste0(getOption("moveVis.map_api")[[map_service]][[map_type]], tg$zoom, "/", x[1], "/", x[2], ".png", if(map_service == "mapbox") paste0("?access_token=", map_token) else NULL)
        if(!file.exists(file)) curl_download(url = url, destfile = file) #utils::download.file(url = url, destfile = file, quiet = T) 
        
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
    crop(projectRaster(r, crs = m.crs), extent(y[1], y[3], y[2], y[4]), snap = "out")
  })
  
  if(length(r) > 1){
    
    # extend over dateline
    ext.both <- list(east = extent(r$east), west = extent(r$west))
    rg <- c("east"= diff(c(ext.both$east@xmin, ext.both$east@xmax)), "west" = diff(c(ext.both$west@xmin, ext.both$west@xmax)))
    
    ext.both <- .expand_ext(ext.both, rg)
    #ext.both <- .shift_ext(ext.both)
    extent(r$east) <- ext.both$east
    extent(r$west) <- ext.both$west
    
    # extend lower res raster, resample higher res raster and merge both
    ext.combi <- .combine_ext(ext.both)
    
    r[[which.min(rg)]] <- extend(r[[which.min(rg)]], ext.combi)
    r[[which.max(rg)]] <- resample(r[[which.max(rg)]], r[[which.min(rg)]])
    r <- list(merge(r[[1]], r[[2]]))
  }
  
  return(r)
  #projectRaster produces hidden warnings:
  # no non-missing arguments to max; returning -Inf
  # no non-missing arguments to min; returning -Inf
  # seems to be a bug
}

#' create interpolated layer by frame position
#' @importFrom raster clusterR overlay brick unstack stack
#' @noRd
.int2frames <- function(r_list, pos, frames, n.rlay, cl){
  
  # get frames outside shoulders not to be interpolated
  r.frames <- rep(list(NULL), length(frames))
  names(r.frames) <- frames
  early <- as.numeric(names(r.frames)) < utils::head(pos, n=1)
  if(any(early)) r.frames[early] <- utils::head(r_list, n=1)
  
  late <- as.numeric(names(r.frames)) > utils::tail(pos, n=1)
  if(any(late)) r.frames[late] <- utils::tail(r_list, n=1)
  
  exist <- match(as.numeric(names(r.frames)), pos)
  if(any(!is.na(exist))){
    r.frames[!is.na(exist)] <- r_list[stats::na.omit(exist)]
  }
  
  # collect remaining frame ids
  i.frames <- as.numeric(names(r.frames)[sapply(r.frames, is.null)])
  
  # between which elements
  i.frames <- lapply(2:length(pos), function(i){
    y <- i.frames > pos[i-1] & i.frames < pos[i]
    if(any(y)) return(i.frames[which(y)])
  })
  i.rasters <- which(!sapply(i.frames, is.null))+1
  i.frames <- i.frames[i.rasters-1]
  
  # interpolation function
  v.fun <- function(v.x, v.y, ...) t(mapply(xx = v.x, yy = v.y, FUN = function(xx, yy, ...) zoo::na.approx(c(xx, v.na, yy), rule = 2)[pos.frames], SIMPLIFY = T))
  #v.fun <- function(v.x, v.y) mapply(xx = v.x, yy = v.y, FUN = function(xx, yy, xx.pos = x.pos, yy.pos = y.pos, xy.frame = frame) zoo::na.approx(c(xx, rep(NA, (yy.pos-xx.pos)-1), yy))[(xy.frame-xx.pos)+1], SIMPLIFY = T)
  #v.fun <- Vectorize(function(x, y, ...) zoo::na.approx(c(x, v.na, y), rule = 2)[pos.frame])
  
  # iterate over shoulder ranges
  for(i in i.rasters){
    
    # rasters
    if(n.rlay > 1){
      x <- unstack(r_list[[i-1]])
      y <- unstack(r_list[[i]])
    } else{
      x <- r_list[i-1] # keep listed using [ instead of [[ to work with lapply
      y <- r_list[i]
    }
    
    # positions
    x.pos <- pos[i-1]
    y.pos <- pos[i]
    v.na <- rep(NA, (y.pos-x.pos)-1)
    pos.frames <- (i.frames[[which(i.rasters == i)]]-x.pos)+1
    if(getOption("moveVis.n_cores") > 1) parallel::clusterExport(cl, c("v.na", "pos.frames"), envir = environment())
    
    # interpolate layer-wise
    r <- lapply(1:length(x), function(i.layer){
      if(getOption("moveVis.n_cores") > 1){
        clusterR(stack(x[[i.layer]], y[[i.layer]]), fun = overlay, args = list("fun" = v.fun), cl = cl) # export = c("pos.frames", "v.na"))
      }else overlay(stack(x[[i.layer]], y[[i.layer]]), fun = v.fun)
    })
    
    # disassemble brick time- and layerwise
    if(length(r) > 1){
      for(j in 1:length(i.frames[[which(i.rasters == i)]])){
        r.frames[[match(i.frames[[which(i.rasters == i)]], frames)[j]]] <- brick(lapply(1:n.rlay, function(lay) r[[lay]][[j]]))
      }
    } else{
      r.frames[match(i.frames[[which(i.rasters == i)]], frames)] <- if(inherits(r[[1]], "RasterLayer")) r else unstack(r[[1]])
    }
  }
  return(r.frames)
}


#' assign raster to frames
#' @importFrom raster nlayers crop extent brick writeRaster dataType
#' @noRd
.rFrames <- function(r_list, r_times, m.df, gg.ext, fade_raster = T, crop_raster = T, ...){
  
  if(!is.list(r_list)){
    r_list <- list(r_list)
    n <- 1
  } else n <- length(r_list)
  n.rlay <- nlayers(r_list[[1]])
  
  #if(n.rlay > 1) r_list <- lapply(1:n.rlay, function(i) lapply(r_list, "[[", i)) else r_list <- list(r_list) #FRIDAY
  
  if(isTRUE(crop_raster)){
    r_list <- lapply(r_list, crop, y = extent(gg.ext[1], gg.ext[3], gg.ext[2], gg.ext[4]), snap = "out")
  }
  
  if(n > 1){
    
    ## calcualte time differences to r_times
    x <- lapply(1:max(m.df$frame), function(y) max(unique(m.df[m.df$frame == y,]$time)))
    frame_times <- unlist(x)
    attributes(frame_times) <- attributes(x[[1]])
    diff.df <- as.data.frame(sapply(r_times, function(x) abs(difftime(frame_times, x, units = "secs"))))
    
    ## assign r_list positions per frame times
    pos.df <- data.frame(frame = 1:nrow(diff.df), pos_r = apply(diff.df, MARGIN = 1, which.min))
    
    ## interpolate/extrapolate
    if(isTRUE(fade_raster)){
      pos.df <- pos.df[apply(diff.df[,unique(pos.df[,2])], MARGIN = 2, which.min),]
      
      # start cluster and interpolate over all frames or badge-wise
      if(getOption("moveVis.n_cores") > 1) cl <- parallel::makeCluster(getOption("moveVis.n_cores"))
      if(isFALSE(getOption("moveVis.frames_to_disk"))){
        r_list <- .int2frames(r_list, pos = pos.df$frame, frames = unique(m.df$frame), n.rlay = n.rlay, cl = cl)
      } else{
        
        # create frames badge-wise?
        badges <- unique(c(unlist(sapply(2:length(pos.df$frame), function(i){
          c(seq(if(i == 2) 1 else pos.df$frame[i-1], pos.df$frame[i],
                by = if(is.null(getOption("moveVis.n_memory_frames"))) length(unique(m.df$frame)) else getOption("moveVis.n_memory_frames")),
          pos.df$frame[i])
        }, simplify = F)), max(m.df$frame)))
        
        # write to drive instead of memory
        files <- unlist(sapply(2:length(badges), function(i){
          frames <- if(i == 2) (badges[i-1]):badges[i] else (badges[i-1]+1):badges[i]
          r <- .int2frames(r_list, pos = pos.df$frame, frames = frames, n.rlay = n.rlay, cl = cl)
          y <- paste0(getOption("moveVis.dir_frames"), "/moveVis_frame_", frames, ".tif")
          catch <- sapply(1:length(r), function(j) writeRaster(r[[j]], filename = y[[j]], datatype = dataType(r_list[[1]]), overwrite = T))
          return(y)
        }, USE.NAMES = F))
        
        # link to files
        r_list <- lapply(files, brick)
      }
      if(getOption("moveVis.n_cores") > 1) parallel::stopCluster(cl)
    }else{
      r_list <- r_list[pos.df$pos_r]
    }
  }else{r_list <- r_list}
  return(r_list)
}

#' package attatching
#' @noRd 
.onAttach <- function(...) {
  messages <- c(
    "Do you need help with moveVis? Have a look at the docs on our web page: http://movevis.org/",
    "Find out about new features added to moveVis at http://movevis.org/news/index.html",
    "Find example code and code snippets at http://movevis.org/index.html#examples",
    "Find a collection of moveVis animations created by other users on Twitter: https://twitter.com/schwalbwillmann",
    "Are you missing a feature or did you find a bug? Please open an issue at https://github.com/16eagle/movevis/issues",
    "Read our accompanying open-access paper published in 'Methods in Ecology and Evolution': https://doi.org/10.1111/2041-210X.13374"
  )
  packageStartupMessage(paste(strwrap(sample(messages, 1)), collapse = "\n"))
  }

#' package startup
#' @importFrom pbapply pboptions
#' @noRd 
.onLoad <- function(libname, pkgname){
  pboptions(type = "timer", char = "=", txt.width = getOption("width")-30) # can be changed to "none"
  if(is.null(getOption("moveVis.verbose")))  options(moveVis.verbose = FALSE)
  if(is.null(getOption("moveVis.n_cores")))  options(moveVis.n_cores = 1)
  if(is.null(getOption("moveVis.frames_to_disk")))  options(moveVis.frames_to_disk = FALSE)
  if(is.null(getOption("moveVis.dir_frames"))){
    options(moveVis.dir_frames = paste0(tempdir(), "/moveVis"))
    if(!dir.exists(getOption("moveVis.dir_frames"))) dir.create(getOption("moveVis.dir_frames"))
  }
  
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
                                            terrain_bg = "http://tile.stamen.com/terrain-background/",
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