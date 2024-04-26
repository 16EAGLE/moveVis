#' Suppress messages and warnings
#' @noRd
quiet <- function(expr) {
  # return(expr)
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

out <- function(input, type = 1, ll = NULL, msg = FALSE, sign = "", verbose = getOption("moveVis.verbose")) {
  if (is.null(ll)) if (isTRUE(verbose)) ll <- 1 else ll <- 2
  if (type == 2 & ll <= 2) {
    warning(paste0(sign, input), call. = FALSE, immediate. = TRUE)
  } else {
    if (type == 3) {
      stop(input, call. = FALSE)
    } else {
      if (ll == 1) {
        if (msg == FALSE) {
          cat(paste0(sign, input), sep = "\n")
        } else {
          message(paste0(sign, input))
        }
      }
    }
  }
}

#' Outputs animation stats
#'
#' @param n.frames numeric
#' @param fps numeric
#'
#' @importFrom lubridate dseconds
#' @keywords internal
#' @noRd
.stats <- function(n.frames, fps = 25) {
  out(paste0("Approximated animation duration: \u2248 ", as.character(dseconds(n.frames / fps)), " at ", toString(fps), " fps for ", toString(n.frames), " frames"))
}

#' Replace value
#'
#' @param data vector to be replaced by values
#' @param x values which to replace
#' @param y values to replace to
#'
#' @keywords internal
#' @noRd
repl_vals <- function(data, x, y) {
  for (i in 1:length(x)) data[data == x[i]] <- y[i]
  data <- methods::as(data, class(y))
  return(data)
}


#' interpolate NAs
#'
#' @param v vector with NAs to be replaced by interpolated values
#' @param rule see approxfun
#'
#' @keywords internal
#' @noRd
.na.approx <- function(v, rule = 2) {
  if (length(which(!is.na(v))) < 2) {
    return(v)
  } else {
    s <- 1:length(v)
    stats::approx(x = s[!is.na(v)], y = v[!is.na(v)], rule = rule, xout = s)$y
  }
}

#' st_transform but quiet wihtou GDLA-version-dependent warning
#'
#' @importFrom sf st_transform
#' @keywords internal
#' @noRd
.st_transform <- function(...) {
  return(quiet(sf::st_transform(...)))
}

#' verbose lapply
#'
#' @importFrom pbapply pblapply
#' @noRd
.lapply <- function(X, FUN, ..., moveVis.verbose = NULL, moveVis.n_cores = NULL, moveVis.export = NULL) {
  if (is.null(moveVis.verbose)) moveVis.verbose <- getOption("moveVis.verbose")
  if (is.null(moveVis.n_cores)) moveVis.n_cores <- getOption("moveVis.n_cores")

  # with parallelization
  if (moveVis.n_cores > 1) {
    cl <- parallel::makeCluster(moveVis.n_cores)
    if (!is.null(moveVis.export)) parallel::clusterExport(cl, moveVis.export)
    y <- try(parallel::parLapply(cl = cl, X, FUN, ...)) # ensures that cluster is stopped appropriately
    parallel::stopCluster(cl)
    if (inherits(y, "try-error")) out(y, type = 3) else return(y)

    # without parallelization
  } else if (isTRUE(moveVis.verbose)) pblapply(X, FUN, ...) else lapply(X, FUN, ...)
}

#' verbose apply
#'
#' @importFrom pbapply pbapply
#' @noRd
.apply <- function(X, MARGIN, FUN, ...) {
  verbose <- getOption("moveVis.verbose")
  if (isTRUE(verbose)) pbapply(X, MARGIN, FUN, ...) else apply(X, MARGIN, FUN, ...)
}


#' moveVis path standard colours
#' @importFrom grDevices rainbow
#' @noRd
.standard_colours <- function(n) {
  grDevices::rainbow(n)
}

#' returns sf data.frame from move object for internal use
#' @importFrom move n.indiv timestamps trackId
#' @importFrom sf st_transform st_as_sf st_crs st_coordinates
#' @noRd
.m2df <- function(m, path_colours = NA, tail_colour = NA, trace_colour = NA, return_latlon = FALSE) {
  ## create data.frame from m with frame time and colour
  m.df <- cbind(
    as.data.frame(m@coords),
    id = repl_vals(as.character(trackId(m)), unique(as.character(trackId(m))), 1:n.indiv(m)),
    time = timestamps(m),
    time_chr = as.character(timestamps(m)),
    name = as.character(trackId(m))
  )

  # transform if requested
  if (isTRUE(return_latlon)) {
    m.df <- st_transform(st_as_sf(m.df, coords = 1:2, crs = st_crs(m), remove = T), st_crs(4326))
    m.df <- cbind(st_coordinates(m.df), data.frame(
      id = m.df$id,
      time = m.df$time,
      time_chr = m.df$time_chr,
      name = m.df$name
    ))
  }
  colnames(m.df)[1:2] <- c("x", "y")

  m.df$frame <- sapply(m.df$time, function(x) which(sort(unique(m.df$time)) == x))

  ## handle colours, either provided as a field in m or argument or computed randomly
  m.info <- methods::as(m, "data.frame")

  # path colours
  if (all(!is.character(path_colours), !all(is.na(m.info$colour)))) {
    m.df$colour <- as.character(m.info$colour)
  } else {
    if (!is.character(path_colours)) {
      path_colours <- .standard_colours(n.indiv(m))
    }
    m.df$colour <- repl_vals(m.df$id, unique(m.df$id), path_colours[1:n.indiv(m)])
  }


  m.df <- m.df[order(m.df$frame), ]
  m.df$name <- factor(as.character(m.df$name), levels = unique(as.character(m.df$name)))
  return(m.df)
}

#' square it
#' @importFrom sf st_bbox st_as_sfc st_crs st_sfc st_point st_distance
#' @noRd
.equidistant <- function(ext, margin_factor = 1) {
  # lat lon extent
  ext.ll <- st_bbox(.st_transform(st_as_sfc(ext), st_crs(4326)))

  # calculate corner coordinates
  corn <- list(c(ext.ll[1], ext.ll[2]), c(ext.ll[1], ext.ll[4]), c(ext.ll[3], ext.ll[2]), c(ext.ll[3], ext.ll[4]))
  corn <- lapply(corn, function(x) st_sfc(st_point(x), crs = st_crs(4326)))

  # calculate difference and distance
  ax.dist <- as.numeric(c(suppressPackageStartupMessages(st_distance(corn[[1]], corn[[3]])), suppressPackageStartupMessages(st_distance(corn[[1]], corn[[2]]))))
  ax.diff <- c(ext.ll[3] - ext.ll[1], ext.ll[4] - ext.ll[2])

  # add difference to match equal distances
  if (ax.dist[1] < ax.dist[2]) {
    x.devi <- (ax.diff[1] / ax.dist[1]) * ((ax.dist[2] - ax.dist[1]) * margin_factor) / 2
    y.devi <- ((ax.diff[2] / ax.dist[2]) * (ax.dist[2] * margin_factor)) - ax.diff[2]
  } else {
    x.devi <- ((ax.diff[1] / ax.dist[1]) * ax.dist[1]) - ax.diff[1]
    y.devi <- ((ax.diff[2] / ax.dist[2]) * (ax.dist[1] - ax.dist[2]) / 2)
  }
  ext.ll.sq <- st_bbox(c(ext.ll[1] - x.devi, ext.ll[3] + x.devi, ext.ll[2] - y.devi, ext.ll[4] + y.devi), crs = st_crs(4326))

  ## add margin
  if (margin_factor > 1) {
    ax.diff <- c(ext.ll.sq[3] - ext.ll.sq[1], ext.ll.sq[4] - ext.ll.sq[2])
    x.devi <- ((ax.diff[1] * margin_factor) - ax.diff[1])
    y.devi <- ((ax.diff[2] * margin_factor) - ax.diff[2])
    ext.ll.sq <- st_bbox(c(ext.ll[1] - x.devi, ext.ll[3] + x.devi, ext.ll[2] - y.devi, ext.ll[4] + y.devi), crs = st_crs(4326))

    if (ext.ll.sq["xmin"] < -180) ext.ll.sq["xmin"] <- -180
    if (ext.ll.sq["xmax"] > 180) ext.ll.sq["xmax"] <- 180
    if (ext.ll.sq["ymin"] < -90) ext.ll.sq["ymin"] <- -90
    if (ext.ll.sq["ymax"] > 90) ext.ll.sq["ymax"] <- 90
  }
  return(st_bbox(.st_transform(st_as_sfc(ext.ll.sq, crs = st_crs(4326)), st_crs(ext))))
}

#' generate extent, return as latlon
#' @importFrom sf st_as_sf st_transform st_crs st_bbox st_as_sfc st_intersects st_coordinates
#' @noRd
.ext <- function(m.df, m.crs, ext = NULL, margin_factor = 1.1, equidistant = FALSE, cross_dateline = FALSE, return_latlon = FALSE) {
  # this works only with EPSG 4326 for cross_dateline stuff.
  m.df <- st_as_sf(m.df, coords = c("x", "y"), crs = m.crs, remove = F)
  m.df <- st_transform(m.df, st_crs(4326))

  ## calcualte ext
  gg.ext <- st_bbox(m.df)

  if (!is.null(ext)) {
    ext <- st_bbox(st_transform(st_as_sfc(st_bbox(ext, crs = m.crs)), st_crs(4326)))

    if (!quiet(st_intersects(st_as_sfc(ext), st_as_sfc(gg.ext), sparse = F)[1, 1])) out("Argument 'ext' does not overlap with the extent of 'm'.", type = 3)

    gg.ext <- ext
    margin_factor <- 1 # no margin since user extent set
  }

  xy.diff <- if (isTRUE(cross_dateline)) {
    xy <- st_coordinates(m.df)
    c(abs(abs(max(xy[xy[, 1] < 0, 1])) - min(xy[xy[, 1] > 0, 1])), gg.ext[4] - gg.ext[2]) / 2
  } else {
    (gg.ext[3:4] - gg.ext[1:2]) / 2
  }

  # squared equidistant extent or not
  if (isTRUE(cross_dateline)) {
    # split extents for both dateline sides
    gg.ext <- list("west" = gg.ext, "east" = gg.ext)

    # cut extents and add margins to x components
    gg.ext$west[[1]] <- -180 # xmin
    gg.ext$west[[3]] <- max(xy[xy[, 1] < 0, 1]) + xy.diff[1] * (-1 + margin_factor) # xmax
    gg.ext$east[[1]] <- min(xy[xy[, 1] > 0, 1]) - xy.diff[1] * (-1 + margin_factor) # xmin
    gg.ext$east[[3]] <- 180 # xmax

    # add margins to y components
    gg.ext$west[[2]] <- gg.ext$west[[2]] - xy.diff[2] * (-1 + margin_factor) # ymin
    gg.ext$west[[4]] <- gg.ext$west[[4]] + xy.diff[2] * (-1 + margin_factor) # ymax
    gg.ext$east[[2]] <- gg.ext$east[[2]] - xy.diff[2] * (-1 + margin_factor) # ymin
    gg.ext$east[[4]] <- gg.ext$east[[4]] + xy.diff[2] * (-1 + margin_factor) # ymax
  } else {
    # equidistant currently not supported for cross_dateline
    if (isTRUE(equidistant)) {
      gg.ext <- .equidistant(ext = gg.ext, margin_factor = margin_factor)
    } else {
      gg.ext <- st_bbox(c(gg.ext[1:2] - (xy.diff * (-1 + margin_factor)), gg.ext[3:4] + (xy.diff * (-1 + margin_factor))), crs = st_crs(m.df))
    }

    # cut by longlat maximums
    if (isTRUE(st_crs(m.df) == st_crs(4326))) {
      if (gg.ext[1] < -180) gg.ext[1] <- -180
      if (gg.ext[3] > 180) gg.ext[3] <- 180
      if (gg.ext[2] < -90) gg.ext[2] <- -90
      if (gg.ext[4] > 90) gg.ext[4] <- 90
    }
  }

  if (isFALSE(return_latlon)) {
    transform_ext <- function(y) st_bbox(st_transform(st_as_sfc(y), m.crs))
    if (inherits(gg.ext, "list")) {
      gg.ext <- lapply(gg.ext, transform_ext)
    } else {
      gg.ext <- transform_ext(gg.ext)
    }
  }
  return(gg.ext)
}

#' calculate x labels from breaks
#' @noRd
.x_labels <- function(x) {
  x.num <- x

  # remove NAs
  which.na <- is.na(x.num)
  x.num[is.na(x.num)] <- 0

  # shift dateline crossings
  x.num[x.num < -180] <- x.num[x.num < -180] + 360
  x.num[x.num > 180] <- x.num[x.num > 180] - 360

  x <- as.character(abs(x.num))
  x <- paste0('"', x, '"', "*degree")
  x[which.na] <- ""

  # assign Northing/Southing
  x[x.num > 0] <- paste0(x[x.num > 0], "*E")
  x[x.num < 0] <- paste0(x[x.num < 0], "*W")
  return(parse(text = x))
}

#' calculate y labels from breaks
#' @noRd
.y_labels <- function(x) {
  x.num <- x

  # remove NAs
  which.na <- is.na(x.num)
  x.num[is.na(x.num)] <- 0

  x <- as.character(abs(x.num))
  x <- paste0('"', x, '"', "*degree")
  x[which.na] <- ""

  # assign Northing/Southing
  x[x.num > 0] <- paste0(x[x.num > 0], "*N")
  x[x.num < 0] <- paste0(x[x.num < 0], "*S")
  return(parse(text = x))
}


#' create paths data.frame for gg on the fly per frame
#' @importFrom utils tail
#' @noRd
.df4gg <- function(m.df, i, tail_length = 0, path_size = 1, tail_size = 1, tail_colour = "white", trace_show = F, trace_size = tail_size, trace_colour = "grey", path_fade = F) {
  # calc range
  i.range <- seq(i - tail_length, i)
  i.range <- i.range[i.range > 0]

  # extract all rows of frame time range
  paths <- m.df[!is.na(match(m.df$frame, i.range)), ]
  paths <- paths[order(paths$id), ]

  # compute colour ramp from id count
  # paths.colours <- sapply(unique(paths$id), function(x) rev(unique(paths[paths$id == x,]$colour)), simplify = F)
  paths.colours <- sapply(unique(paths$id), function(x) paths[paths$id == x, ]$colour, simplify = F)
  paths.count <- as.vector(table(paths$id))
  diff.max <- max(m.df$frame, na.rm = T) - max(i.range)

  paths$tail_colour <- unlist(mapply(paths.cols = paths.colours, paths.size = paths.count, function(paths.cols, paths.size) {
    if (all(isTRUE(path_fade), diff.max < tail_length, paths.size > diff.max)) {
      n <- diff.max + 1
      v <- rep(tail_colour, (paths.size - (n)))
      paths.cols <- utils::tail(paths.cols, n = n)
    } else {
      n <- paths.size
      v <- NULL
    }

    paths.ramps <- lapply(unique(paths.cols), function(x) {
      f <- grDevices::colorRampPalette(c(x, tail_colour))
      rev(f(n + 4)[1:n])
    })

    c(v, mapply(i = 1:n, i.ramp = repl_vals(paths.cols, unique(paths.cols), 1:length(unique(paths.cols))), function(i, i.ramp) {
      paths.ramps[[i.ramp]][i]
    }, USE.NAMES = F))
  }, SIMPLIFY = F))

  # compute tail size from id count
  paths$tail_size <- unlist(lapply(paths.count, function(paths.size) {
    if (all(isTRUE(path_fade), diff.max < tail_length, paths.size > diff.max)) {
      n <- diff.max + 1
      v <- rep(tail_size, (paths.size - (n)))
    } else {
      n <- paths.size
      v <- NULL
    }
    c(v, seq(tail_size, path_size, length.out = n))
  }))

  paths$trace <- FALSE
  if (all(isTRUE(trace_show))) { # & i > tail_size)){ # isn't rather path_length meant here?

    paths.trace <- m.df[!is.na(match(m.df$frame, 1:(min(i.range)))), ]
    paths.trace$colour <- paths.trace$tail_colour <- trace_colour
    paths.trace$tail_size <- trace_size
    paths.trace$trace <- TRUE

    # join trace, reorder by frame and group by id
    paths <- rbind(paths, paths.trace)
    paths <- paths[order(paths$frame), ]
    paths <- paths[order(paths$id), ]
  }
  return(paths)
}

#' detect time gaps
#' @noRd
.time_conform <- function(m) {
  m.indi <- if (inherits(m, "MoveStack")) move::split(m) else list(m)
  ts <- .lapply(m.indi, timestamps, moveVis.verbose = F)
  tl <- .lapply(m.indi, timeLag, unit = "secs", moveVis.verbose = F)

  ## check time lag
  uni.lag <- length(unique(unlist(tl))) <= 1
  if (!isTRUE(uni.lag)) out("The temporal resolution of 'm' is diverging. Use align_move() to align movement data to a uniform time scale with a consistent temporal resolution.", type = 3)

  ## check temporal consistence per individual (consider to remove, if NA timestamps should be allowed)
  uni.intra <- mapply(x = tl, y = ts, function(x, y) length(c(min(y, na.rm = T), min(y, na.rm = T) + cumsum(x))) == length(y))
  if (!all(uni.intra)) out("For at least one movement track, variating time lags have been detected. Use align_move() to align movement data to a uniform time scale with a consistent temporal resolution.", type = 3)

  ## check overall consistence of timestamps
  ts.art <- seq.POSIXt(min(do.call(c, ts), na.rm = T), max(do.call(c, ts), na.rm = T), by = unique(unlist(tl)))
  uni.all <- all(sapply(unique(timestamps(m)), function(x, ta = ts.art) x %in% ta))
  if (!isTRUE(uni.all)) out("For at least one movement track, timestamps diverging from those of the other tracks have been detected. Use align_move() to align movement data to a uniform time scale with a consistent temporal resolution.", type = 3)

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

#' create interpolated layer by frame position
#' @importFrom raster clusterR overlay brick unstack stack
#' @importFrom utils tail head
#' @noRd
.int2frames <- function(r_list, pos, frames, n.rlay, cl) {
  # get frames outside shoulders not to be interpolated
  r.frames <- rep(list(NULL), length(frames))
  names(r.frames) <- frames
  early <- as.numeric(names(r.frames)) < utils::head(pos, n = 1)
  if (any(early)) r.frames[early] <- utils::head(r_list, n = 1)

  late <- as.numeric(names(r.frames)) > utils::tail(pos, n = 1)
  if (any(late)) r.frames[late] <- utils::tail(r_list, n = 1)

  exist <- match(as.numeric(names(r.frames)), pos)
  if (any(!is.na(exist))) {
    r.frames[!is.na(exist)] <- r_list[stats::na.omit(exist)]
  }

  # collect remaining frame ids
  i.frames <- as.numeric(names(r.frames)[sapply(r.frames, is.null)])

  # between which elements
  i.frames <- lapply(2:length(pos), function(i) {
    y <- i.frames > pos[i - 1] & i.frames < pos[i]
    if (any(y)) {
      return(i.frames[which(y)])
    }
  })
  i.rasters <- which(!sapply(i.frames, is.null)) + 1
  i.frames <- i.frames[i.rasters - 1]

  # interpolation function
  v.fun <- function(v.x, v.y, ...) t(mapply(xx = v.x, yy = v.y, FUN = function(xx, yy, ...) .na.approx(c(xx, v.na, yy))[pos.frames], SIMPLIFY = T))
  # v.fun <- function(v.x, v.y, ...) t(mapply(xx = v.x, yy = v.y, FUN = function(xx, yy, ...) zoo::na.approx(c(xx, v.na, yy), rule = 2)[pos.frames], SIMPLIFY = T))
  # v.fun <- function(v.x, v.y) mapply(xx = v.x, yy = v.y, FUN = function(xx, yy, xx.pos = x.pos, yy.pos = y.pos, xy.frame = frame) zoo::na.approx(c(xx, rep(NA, (yy.pos-xx.pos)-1), yy))[(xy.frame-xx.pos)+1], SIMPLIFY = T)
  # v.fun <- Vectorize(function(x, y, ...) zoo::na.approx(c(x, v.na, y), rule = 2)[pos.frame])

  # iterate over shoulder ranges
  for (i in i.rasters) {
    # rasters
    if (n.rlay > 1) {
      x <- unstack(r_list[[i - 1]])
      y <- unstack(r_list[[i]])
    } else {
      x <- r_list[i - 1] # keep listed using [ instead of [[ to work with lapply
      y <- r_list[i]
    }

    # positions
    x.pos <- pos[i - 1]
    y.pos <- pos[i]
    v.na <- rep(NA, (y.pos - x.pos) - 1)
    pos.frames <- (i.frames[[which(i.rasters == i)]] - x.pos) + 1
    if (getOption("moveVis.n_cores") > 1) parallel::clusterExport(cl, c("v.na", "pos.frames"), envir = environment())

    # interpolate layer-wise
    r <- lapply(1:length(x), function(i.layer) {
      if (getOption("moveVis.n_cores") > 1) {
        clusterR(stack(x[[i.layer]], y[[i.layer]]), fun = overlay, args = list("fun" = v.fun), cl = cl) # export = c("pos.frames", "v.na"))
      } else {
        overlay(stack(x[[i.layer]], y[[i.layer]]), fun = v.fun)
      }
    })

    # disassemble brick time- and layerwise
    if (length(r) > 1) {
      for (j in 1:length(i.frames[[which(i.rasters == i)]])) {
        r.frames[[match(i.frames[[which(i.rasters == i)]], frames)[j]]] <- brick(lapply(1:n.rlay, function(lay) r[[lay]][[j]]))
      }
    } else {
      r.frames[match(i.frames[[which(i.rasters == i)]], frames)] <- if (inherits(r[[1]], "RasterLayer")) r else unstack(r[[1]])
    }
  }
  return(r.frames)
}


#' assign raster to frames
#' @importFrom raster nlayers crop extent brick writeRaster dataType
#' @noRd
.rFrames <- function(r_list, r_times, m.df, gg.ext, fade_raster = T, crop_raster = T, ...) {
  if (!is.list(r_list)) {
    r_list <- list(r_list)
    n <- 1
  } else {
    n <- length(r_list)
  }
  n.rlay <- terra::nlyr(r_list[[1]])

  # if(n.rlay > 1) r_list <- lapply(1:n.rlay, function(i) lapply(r_list, "[[", i)) else r_list <- list(r_list) #FRIDAY

  if (isTRUE(crop_raster)) {
    r_list <- lapply(r_list, terra::crop, y = extent(gg.ext[1], gg.ext[3], gg.ext[2], gg.ext[4]), snap = "out")
  }

  if (n > 1) {
    ## calcualte time differences to r_times
    x <- lapply(1:max(m.df$frame), function(y) max(unique(m.df[m.df$frame == y, ]$time)))
    frame_times <- unlist(x)
    attributes(frame_times) <- attributes(x[[1]])
    diff.df <- as.data.frame(sapply(r_times, function(x) abs(difftime(frame_times, x, units = "secs"))))

    ## assign r_list positions per frame times
    pos.df <- data.frame(frame = 1:nrow(diff.df), pos_r = apply(diff.df, MARGIN = 1, which.min))

    ## interpolate/extrapolate
    if (isTRUE(fade_raster)) {
      pos.df <- pos.df[apply(diff.df[, unique(pos.df[, 2])], MARGIN = 2, which.min), ]

      # start cluster and interpolate over all frames or badge-wise
      if (getOption("moveVis.n_cores") > 1) cl <- parallel::makeCluster(getOption("moveVis.n_cores"))
      if (isFALSE(getOption("moveVis.frames_to_disk"))) {
        r_list <- .int2frames(r_list, pos = pos.df$frame, frames = unique(m.df$frame), n.rlay = n.rlay, cl = cl)
      } else {
        # create frames badge-wise?
        badges <- unique(c(unlist(sapply(2:length(pos.df$frame), function(i) {
          c(
            seq(if (i == 2) 1 else pos.df$frame[i - 1], pos.df$frame[i],
              by = if (is.null(getOption("moveVis.n_memory_frames"))) length(unique(m.df$frame)) else getOption("moveVis.n_memory_frames")
            ),
            pos.df$frame[i]
          )
        }, simplify = F)), max(m.df$frame)))

        # write to drive instead of memory
        files <- unlist(sapply(2:length(badges), function(i) {
          frames <- if (i == 2) (badges[i - 1]):badges[i] else (badges[i - 1] + 1):badges[i]
          r <- .int2frames(r_list, pos = pos.df$frame, frames = frames, n.rlay = n.rlay, cl = cl)
          y <- paste0(getOption("moveVis.dir_frames"), "/moveVis_frame_", frames, ".tif")
          catch <- sapply(1:length(r), function(j) quiet(writeRaster(r[[j]], filename = y[[j]], datatype = dataType(r_list[[1]]), overwrite = T)))
          return(y)
        }, simplify = F, USE.NAMES = F))

        # link to files
        r_list <- lapply(files, brick)
      }
      if (getOption("moveVis.n_cores") > 1) parallel::stopCluster(cl)
    } else {
      r_list <- r_list[pos.df$pos_r]
    }
  } else {
    r_list <- r_list
  }
  return(r_list)
}

#' frame plotting function
#' @importFrom ggplot2 geom_path aes_string theme scale_colour_manual theme_bw guides guide_legend geom_point
#' @noRd
gg.spatial <- function(x, y, m_names, m_colour, path_end, path_join, path_mitre, path_arrow, path_alpha, path_legend, path_legend_title, path_size, equidistant, tail_length) {
  ## scale plot to extent and set na.rm to TRUE to avoid warnings
  y$layers[[1]]$geom_params$na.rm <- T

  x_path <- x[!x$trace, ]
  x_trace <- x[x$trace, ]

  ## trace plot
  if (nrow(x_trace) > 1) {
    p <- y + geom_path(
      data = x_trace, aes_string(x = "x", y = "y", group = "id"), size = x_trace$tail_size, lineend = path_end, linejoin = path_join,
      linemitre = path_mitre, arrow = path_arrow, colour = x_trace$tail_colour, alpha = path_alpha, na.rm = T
    )
  } else {
    p <- y
  }

  ## base plot
  if (tail_length == 0) {
    p <- p + geom_point(
      data = x_path, aes_string(x = "x", y = "y", group = "id"), size = x_path$tail_size,
      colour = x_path$tail_colour, alpha = path_alpha, na.rm = T
    )
  } else {
    p <- p + geom_path(
      data = x_path, aes_string(x = "x", y = "y", group = "id"), size = x_path$tail_size, lineend = path_end, linejoin = path_join,
      linemitre = path_mitre, arrow = path_arrow, colour = x_path$tail_colour, alpha = path_alpha, na.rm = T
    )
  }
  p <- p + theme_bw() + x$coord[[1]] + x$scalex[[1]] + x$scaley[[1]]

  ## add legend?
  if (isTRUE(path_legend)) {
    l.df <- cbind.data.frame(
      x = x[1, ]$x, y = x[1, ]$y, name = levels(m_names),
      colour = as.character(m_colour[sapply(as.character(unique(m_names)), function(x) match(x, m_names)[1])]), stringsAsFactors = F
    )
    l.df$name <- factor(l.df$name, levels = l.df$name)
    l.df <- rbind(l.df, l.df)

    p <- p + geom_path(data = l.df, aes_string(x = "x", y = "y", colour = "name", linetype = NA), size = path_size, na.rm = TRUE) + scale_colour_manual(values = as.character(l.df$colour), name = path_legend_title) + guides(color = guide_legend(order = 1))
  }

  if (isTRUE(equidistant)) p <- p + theme(aspect.ratio = 1)
  return(p)
}

#' flow stats plot function
#' @importFrom ggplot2 ggplot geom_path aes_string theme scale_fill_identity scale_y_continuous scale_x_continuous scale_colour_manual theme_bw coord_cartesian geom_bar
#'
#' @noRd
.gg_flow <- function(x, y, path_legend, path_legend_title, path_size, val_seq) {
  ## generate base plot
  p <- ggplot(x, aes_string(x = "frame", y = "value")) +
    geom_path(aes_string(group = "id"), size = path_size, show.legend = F, colour = x$colour) +
    coord_cartesian(xlim = c(0, max(y$frame, na.rm = T)), ylim = c(min(val_seq, na.rm = T), max(val_seq, na.rm = T))) +
    theme_bw() +
    theme(aspect.ratio = 1) +
    scale_y_continuous(expand = c(0, 0), breaks = val_seq) +
    scale_x_continuous(expand = c(0, 0))

  ## add legend
  if (isTRUE(path_legend)) {
    l.df <- cbind.data.frame(
      frame = x[1, ]$frame, value = x[1, ]$value, name = levels(y$name),
      colour = as.character(y$colour[sapply(as.character(unique(y$name)), function(x) match(x, y$name)[1])]), stringsAsFactors = F
    )
    l.df$name <- factor(l.df$name, levels = l.df$name)
    l.df <- rbind(l.df, l.df)
    p <- p + geom_path(data = l.df, aes_string(x = "frame", y = "value", colour = "name", linetype = NA), size = path_size, na.rm = TRUE) + scale_colour_manual(values = as.character(l.df$colour), name = path_legend_title)
  }
  return(p)
}


#' hist stats plot function
#' @importFrom ggplot2 ggplot geom_path aes_string theme scale_fill_identity scale_y_continuous scale_x_continuous scale_colour_manual theme_bw  coord_cartesian geom_bar
#' @noRd
## stats plot function
.gg_hist <- function(x, y, path_legend, path_legend_title, path_size, val_seq, r_type) {
  ## generate base plot
  if (r_type == "gradient") {
    p <- ggplot(x, aes_string(x = "value", y = "count")) +
      geom_path(aes_string(group = "name"), size = path_size, show.legend = F, colour = x$colour)
  }
  if (r_type == "discrete") {
    p <- ggplot(x, aes_string(x = "value", y = "count", fill = "colour")) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_identity()
  }

  p <- p + coord_cartesian(xlim = c(min(val_seq, na.rm = T), max(val_seq, na.rm = T)), ylim = c(min(y$count, na.rm = T), max(y$count, na.rm = T))) +
    theme_bw() + theme(aspect.ratio = 1) + scale_y_continuous(expand = c(0, 0)) + scale_x_continuous(expand = c(0, 0), breaks = val_seq)

  ## add legend
  if (isTRUE(path_legend)) {
    l.df <- cbind.data.frame(
      value = x[1, ]$value, count = x[1, ]$count, name = levels(y$name),
      colour = as.character(y$colour[sapply(as.character(unique(y$name)), function(x) match(x, y$name)[1])]), stringsAsFactors = F
    )
    l.df$name <- factor(l.df$name, levels = l.df$name)
    l.df <- rbind(l.df, l.df)
    p <- p + geom_path(data = l.df, aes_string(x = "value", y = "count", colour = "name", linetype = NA), size = path_size, na.rm = TRUE) + scale_colour_manual(values = as.character(l.df$colour), name = path_legend_title)
  }
  return(p)
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
.onLoad <- function(libname, pkgname) {
  catch <- try(lwgeom::lwgeom_extSoftVersion()) # lwgeom is needed for st_distance, but no import of sf.
  # Importing an lwgeom function here to avoid NOTE on missing imports

  pbapply::pboptions(type = "timer", char = "=", txt.width = getOption("width") - 30) # can be changed to "none"
  if (is.null(getOption("moveVis.verbose"))) options(moveVis.verbose = FALSE)
  if (is.null(getOption("moveVis.n_cores"))) options(moveVis.n_cores = 1)
  if (is.null(getOption("moveVis.frames_to_disk"))) options(moveVis.frames_to_disk = FALSE)
  if (is.null(getOption("moveVis.dir_frames"))) {
    options(moveVis.dir_frames = paste0(tempdir(), "/moveVis"))
    if (!dir.exists(getOption("moveVis.dir_frames"))) dir.create(getOption("moveVis.dir_frames"))
  }
}
