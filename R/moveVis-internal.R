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
  lapply(1:(max(m.df$frame)-tail_length), function(i){
    
    # extract all rows of frame time range
    y <- m.df[!is.na(match(m.df$frame,i:(i+tail_length))),]
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
.gg <- function(m.split, ggbmap, path_size = 3, path_end = "round", path_join = "round", squared = T, 
                path_mitre = 10, path_arrow = NULL, print_plot = T){
  lapply(m.split, function(x){
    
    ## plot frame
    p <- ggbmap + geom_path(aes(x = x, y = y, group = id), data = x, size = x$tail_size, lineend = path_end, linejoin = path_join,
                            linemitre = path_mitre, arrow = path_arrow, colour = x$tail_colour) + 
                            scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0))
    if(isTRUE(squared)) p <- p + theme(aspect.ratio = 1)
    if(isTRUE(print_plot)) print(p) else return(p)
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
.time_gaps <- function(m){
  ts.digits <- lapply(c("secs", "mins", "hours", "days"), function(x, ts = timestamps(m)) sort(unique(as.numeric(format(unique(timestamps(m)), .convert_units(x))))))
  ts.dl <- lapply(ts.digits, function(x) length(unique(diff(x))))
  sapply(ts.dl, function(x) x > 1)
}

#' package startup
#' @noRd 
.onLoad <- function(libname, pkgname){
  if(is.null(getOption("moveVis.verbose")))  options(moveVis.verbose = FALSE)
}