#' Print moveVis frames
#' 
#' Method for printing \code{moveVis} frames. Prints show basic information about the object, including number of frames, extent and more.
#' 
#' @param x an object of class \code{moveVis}.
#' @param ... further arguments passed to or from other methods.
#' 
#' @return 
#' Invisible, used for its side effect.
#' 
#' @rdname print
#' @export
print.moveVis <- function(x, ...) {
  if(inherits(x, "frames_spatial")){
    cat(paste0("Spatial frames of class moveVis\n"))
    cat(paste0("number of frames: ", as.character(length(x)), "\n"))
    cat(paste0("temporal extent:  ", paste0(x$move_data$time_chr[1], "' to '", x$move_data$time_chr[nrow(x$move_data)]), "\n"))
    cat(paste0("spatial extent:   ", paste0(mapply(x = names(x$aesthetics$gg.ext), y = x$aesthetics$gg.ext, function(x, y) paste0(x, ": ", round(y, digits = 5)), USE.NAMES = F), collapse = "; "), "\n"))
    cat(paste0("raster type:      ", x$aesthetics$r_type, "\n"))
    cat(paste0("basemap:          ", if(x$aesthetics$map_service != "custom") paste0("'", x$aesthetics$map_type, "' from '", x$aesthetics$map_service, "'") else "custom", "\n"))
    cat(paste0("names:            '", paste0(unique(x$move_data$name), collapse = "', '"), "'\n"))
    #cat(paste0("added function:   ", length(x$additions), "\n"))
  }
  
  if(inherits(x, "frames_graph")){
    cat(paste0("Graph frames of class moveVis\n"))
    cat(paste0("number of frames: ", as.character(length(x)), "\n"))
    cat(paste0("temporal extent:  ", paste0(x$move_data$time_chr[1], "' to '", x$move_data$time_chr[nrow(x$move_data)]), "\n"))
    cat(paste0("raster type:      ", x$aesthetics$r_type, "\n"))
    cat(paste0("names:            '", paste0(unique(x$move_data$name), collapse = "', '"), "'\n"))
    #cat(paste0("added function:   ", length(frames$additions), "\n"))
  }
  
  if(inherits(x, "frames_joined")){
    cat(paste0("Joined frames of class moveVis\n"))
    cat(paste0("number of frames: ", as.character(length(x)), "\n"))
    cat(paste0("temporal extent:  -"))
    cat(paste0("raster type:      -"))
    cat(paste0("names:            '", paste0(unique(x$move_data$name), collapse = "', '"), "'\n"))
  }
}

#' Length of moveVis frames
#' 
#' Method to get length of \code{moveVis} frames, i.e. number of frames.
#' 
#' @inheritParams print.moveVis
#' 
#' @return 
#' Numeric
#' 
#' @rdname length
#' @export
length.moveVis <- function(x){
  if(inherits(x, "frames_joined")){
    length(x$frames_lists[[1]])
  }else{
    length(unique(x$move_data$frame))
  }
}

#' Combining moveVis frames
#' 
#' Method for combining multiple \code{moveVis} frames objects.
#' 
#' @param ... two or more objects of class \code{moveVis}.
#' 
#' @return 
#' A list of \code{moveVis} frames objects.
#' 
#' @rdname c
#' @export
c.moveVis <- function(...){
  frames <- list(...)
  return(frames)
}


# head method
#' @rdname head
#' @importFrom utils tail
#' @export
tail.moveVis <- function(x, n = 6L, ...){
  x[utils::tail(1:length(x), n, ...)]
}


#' Return first or last frames of an moveVis frames object
#' 
#' Method for returning \code{n} last or first frames of a \code{moveVis} frames objects.
#' 
#' @inheritParams print.moveVis
#' @param n an integer of length up to \code{length(x)}.
#' 
#' @return 
#' A \code{moveVis} frames object.
#' 
#' @rdname head
#' @importFrom utils head
#' @export
head.moveVis <- function(x, n = 6L, ...){
  x[utils::head(1:length(x), n, ...)]
}

#' Reverse moveVis frames
#' 
#' Method for reversing the order of frames in a \code{moveVis} frames object.
#' 
#' @inheritParams print.moveVis
#' 
#' @return 
#' A \code{moveVis} frames object.
#' 
#' @rdname rev
#' @export
rev.moveVis <- function(x){
  x[rev(1:length(x))]
}

#' Extract moveVis frames
#' 
#' Method for extracting individual frames or a sequence of frames from a \code{moveVis} frames object.
#' 
#' @inheritParams print.moveVis
#' @param i numeric, index number or sequence of index numbers of the frame(s) to be extracted.
#' 
#' @return 
#' A \code{moveVis} frames object.
#' 
#' @rdname Extract
#' @export
"[.moveVis" <- function(x, i, ...) {
  bounds <- sapply(i, function(j) any(j < 1, j > length(x)))
  if(all(bounds)) stop(paste0("Subscript out of bounds. Length of frames is ", length(x), "."), call. = FALSE)
  if(any(bounds)) warning(paste0("Subscript extends beyond bounds and is thus truncated. Length of frames is ", length(x), "."), call. = FALSE, immediate. = FALSE)
  i <- i[!bounds]
  
  # seubsetting
  .sub <- function(x, i){
    sub <- apply(sapply(i, function(j) x$move_data$frame == j), MARGIN = 1, any)
    
    x$move_data <- x$move_data[sub,]
    if(length(x$raster_data) > 1) x$raster_data <- x$raster_data[sub]
    return(x)
  }
  
  if(inherits(x, "frames_joined")){
    x$frames_lists <- lapply(x$frames_lists, function(x) x[i])
  }else{
    x <- .sub(x, i)
  }  
  return(x)
}

# render methods
#' @rdname render_frame
#' @export
"[[.moveVis" <- function(x, i, ...) {
  quiet(render_frame(x, i))
}