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
#' 
#' @export
print.moveVis <- function(x, ...) {
  if(inherits(x, "frames_spatial")){
    #crs_params <- sf:::crs_parameters(st_crs(x$m))
    
    cat(paste0("moveVis frames (spatial)"))
    .stats(n.frames = length(unique(x$m$frame)), lead_text = ", ", return_char = F)
    cat(paste0("Temporal extent:  ", paste0(min(x$m$time), " to ", max(x$m$time), "\n")))
    cat(paste0("Spatial extent:   ", paste0(mapply(x = names(x$aesthetics$gg.ext), y = x$aesthetics$gg.ext, function(x, y) paste0(x, ": ", round(y, digits = 5)), USE.NAMES = F), collapse = "; "), "\n"))
    .cat_crs_params(.crs_params(x$m))
    cat(paste0("Basemap:          ", if(x$aesthetics$map_service != "custom") paste0("'", x$aesthetics$map_type, "' from '", x$aesthetics$map_service, "'") else paste0("SpatRaster*: ", x$aesthetics$n_r, " user-supplied raster(s)", if(x$aesthetics$fade_raster) ", interpolated" else ", not interpolated"), "\n"))
    cat(paste0("Names:            '", paste0(unique(x$m$name), collapse = "', '"), "'\n"))
  }
  
  if(inherits(x, "frames_graph")){
    cat(paste0("moveVis frames (graph)"))
    .stats(n.frames = length(unique(x$m$frame)), lead_text = ", ", return_char = F)
    cat(paste0("Temporal extent:  ", paste0(min(x$m$time), " to ", max(x$m$time), "\n")))
    cat(paste0("Raster type:      ", x$aesthetics$r_type, "\n"))
    cat(paste0("Names:            '", paste0(unique(x$m$name), collapse = "', '"), "'\n"))
    #cat(paste0("added function:   ", length(frames$additions), "\n"))
  }
  
  if(inherits(x, "frames_joined")){
    cat(paste0("moveVis frames (joined)"))
    .stats(n.frames = length(unique(x$frames_list[[1]]$m$frame)), lead_text = ", ", return_char = F)
    cat(paste0("Temporal extent:  ", paste0(min(x$frames_list[[1]]$m$time), " to ", max(x$frames_list[[1]]$m$time), "\n")))
    cat(paste0("Raster type:      ", x$frames_list[[1]]$aesthetics$r_type, "\n"))
    cat(paste0("Names:            '", paste0(unique(x$frames_list[[1]]$m$name), collapse = "', '"), "'\n"))
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
    length(x$frames_list[[1]])
  }else{
    max(x$m$frame)
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
  
  # subsetting
  .sub <- function(x, i){
    sub <- apply(sapply(i, function(j) x$m$frame == j), MARGIN = 1, any)
    
    x$m <- x$m[sub,]
    if(length(x$r) > 1) x$r <- x$r[which(sub)]
    if(inherits(x$r, "SpatRaster")) x$r <- terra::sds(x$r)
    return(x)
  }
  
  if(inherits(x, "frames_joined")){
    x$frames_list <- lapply(x$frames_list, function(x) x[i])
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