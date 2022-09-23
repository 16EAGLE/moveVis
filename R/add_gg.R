#' Add \code{ggplot2} function to frames
#'
#' This function adds \code{ggplot2} functions (e.g. to add layers, change scales etc.) to the animation frames created with \code{\link{frames_spatial}} or \code{\link{frames_graph}}.
#'
#' @inheritParams add_labels
#' @param gg \code{ggplot2} expressions (see details), either as
#' \itemize{
#'   \item an expression of one or a list of \code{ggplot2} functions to be added to every frame,
#'   \item a list of such of the same length as \code{frames} to add different \code{ggplot2} expressions per frame
#' }
#' @param data optional data used by \code{gg} (see details), either
#'  \itemize{
#'   \item an object of any class, e.g. a \code{data.frame}, used by \code{gg} that will be added to all frames,
#'   \item a list, e.g. of multiple \code{data.frames}, with length of \code{frames} to add different data to each frame.
#' }
#' @param ... additional (non-iterated) objects that should be visible to \code{gg}.
#'
#' @details 
#' Agrument \code{gg} expects \code{ggplot2} functions handed over as expressions (see \code{\link{expr}}) to avoid their evaluation
#' before thex are called for the correct frame. Simply wrap your \code{ggplot2} function into \code{expr()} and supply it to 
#' \code{gg}. To add multiple \code{ggplot2} functions to be applied on every frame, supply an expression containing a list of 
#' \code{ggplot2} functions (e.g. \code{expr(list(geom_label(...), geom_text(...)))}). This expression would be added to all frames. 
#' To add specific \code{ggplot2} functions per frame, supply a list of expressions of the same length as frames. Each expression may
#' contain a list of \code{ggplot2} functions, if you want to add multiple functions per frame.
#' 
#' If \code{data} is used, the \code{ggplot2} expressions supplied with \code{gg} can use the object by the name \code{data} for plotting. 
#' If \code{data} is a list, it must be of the same length as \code{frames}. The list will be iterated, so that functions in \code{gg}
#' will have access to the individual objects within the list by the name \code{data} per each frame. If the data you want to display
#' is does not change with frames and may only be a character vector or similiar, you may not need \code{data}, as you can supply
#' the needed values within the expression supplied through \code{gg}.
#' 
#' If you supply \code{gg} as a list of expressions for each frame and \code{data} as a list of objects (e.g. data.frames) for each frame,
#' each frame will be manipulated with the corresponding \code{ggplot2} function and the corresponding data. 
#'
#' @return A frames object of class \code{moveVis}.
#' @author Jakob Schwalb-Willmann
#' 
#' @examples
#' library(moveVis)
#' library(move)
#' library(ggplot2)
#' 
#' data("move_data", "basemap_data")
#' # align movement
#' m <- align_move(move_data, res = 4, unit = "mins")
#' 
#' \dontrun{
#' frames <- frames_spatial(m, map_service = "osm", map_type = "watercolor")
#' frames[[100]] # take a look at one of the frames
#' 
#' # let's draw a polygon on frames:
#' data <- data.frame(x = c(8.917, 8.924, 8.924, 8.916, 8.917),
#'                    y = c(47.7678, 47.7675, 47.764, 47.7646, 47.7678))
#'                    
#' frames = add_gg(frames, gg = expr(geom_path(aes(x = x, y = y), data = data,
#'                                             colour = "red", linetype = "dashed")), data = data)
#'                         
#' # add some text
#' frames <- add_text(frames, "Static feature", x = 8.9205, y = 47.7633,
#'                     colour = "black", size = 3)
#' frames[[100]]
#' 
#' # add_gg can also be used iteratively to manipulate each frame differently.
#' # Let's create unique polygons per frame:
#' 
#' # create data.frame containing corner coordinates
#' data <- data.frame(x = c(8.96, 8.955, 8.959, 8.963, 8.968, 8.963, 8.96),
#'                    y = c(47.725, 47.728, 47.729, 47.728, 47.725, 47.723, 47.725))
#' # make a list from it by replicating it by the length of frames
#' data <- rep(list(data), length.out = length(frames))
#' 
#' # now alter the coordinates to make them shift
#' data <- lapply(data, function(x){
#'   y <- rnorm(nrow(x)-1, mean = 0.00001, sd = 0.0001) 
#'   x + c(y, y[1])
#' })
#' 
#' # draw each individual polygon to each frame
#' frames = add_gg(frames, gg = expr(geom_path(aes(x = x, y = y), data = data,
#'                                             colour = "black")), data = data)
#'                                             
#' # add a text label
#' frames <- add_text(frames, "Dynamic feature", x = 8.959, y = 47.7305,
#'                    colour = "black", size = 3)
#' frames[[100]]
#' 
#' # animate frames to see how the polygons "flip"
#' animate_frames(frames, out_file = tempfile(fileext = ".mov"))
#' 
#' # you can use add_gg on any list of ggplot2 objects,
#' # also on frames made using frames_gr
#' r_list <- basemap_data[[1]]
#' r_times <- basemap_data[[2]]
#' 
#' frames.gr <- frames_graph(m, r_list = r_list, r_times = r_times, r_type = "gradient",
#'                           fade_raster = TRUE, graph_type = "hist", val_by = 0.01)
#' frames.gr[[100]]                         
#' # manipulate the labels, since they are very dense:
#' # just replace the current scale
#' frames.gr <- add_gg(frames.gr, expr(scale_x_continuous(breaks=seq(0,1,0.1),
#'                                     labels=seq(0,1,0.1), expand = c(0,0))))
#' frames.gr[[100]]
#' }
#' 
#' @seealso \code{\link{frames_spatial}} \code{\link{frames_graph}} \code{\link{animate_frames}}
#' @export

add_gg <- function(frames, gg, data = NULL, ..., verbose = T){
  
  ## check data and replicate if necessary
  if(inherits(data, "list")){
    if(length(data) != length(frames)) out("Argument 'data' is a list und thus must be of same length as 'frames'.", type = 3)
  } else{
    if(!is.null(data)) data <- rep(list(data), length(frames))
  }
  
  ## gg is not a list, make it one
  if(inherits(gg, "list")){
    if(length(gg) != length(frames)) out("Argument 'gg' is a list und thus must be of same length as 'frames'.", type = 3)
  } else{
    if(length(gg) != length(frames)) gg <- rep(list(gg), length(frames))
  }
  if(!is.call(gg[[1]])) out("Argument 'gg' must be an expression or a list of expressions (see ?moveVis::add_gg and ?ggplot2::expr).", type = 3)
  
  if(is.null(frames$additions)) frames$additions <- list(list(expr = gg, data = data, arg = list(...))) else{
    frames$additions <- c(frames$additions, list(list(expr = gg, data = data, arg = list(...))))
  }
  return(frames)
}
