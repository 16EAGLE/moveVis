#' Add \code{ggplot2} function to frames
#'
#' This function adds \code{ggplot2} functions (e.g. to add layers, change scales etc.) to the animation frames created with \code{\link{frames_spatial}}.
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
#' @return List of frames.
#' @author Jakob Schwalb-Willmann
#' 
#'
#' @seealso \link{frames_spatial}
#' @export

add_gg <- function(frames, gg, data = NULL, ..., verbose = T){
  
  ## check data and replicate if necessary
  if(is.list(data)){
    if(length(data) != length(frames)) out("Argument 'data' is a list und thus must be of same length as 'frames'.", type = 3)
  } else{
    data <- rep(list(data), length(frames))
  }
  
  ## gg is not a list, make it one
  if(inherits(gg, "list")){
    if(length(gg) != length(frames)) out("Argument 'gg' is a list und thus must be of same length as 'frames'.", type = 3)
  } else{
    if(length(gg) != length(frames)) gg <- rep(list(gg), length(frames))
  }
  if(!is.call(gg[[1]])) out("Argument 'gg' must be an expression or a list of expressions (see ?moveVis::add_gg and ?ggplot2::expr).", type = 3)
  
  mapply(.frame = frames, .gg = gg, data = data, function(.frame, .gg, data, arg = list(...)){
    if(length(arg) > 0) for(i in 1:length(arg)) assign(names(arg)[[i]], arg[[i]])
    return(.frame + eval(.gg)) #parse(text = paste0(y, collapse = " + ")))
  }, USE.NAMES = F, SIMPLIFY = F)
}
