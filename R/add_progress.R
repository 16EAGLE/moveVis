#' Add progress bar to frames
#'
#' This function adds a progress bar to animation frames created with \code{\link{create_frames}}.
#'
#' @inheritParams add_labels
#' @param title character, frame title.
#' @param subtitle character, frame subtitle.
#' @param caption character, frame caption.
#' @param tag character, frame tag.
#' @param x character, label of the x axis.
#' @param y character, label of the y axis.
#'
#' @return List of frames.
#' @author Jakob Schwalb-Willmann
#'
#' @importFrom ggplot2 geom_line aes ggplot_build expr
#' @importFrom dplyr bind_rows
#'
#' @seealso \link{create_frames}
#' @export

add_progress <- function(frames, colour = "grey", size = 1.8, verbose = TRUE){
  
  ## checks
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  if(!inherits(frames, "list")) out("Argument 'frames' needs to be a list of ggplot objects. See create_frames()).", type = 3)
  if(!all(sapply(frames, function(x) inherits(x, "ggplot")))) out("At least one element of argument 'frames' is not a ggplot object.", type = 3)
  
  if(!inherits(colour, "character")) out("Argument 'colour' needs to be of type 'character'.", type = 3)
  if(!inherits(size, "numeric")) out("Argument 'size' needs to be of type 'numeric'.", type = 3)
  
  gg.xy <- ggplot_build(frames[[1]])$data[[1]]
  # gg.xy <- lapply(ggplot_build(frames[[1]])$data, function(x) cbind.data.frame(x = x$x, xmin = x$xmin, xmax = x$xmax,
  #                                                                              y = x$y, ymin = x$ymin, ymax = x$ymax))
  # gg.xy <- bind_rows(gg.xy[!sapply(gg.xy, is.null)])
  
  progress <- lapply(seq(min(gg.xy$xmin), max(gg.xy$xmax), length.out = length(frames)), function(x, x.min = min(gg.xy$xmin), y = max(gg.xy$ymax)){
    cbind.data.frame(x = c(x.min, x), y = c(y, y))
  })
  
  add_gg(frames, gg = expr(geom_line(aes(x = x, y = y), data = data, colour = colour, size = size)),
         data = progress, colour = colour, size = size)
}