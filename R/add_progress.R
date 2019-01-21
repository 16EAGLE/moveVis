#' Add progress bar to frames
#'
#' This function adds a progress bar to to animation frames created with \code{\link{create_frames}}.
#'
#' @inheritParams create_frames
#' @param frames list of \code{ggplot} objects, crated with \code{\link{create_frames}}.
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
#' @importFrom ggplot2 labs waiver theme element_text
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
  
  gg.xy <- ggplot_build(frames[[1]])$data[[2]][,1:2]
  progress <- lapply(seq(min(gg.xy$x), max(gg.xy$x), length.out = length(frames)), function(x, x.min = min(gg.xy$x), y = max(gg.xy$y)){
    cbind.data.frame(x = c(x.min, x), y = c(y, y))
  })
  
  mapply(x = frames, y = progress, function(x, y) x + geom_line(aes(x = x, y = y), data = y, colour = colour, size = size), SIMPLIFY = F)
}
