#' Add labels to frames
#'
#' This function adds character labels such as title or axis labels to animation frames created with \code{\link{create_frames}}.
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

add_labels <- function(frames, title = waiver(), subtitle = waiver(), caption = waiver(), tag = waiver(),
                     x = waiver(), y = waiver(), verbose = TRUE){

  ## checks
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  if(!inherits(frames, "list")) out("Argument 'frames' needs to be a list of ggplot objects. See create_frames()).", type = 3)
  if(!all(sapply(frames, function(x) inherits(x, "ggplot")))) out("At least one element of argument 'frames' is not a ggplot object.", type = 3)
  
  waiver.args <- list(title = title, subtitle = subtitle, caption = caption, tag = tag, x = x, y = y)
  waiver.which <- sapply(waiver.args, function(x) inherits(x, "waiver"))
  if(all(waiver.which)) out("At least one label argument has to be defined.", type = 3)
  if(any(!sapply(waiver.args[!waiver.which], function(x) inherits(x, "character")))) out("Label arguments must be of type character.", type = 3)
  
  .addToFrames(frames = frames, eval = list(labs(title = title, subtitle = subtitle, caption = caption, x = x, y = y),
                                                 theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))))
}
