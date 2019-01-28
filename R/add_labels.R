#' Add labels to frames
#'
#' This function adds character labels such as title or axis labels to animation frames created with \code{\link{create_frames}}.
#'
#' @inheritParams create_frames
#' @param frames list of \code{ggplot} objects, crated with \code{\link{create_frames}}.
#' @param title character, frame title. If \code{NULL}, an existing title of \code{frames} is removed. If \code{waiver()} (default, see \code{ggplot2::waiver()}), an existing title of \code{frames} is kept.
#' @param subtitle character, frame subtitle. If \code{NULL}, an existing title of \code{frames} is removed. If \code{waiver()} (default, see \code{ggplot2::waiver()}), an existing title of \code{frames} is kept.
#' @param caption character, frame caption. If \code{NULL}, an existing title of \code{frames} is removed. If \code{waiver()} (default, see \code{ggplot2::waiver()}), an existing title of \code{frames} is kept.
#' @param tag character, frame tag. If \code{NULL}, an existing title of \code{frames} is removed. If \code{waiver()} (default, see \code{ggplot2::waiver()}), an existing title of \code{frames} is kept.
#' @param x character, label of the x axis. If \code{NULL}, an existing title of \code{frames} is removed. If \code{waiver()} (default, see \code{ggplot2::waiver()}), an existing title of \code{frames} is kept.
#' @param y character, label of the y axis. If \code{NULL}, an existing title of \code{frames} is removed. If \code{waiver()} (default, see \code{ggplot2::waiver()}), an existing title of \code{frames} is kept.
#'
#'
#' @return List of frames.
#' @author Jakob Schwalb-Willmann
#'
#' @importFrom ggplot2 labs waiver theme element_text expr
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
  if(any(!sapply(waiver.args[!waiver.which], function(x) any(is.character(x), is.null(x))))) out("Label arguments must be of type character, NULL to remove a label or waiver() to keep an already set label.", type = 3)
  
  add_gg(frames, gg = expr(list(labs(title = title, subtitle = subtitle, caption = caption, tag = tag, x = x, y = y),
                           theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5)))),
         title = title, subtitle = subtitle, caption = caption, tag = tag, x = x, y = y)
}
