#' Add visual elements to animation frames
#'
#' Functions that add visual elements to animation frames created with \code{links{create_frames}} to adjust the appearance of the movement animation before calling \code{\link{animate_frames}}.
#'

#' @details
#' \code{add_labels} adds title, sub title and axis labels to all frames
#'
#' @return List of frames.
#' @author Jakob Schwalb-Willmann
#'
#' @importFrom ggplot2 labs waiver theme element_text
#'
#' @seealso \link{create_frames}
#' @export

add_northarrow <- function(frames, ..., verbose = TRUE){
  
  ## checks
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  if(!inherits(frames, "list")) out("Argument 'frames' needs to be a list of ggplot objects. See create_frames()).", type = 3)
  if(!all(sapply(frames, function(x) inherits(x, "ggplot")))) out("At least one element of argument 'frames' is not a ggplot object.", type = 3)
  

}
