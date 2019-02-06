#' Add scale to frames
#'
#' This function adjusts the colour scales of the animation frames created with \code{\link{frames_spatial}} and custom map imagery.
#'
#' @inheritParams add_labels
#' @param type character, either \code{"gradient"} or \code{"discrete"}. Must be equal to the defintion of argument \code{r_type} with which \code{frames} have been created (see \code{\link{frames_spatial}}).
#' @param colours character, a vector of colours. If \code{type = "discrete"}, number of colours must be equal to the number of classes contained in the raster imagery with which \code{frames} have been created. Provide a named vector to associate map values with colours, e.g. \code{c("1" = "red", "2" = "green", "3" = "black")}
#' @param labels character, a vector of labels with the same length as \code{colours}. Ignored, if \code{type = "gradient"}.
#' @param legend_title character, a legend title.
#'
#' @return List of frames.
#' @author Jakob Schwalb-Willmann
#'
#' @importFrom ggplot2 scale_fill_gradientn scale_colour_manual expr
#'
#' @seealso \link{frames_spatial}
#' @export

add_colourscale <- function(frames, type, colours, labels = waiver(), legend_title = NULL, verbose = TRUE){
  
  ## checks
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  if(!inherits(frames, "list")) out("Argument 'frames' needs to be a list of ggplot objects. See frames_spatial()).", type = 3)
  if(!all(sapply(frames, function(x) inherits(x, "ggplot")))) out("At least one element of argument 'frames' is not a ggplot object.", type = 3)
  
  if(!inherits(type, "character")) out("Argument 'type' must be of type 'character'.", type = 3)
  if(!any(c("gradient", "discrete") %in% type)) out("Argument 'type' must either be 'gradient' or 'discrete'.", type = 3)
  if(!inherits(colours, "character")) out("Argument 'colours' must be of type 'character'.", type = 3)
  if(length(colours) == 0) out("Argument 'colours' must have length greater than 0.", type = 3)
  if(all(type == "discrete", !inherits(labels, "waiver"))){
    if(!inherits(labels, "character")) out("Argument 'labels' must be of type 'character'.", type = 3)
    if(length(labels) != length(colours)) out("Arguments 'colours' and 'labels' must have equal lengths.", type = 3)
  }
  
  if(type == "gradient") gg.scale <- expr(scale_fill_gradientn(name = legend_title, colours = colours))
  if(type == "discrete") gg.scale <- expr(scale_fill_manual(name = legend_title, values = colours, labels = labels))
  
  add_gg(frames, gg.scale, colours = colours, legend_title = legend_title)
}
