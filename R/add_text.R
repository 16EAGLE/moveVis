#' Add static or dynamic text to frames
#'
#' This function adds static or dynamically changing text to the animation frames created with \code{\link{frames_spatial}}.
#'
#' @inheritParams add_labels
#' @param labels character, text to be added to frames. Either a single character value or a character vector of same length as \code{frames}.
#' @param x numeric, position of text on the x scale. Either a single numeric value or a numeric vector of same length as \code{frames}.
#' @param y numeric, position of text on the y scale. Either a single numeric value or a numeric vector of same length as \code{frames}.
#' @param colour character, the text colour(s). Either a single character value or a character vector of same length as \code{frames}.
#' @param size numeric, the text size(s). Either a single numeric value or a numeric vector of same length as \code{frames}.
#' @param type character, either \code{"text"} to draw text or \code{"label"} to draw text inside a box.
#'
#' @return List of frames.
#' @author Jakob Schwalb-Willmann
#'
#' @importFrom ggplot2 annotate 
#' @importFrom rlang expr
#' @importFrom dplyr bind_cols
#'
#' @seealso \link{frames_spatial}
#' @export

add_text <- function(frames, labels, x, y, colour = "black", size = 3, type = "text", verbose = TRUE){
  
  ## checks
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  if(!inherits(frames, "list")) out("Argument 'frames' needs to be a list of ggplot objects. See frames_spatial()).", type = 3)
  if(!all(sapply(frames, function(x) inherits(x, "ggplot")))) out("At least one element of argument 'frames' is not a ggplot object.", type = 3)
  
  if(!is.character(labels)) out("Argument 'labels' must be of type 'character'.", type = 3)
  if(!is.character(colour)) out("Argument 'colour' must be of type 'character'.", type = 3)
  if(!is.numeric(x)) out("Argument 'x' must be of type 'numeric'.", type = 3)
  if(!is.numeric(y)) out("Argument 'y' must be of type 'numeric'.", type = 3)
  if(!is.numeric(size)) out("Argument 'size' must be of type 'numeric'.", type = 3)
  
  ## check lengths
  check <- list("labels" = labels, "x" = x, "y" = y, "colour" = colour, "size" = size)
  data <- sapply(1:length(check), function(i){
    if(length(check[[i]]) == 1) v <- rep(check[[i]], length(frames)) else v <- check[[i]]
    if(length(v) != length(frames)) out(paste0("Length of argument ", names(check)[[i]], " must either be 1 or equal to the length of agrument 'frames'."), type = 3)
    return(v)
  }, simplify = F)
  data <- as.data.frame(bind_cols(data), stringsAsFactors = F)
  data <- split(data, seq(nrow(data)))
  
  add_gg(frames, gg = expr(annotate(type, x = data[[2]], y = data[[3]], label = data[[1]], colour = data[[4]], size = data[[5]])), data = data, type = type)
}
