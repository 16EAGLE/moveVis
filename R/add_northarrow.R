#' Add north arrow to frames
#'
#' This function adds a north arrow to the animation frames created with \code{\link{create_frames}}.
#'
#' @inheritParams add_labels
#' @param height numeric, height of the north arrow in a range from 0 to 1 as the proportion of the overall height of the frame map. Default is 0.5.
#' @param position character, position of the north arrow on the map. Either \code{"bottomleft", "upperleft", "upperright", "bottomright"}. Ignored, if \code{x} and \code{y} are set.
#' @param x numeric, position of the bottom left corner of the north arrow on the x axis. If not set, \code{position} is used to calculate the position of the north arrow.
#' @param y numeric, position of the bottom left corner of the north arrow on the y axis. If not set, \code{position} is used to calculate the position of the north arrow.
#' @param colour character, colour. Default is \code{"black"}.
#' @param size numeric, arrow size. Default is 1.
#' @param label_text character, text below the north arrow. Default is "North".
#' @param label_margin numeric, margin between label and north arrow as a proportion of the size of the north arrow. Default is 0.3.
#' @param label_size numeric, label font size. Default is 4.
#'
#' @return List of frames.
#' @author Jakob Schwalb-Willmann
#'
#' @importFrom ggplot2 geom_line geom_text aes
#' @importFrom geosphere distGeo
#'
#' @seealso \link{create_frames}
#' @export

add_northarrow <- function(frames, height = 0.05, position = "bottomright", x = NULL, y = NULL, colour = "black", size = 1,
                           label_text = "N", label_margin = 0.4, label_size = 5, verbose = TRUE){
  
  ## checks
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  if(!inherits(frames, "list")) out("Argument 'frames' needs to be a list of ggplot objects. See create_frames()).", type = 3)
  if(!all(sapply(frames, function(x) inherits(x, "ggplot")))) out("At least one element of argument 'frames' is not a ggplot object.", type = 3)
  if(!is.character(position)) out("Argument 'position' needs to be of type 'character'.", type = 3)
  
  check.args <- list(x = x, y = y)
  catch <- lapply(seq(1, length(check.args)), function(i) if(!any(is.numeric(check.args[[i]]), is.null(check.args[[i]]))) out(paste0("Argument '", names(check.args)[[i]], "' needs to be of type 'numeric'."), type = 3))
 
  ## calculate gg plot dimensions
  gg.xy <- ggplot_build(frames[[1]])$data[[1]]
  gg.corner <- list(bottomleft = c(min(gg.xy$xmin), min(gg.xy$ymin)), upperleft = c(min(gg.xy$xmin), max(gg.xy$ymax)),
                    upperright = c(max(gg.xy$xmax), max(gg.xy$ymax)), bottomright = c(max(gg.xy$xmax), min(gg.xy$ymin)))
  
  ## calculate arrow differnece
  gg.diff <- list(x = max(gg.xy$xmax) - min(gg.xy$xmin), y = max(gg.xy$ymax) - min(gg.xy$ymin))
  arrow.diff <- gg.diff$y*height
  
  ## calculate scale postiotn
  gg.margin <- list(bottomleft = unlist(gg.diff)*0.1,
                    upperleft = c(x = gg.diff$x*0.1, y = gg.diff$y*-(0.1+height)),
                    upperright = c(x = gg.diff$x*-0.1, y = gg.diff$y*-(0.1+height)),
                    bottomright = c(x = gg.diff$x*-0.1, y = gg.diff$y*0.1))

  arrow.data <- if(all(!is.null(x), !is.null(y))) c(x, y) else gg.corner[[position]] + gg.margin[[position]]
  arrow.data <- rbind.data.frame(arrow.data, c(arrow.data[1], arrow.data[2]+arrow.diff))
  colnames(arrow.data) <- c("x", "y")
  
  ## text label
  text.margin <- (max(arrow.data$y) - min(arrow.data$y))*label_margin
  text.data <- data.frame(x = arrow.data$x[1], y = min(arrow.data$y)-text.margin, label = label_text)
  
  add_gg(frames, gg = expr(list(geom_line(aes(x = x, y = y), data = arrow.data, arrow=arrow(length = unit(3.7, "mm")), size = size, colour = colour),
                                geom_text(aes(x = x, y = y, label = label), text.data, colour = colour, size = label_size))),
         arrow.data = arrow.data, size = size, colour = colour, text.data = text.data, label_size = label_size)
}
