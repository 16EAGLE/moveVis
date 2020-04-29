#' Add scalebar to frames
#'
#' This function adds a scalebar to the animation frames created with \code{\link{frames_spatial}}.
#'
#' @inheritParams add_labels
#' @param distance numeric, optional. Distance displayed by the scalebar (in either km or miles defined by argument \code{units}) By default, the displayed distance is calculated automatically.
#' @param height numeric, height of the scalebar in a range from 0 to 1 as the proportion of the overall height of the frame map. Default is 0.015.
#' @param position character, position of the scalebar on the map. Either \code{"bottomleft", "upperleft", "upperright", "bottomright"}. Ignored, if \code{x} and \code{y} are set.
#' @param x numeric, position of the bottom left corner of the scalebar on the x axis. If not set, \code{position} is used to calculate the position of the scalebar.
#' @param y numeric, position of the bottom left corner of the scalebar on the y axis. If not set, \code{position} is used to calculate the position of the scalebar.
#' @param colour character, colour of the distance labels. Default is \code{"black"}.
#' @param label_margin numeric, distance of the labels to the scalebar as a proportion of the height of the scalebar (e.g. if set to 2, the labels will be positioned with a distance to the scalebar of twice the scalebar height).
#' @param units character, either "km" for kilometers or "miles" for miles.
#'
#' @return List of frames.
#' @author Jakob Schwalb-Willmann
#'
#' @importFrom ggplot2 geom_polygon geom_text aes_string expr
#' @importFrom sf st_distance st_sfc st_point st_crs
#'
#' @examples 
#' library(moveVis)
#' library(move)
#' 
#' data("move_data", "basemap_data")
#' m <- align_move(move_data, res = 4, unit = "mins")
#' 
#' # create spatial frames using a custom NDVI base layer
#' r_list <- basemap_data[[1]]
#' r_times <- basemap_data[[2]]
#' 
#' \dontrun{
#' frames <- frames_spatial(m, r_list = r_list, r_times = r_times, r_type = "gradient",
#'                          fade_raster = TRUE)
#' 
#' # add a scale bar to frames:
#' frames.a <- add_scalebar(frames)
#' frames.a[[100]]
#' 
#' # or in white at another position
#' frames.b <- add_scalebar(frames, colour = "white", position = "bottomright")
#' frames.b[[100]]
#' 
#' # or with another height
#' frames.c <- add_scalebar(frames, colour = "white", position = "bottomright", height = 0.025)
#' frames.c[[100]]
#' }
#' @seealso \code{\link{frames_spatial}} \code{\link{frames_graph}} \code{\link{animate_frames}}
#' @export

add_scalebar <- function(frames, distance = NULL, height = 0.015, position = "bottomleft", x = NULL, y = NULL, colour = "black", label_margin = 1.2, units = "km", verbose = TRUE){
  
  ## checks
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  if(!inherits(frames, "list")) out("Argument 'frames' needs to be a list of ggplot objects. See frames_spatial()).", type = 3)
  if(!all(sapply(frames, function(x) inherits(x, "ggplot")))) out("At least one element of argument 'frames' is not a ggplot object.", type = 3)
  if(!is.character(position)) out("Argument 'position' needs to be of type 'character'.", type = 3)
  if(isFALSE(units == "km" | units == "miles")) out("Argument 'units' must either be 'km' or 'miles'.", type = 3)
  
  check.args <- list(distance = distance, x = x, y = y)
  catch <- lapply(seq(1, length(check.args)), function(i) if(!any(is.numeric(check.args[[i]]), is.null(check.args[[i]]))) out(paste0("Argument '", names(check.args)[[i]], "' needs to be of type 'numeric'."), type = 3))
 
  ## calculate gg plot dimensions
  gg.crs <- frames[[1]]$coordinates$crs
  gg.xy <- ggplot_build(frames[[1]])$data[[1]]
  
  .corner <- function(xy){
    list(bottomleft = c(min(xy$xmin), min(xy$ymin)), upperleft = c(min(xy$xmin), max(xy$ymax)),
         upperright = c(max(xy$xmax), max(xy$ymax)), bottomright = c(max(xy$xmax), min(xy$ymin)))
  }
  gg.corner <- .corner(gg.xy)
  
  # cross_dateline
  if(is.null(gg.crs)){
    gg.crs <- st_crs(4326)
    gg.xy_cdl <- gg.xy
    gg.xy_cdl$xmin[gg.xy_cdl$xmin < -180] <- -180
    gg.xy_cdl$xmax[gg.xy_cdl$xmax < -180] <- -180
    gg.xy_cdl$xmin[gg.xy_cdl$xmin > 180] <- 180
    gg.xy_cdl$xmax[gg.xy$xmax > 180] <- 180
    gg.corner_sf <- lapply(.corner(gg.xy_cdl), function(x) st_sfc(st_point(x), crs = gg.crs))
    # THIS SOLUTION IS NOT CORRECT - SCALES WILL BE TOO SMALL!
  } else {
    gg.corner_sf <- lapply(gg.corner, function(x) st_sfc(st_point(x), crs = gg.crs))
  }
  gg.dist <- list(x = as.numeric(suppressPackageStartupMessages(st_distance(gg.corner_sf$bottomleft, gg.corner_sf$bottomright, by_element = T)))/1000,
                  y = as.numeric(suppressPackageStartupMessages(st_distance(gg.corner_sf$bottomleft, gg.corner_sf$upperleft, by_element = T)))/1000)
 
  ## calculate axis distances
  if(units == "miles") gg.dist <- lapply(gg.dist, function(x) x/1.609344 )
  gg.diff <- list(x = max(gg.xy$xmax) - min(gg.xy$xmin), y = max(gg.xy$ymax) - min(gg.xy$ymin))
  
  ## calculate scale distance
  if(!is.null(distance)){scale.dist <- distance}else{
    scale.dist <- digits <- 0
    while(scale.dist == 0){
      scale.dist <- round((gg.dist$x*0.2), digits = digits)
      digits <- digits+1
    }
  }
  
  # round to even
  if(scale.dist > 10) scale.dist <- round(scale.dist/2)*2

  scale.diff <- gg.diff$x*((scale.dist)/gg.dist$x)
  
  ## calculate scale postiotn
  gg.margin <- list(bottomleft = unlist(gg.diff)*0.1,
                    upperleft = c(x = gg.diff$x*0.1, y = gg.diff$y*-(0.1+height)),
                    upperright = c(x = (gg.diff$x*-0.1)-scale.diff, y = gg.diff$y*-(0.1+height)),
                    bottomright = c(x = (gg.diff$x*-0.1)-scale.diff, y = gg.diff$y*0.1))

  scale.outer <- if(all(!is.null(x), !is.null(y))) c(x, y) else gg.corner[[position]] + gg.margin[[position]]
  scale.outer <- rbind.data.frame(scale.outer, c(scale.outer[1], (scale.outer[2] + (gg.diff$y*height))), 
                              c(scale.outer[1]+scale.diff, (scale.outer[2] + (gg.diff$y*height))), c(scale.outer[1]+scale.diff, scale.outer[2]))
  colnames(scale.outer) <- c("x", "y")
  
  ## calculate inner scale position
  scale.inner <- scale.outer
  scale.inner[1:2,1] <- scale.inner[1:2,1] + (scale.diff/2)
  
  ## calculate annotation position
  text.margin <- (max(scale.outer$y) - min(scale.outer$y))*label_margin
  text.data <- cbind.data.frame(x = c(min(scale.outer$x), min(scale.inner$x), max(scale.outer$x)),
                                y = (min(scale.outer$y)-text.margin),
                                label = paste0(c(0, scale.dist/2, scale.dist), " ", units),
                                col = colour, stringsAsFactors = F)
  
  add_gg(frames, gg = expr(list(geom_polygon(aes_string(x = "x", y = "y"), data = scale.outer, fill = "white", colour = "black"), 
                                geom_polygon(aes_string(x = "x", y = "y"), data = scale.inner, fill = "black", colour = "black"),
                                geom_text(aes_string(x = "x", y = "y", label = "label", color = "col"), data = text.data, size = 3, colour = text.data$col))),
         scale.outer = scale.outer, scale.inner = scale.inner, text.data = text.data)
}
