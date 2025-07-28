#' Tools to visualize movement data in R
#'
#' \code{moveVis} provides tools to visualize movement data (e.g. from GPS tracking) and temporal changes of environmental data (e.g. from remote sensing) by creating video animations.
#' The \code{moveVis} package is closely connected to the \code{move2} package and builds up on \code{ggplot2} grammar of graphics.
#'
#' @details The package includes the following functions, sorted by the order they would be applied to create an animation from movement data:
#' 
#' \itemize{
#'    \item \code{\link{align_move}} aligns single and multi-individual movement data to a uniform time scale with a uniform temporal resolution needed for creating an animation from it. Use this function to prepare your movement data for animation depending on the temporal resolution that suits your data.
#'    \item \code{\link{get_maptypes}} returns a character vector of available map types that can be used with \code{\link{frames_spatial}}. \code{moveVis} supports OpenStreetMaps and Mapbox basemap imergay. Alternatively, you can provide custom imagery to \code{\link{frames_spatial}}.
#'    \item \code{\link{frames_spatial}} creates frames from movement and map/raster data. Frames are returned as an object of class \code{moveVis} and can be subsetted, viewed (see \code{\link{render_frame}}), modified (see \code{\link{add_gg}} and associated functions ) and animated (see \code{\link{animate_frames}}).
#'    \item \code{\link{frames_graph}} creates frames of \code{ggplot2} graphs displaying movement-environment interaction. Each object represents a single frame. Each frame can be viewed or modified individually. The returnedframes can be animated using \code{\link{animate_frames}}.
#'    \item \code{\link{add_gg}} adds \code{ggplot2} functions (e.g. to add layers such as points, polygons, lines, or to change scales etc.) to the animation frames created with \code{\link{frames_spatial}} or \code{\link{frames_graph}}. Instead of creating your own \code{ggplot2} functions, you can use one of the other \code{moveVis} \code{add_} functions:
#'    \item \code{\link{add_labels}} adds character labels such as title or axis labels to animation frames created with \code{\link{frames_spatial}} or \code{\link{frames_graph}}.
#'    \item \code{\link{add_scalebar}} adds a scalebar to the animation frames created with \code{\link{frames_spatial}} or \code{\link{frames_graph}}.
#'    \item \code{\link{add_northarrow}} adds a north arrow to the animation frames created with \code{\link{frames_spatial}} or \code{\link{frames_graph}}.
#'    \item \code{\link{add_progress}} adds a progress bar to animation frames created with \code{\link{frames_spatial}} or \code{\link{frames_graph}}.
#'    \item \code{\link{add_timestamps}} adds timestamps to animation frames created with \code{\link{frames_spatial}} or \code{\link{frames_graph}}.
#'    \item \code{\link{add_text}} adds static or dynamically changing text to the animation frames created with \code{\link{frames_spatial}} or \code{\link{frames_graph}}.
#'    \item \code{\link{add_colourscale}} adjusts the colour scales of the animation frames created with \code{\link{frames_spatial}} and custom map imagery.
#'    \item \code{\link{join_frames}} side-by-side joins the \code{ggplot2} objects of two or more frames lists of equal lengths into a single list of \code{ggplot2} objects per frame using \code{\link[patchwork]{wrap_plots}}. This is useful if you want to side-by-side combine spatial frames returned by \code{\link{frames_spatial}} with graph frames returned by \code{\link{frames_graph}}.
#'    \item \code{\link{get_frametimes}} extracts the timestamps associated with each frame from a \code{moveVis} object created using \code{\link{frames_spatial}} or \code{\link{frames_graph}} and returns them as a vector.
#'    \item \code{\link{render_frame}} renders an individual frame. It yields the same result as if an individual frame is extracted using default subsetting \code{[[]]}.
#'    \item \code{\link{suggest_formats}} returns a selection of suggested file formats that can be used with \code{out_file} of \code{\link{animate_frames}} on your system.
#'    \item \code{\link{animate_frames}} creates an animation from a list of frames computed with \code{\link{frames_spatial}} or \code{\link{frames_graph}}.
#'    \item \code{\link{view_spatial}} displays movement tracks on an interactive \code{mapview} or \code{leaflet} map.
#'    \item \code{\link{use_multicore}} enables multi-core usage for computational expensive processing steps. 
#'    \item \code{\link{use_disk}} enables the usage of disk space for creating frames, which can prevent memory overload when creating frames for very large animations.
#' }
#' 
#' The majority of these functions can be used with the forward pipe operator \code{\%>\%}, which is re-exported by \code{moveVis}.
#' 
#' @author Jakob Schwalb-Willmann. Maintainer: Jakob Schwalb-Willmann, moveVis@schwalb-willmann.de
"_PACKAGE"