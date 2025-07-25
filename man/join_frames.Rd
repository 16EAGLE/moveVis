% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/join_frames.R
\name{join_frames}
\alias{join_frames}
\title{Join multiple frames side-by-side}
\usage{
join_frames(
  frames_list,
  guides = "collect",
  design = NULL,
  render_all_legends = FALSE,
  ...,
  frames_lists = NULL,
  verbose = T
)
}
\arguments{
\item{frames_list}{list, a list of two or more \code{moveVis} frame objects that you want to combine into onw. Must be of equal lengths. Frames are being passed to the \code{plotlist} argument of \code{\link{plot_grid}} and combined frame-by-frame.}

\item{guides}{character, controls how to treat the scales/legends of both frames. See \code{\link{wrap_plots}} for details. Defaults to 'collect'.}

\item{design}{character, controls how frames are arranged. See \code{\link{wrap_plots}} for details. By default, frames are joined sidy-by-side horizontally.}

\item{render_all_legends}{logical, whether legends should be preserved (\code{TRUE}) or only the legend of the first frames object should be rendered (\code{FALSE}, default).}

\item{...}{Further arguments, specifying the appearance of the joined \code{ggplot2} objects, passed to \code{\link{wrap_plots}}. See \code{\link{wrap_plots}} for further options.}

\item{frames_lists}{deprecated}

\item{verbose}{logical, if \code{TRUE}, messages and progress information are displayed on the console (default).}
}
\value{
A frames object of class \code{moveVis}.
}
\description{
This function joins two or more \code{moveVis} frame objects of equal lengths side-by-side into a single plot per frame using \code{\link{plot_grid}}. This is useful if you want to side-by-side combine spatial frames returned by \code{\link{frames_spatial}} with graph frames returned by \code{\link{frames_graph}}.
}
\examples{
\dontrun{
library(moveVis)
library(move)

data("move_data", "basemap_data")
# align movement
m <- align_move(move_data, res = 4, unit = "mins")

# create spatial frames and graph frames:
r_list <- basemap_data[[1]]
r_times <- basemap_data[[2]]

frames.sp <- frames_spatial(m, r_list = r_list, r_times = r_times, r_type = "gradient",
                            fade_raster = TRUE)
frames.sp <- add_colourscale(frames.sp, type = "gradient",
                             colours = c("orange", "white", "darkgreen"), legend_title = "NDVI")
frames.flow <- frames_graph(m, r_list, r_times, path_legend = FALSE, graph_type = "flow")
frames.hist <- frames_graph(m, r_list, r_times, path_legend = FALSE, graph_type = "hist")

# check lengths (must be equal)
sapply(list(frames.sp, frames.flow, frames.hist), length)

# Let's join the graph frames vertically
frames.join.gr <- join_frames(list(frames.flow, frames.hist), ncol = 1, nrow = 2)
frames.join.gr[[100]]

# Now, let's join the joined graph frames with the spatial frames horizontally
# in 2:1 ration and align all axis
frames.join <- join_frames(list(frames.sp, frames.join.gr),
                           ncol = 2, nrow = 1, rel_widths = c(2, 1), axis = "tb")
frames.join[[100]]
# in a standard graphics device, this looks a bit unproportional
# however when setting the correct width, height and resolution of a graphic device,
# it will come out well aligend.

# Do so for example with animate_move() with width = 900, dheight = 500 and res = 90
animate_frames(frames.join, out_file = tempfile(fileext = ".gif"), fps = 25, 
               width = 900, height = 500, res = 90, display = TRUE, overwrite = TRUE)
}
}
\seealso{
\code{\link{frames_spatial}} \code{\link{frames_graph}} \code{\link{animate_frames}}
}
