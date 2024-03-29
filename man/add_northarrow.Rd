% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_northarrow.R
\name{add_northarrow}
\alias{add_northarrow}
\title{Add north arrow to frames}
\usage{
add_northarrow(
  frames,
  height = 0.05,
  position = "bottomright",
  x = NULL,
  y = NULL,
  colour = "black",
  size = 1,
  label_text = "N",
  label_margin = 0.4,
  label_size = 5,
  verbose = TRUE
)
}
\arguments{
\item{frames}{an object of class \code{moveVis} created with \code{\link{frames_spatial}}.}

\item{height}{numeric, height of the north arrow in a range from 0 to 1 as the proportion of the overall height of the frame map.}

\item{position}{character, position of the north arrow on the map. Either \code{"bottomleft", "upperleft", "upperright", "bottomright"}. Ignored, if \code{x} and \code{y} are set.}

\item{x}{numeric, position of the bottom left corner of the north arrow on the x axis. If not set, \code{position} is used to calculate the position of the north arrow.}

\item{y}{numeric, position of the bottom left corner of the north arrow on the y axis. If not set, \code{position} is used to calculate the position of the north arrow.}

\item{colour}{character, colour.}

\item{size}{numeric, arrow size.}

\item{label_text}{character, text below the north arrow.}

\item{label_margin}{numeric, margin between label and north arrow as a proportion of the size of the north arrow.}

\item{label_size}{numeric, label font size.}

\item{verbose}{logical, if \code{TRUE}, messages and progress information are displayed on the console (default).}
}
\value{
A frames object of class \code{moveVis}.
}
\description{
This function adds a north arrow to frames created with \code{\link{frames_spatial}}.
}
\examples{
library(moveVis)
library(move)

data("move_data", "basemap_data")
m <- align_move(move_data, res = 4, unit = "mins")

# create spatial frames using a custom NDVI base layer
r_list <- basemap_data[[1]]
r_times <- basemap_data[[2]]

\dontrun{
frames <- frames_spatial(m, r_list = r_list, r_times = r_times, r_type = "gradient",
                         fade_raster = TRUE)

# add a north arrow to frames:
frames.a <- add_northarrow(frames)
frames.a[[100]]

# or in white at another position
frames.b <- add_northarrow(frames, colour = "white", position = "bottomleft")
frames.b[[100]]
}

}
\seealso{
\code{\link{frames_spatial}} \code{\link{frames_graph}} \code{\link{animate_frames}}
}
\author{
Jakob Schwalb-Willmann
}
