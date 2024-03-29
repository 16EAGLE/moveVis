% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_labels.R
\name{add_labels}
\alias{add_labels}
\title{Add labels to frames}
\usage{
add_labels(
  frames,
  title = waiver(),
  subtitle = waiver(),
  caption = waiver(),
  tag = waiver(),
  x = waiver(),
  y = waiver(),
  verbose = TRUE
)
}
\arguments{
\item{frames}{an object of class \code{moveVis} created with \code{\link{frames_spatial}}.}

\item{title}{character, frame title. If \code{NULL}, an existing title of \code{frames} is removed. If \code{waiver()} (default, see \code{ggplot2::waiver()}), an existing title of \code{frames} is kept.}

\item{subtitle}{character, frame subtitle. If \code{NULL}, an existing title of \code{frames} is removed. If \code{waiver()} (default, see \code{ggplot2::waiver()}), an existing title of \code{frames} is kept.}

\item{caption}{character, frame caption. If \code{NULL}, an existing title of \code{frames} is removed. If \code{waiver()} (default, see \code{ggplot2::waiver()}), an existing title of \code{frames} is kept.}

\item{tag}{character, frame tag. If \code{NULL}, an existing title of \code{frames} is removed. If \code{waiver()} (default, see \code{ggplot2::waiver()}), an existing title of \code{frames} is kept.}

\item{x}{character, label of the x axis. If \code{NULL}, an existing title of \code{frames} is removed. If \code{waiver()} (default, see \code{ggplot2::waiver()}), an existing title of \code{frames} is kept.}

\item{y}{character, label of the y axis. If \code{NULL}, an existing title of \code{frames} is removed. If \code{waiver()} (default, see \code{ggplot2::waiver()}), an existing title of \code{frames} is kept.}

\item{verbose}{logical, if \code{TRUE}, messages and progress information are displayed on the console (default).}
}
\value{
A frames object of class \code{moveVis}.
}
\description{
This function adds character labels such as title or axis labels to animation frames created with \code{\link{frames_spatial}}.
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

# add labels to frames:
frames <- add_labels(frames, title = "Example animation using moveVis::add_labels()", 
                     subtitle = "Adding a subtitle to frames created using frames_spatial()",
                     caption = "Projection: Geographical, WGS84. Sources: moveVis examples.",
                     x = "Longitude", y = "Latitude")
# have a look at one frame
frames[[100]]
}

}
\seealso{
\code{\link{frames_spatial}} \code{\link{frames_graph}} \code{\link{animate_frames}}
}
\author{
Jakob Schwalb-Willmann
}
