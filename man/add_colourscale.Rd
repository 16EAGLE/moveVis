% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_colourscale.R
\name{add_colourscale}
\alias{add_colourscale}
\title{Add scale to frames}
\usage{
add_colourscale(
  frames,
  type,
  colours,
  labels = waiver(),
  na.colour = "grey50",
  na.show = TRUE,
  legend_title = NULL,
  verbose = TRUE
)
}
\arguments{
\item{frames}{an object of class \code{moveVis} created with \code{\link{frames_spatial}}.}

\item{type}{character, either \code{"gradient"} or \code{"discrete"}. Must be equal to the defintion of argument \code{r_type} with which \code{frames} have been created (see \code{\link{frames_spatial}}).}

\item{colours}{character, a vector of colours. If \code{type = "discrete"}, number of colours must be equal to the number of classes contained in the raster imagery with which \code{frames} have been created. Optioanlly, the vector can be named to associate map values with colours and define the scale limits, e.g. \code{c("-1" = "red", "0" = "blue", "1" = "green")}}

\item{labels}{character, a vector of labels with the same length as \code{colours}. Ignored, if \code{type = "gradient"}.}

\item{na.colour}{character, colour to use for missing values.}

\item{na.show}{logical, whether to display NA values in discrete scaling. Ignored, if \code{type = "gradient"}.}

\item{legend_title}{character, a legend title.}

\item{verbose}{logical, if \code{TRUE}, messages and progress information are displayed on the console (default).}
}
\value{
A frames object of class \code{moveVis}.
}
\description{
This function adjusts the colour scales of the animation frames created with \code{\link{frames_spatial}} and custom map imagery.
}
\examples{
library(moveVis)
library(move)

data("move_data", "basemap_data")
# align movement
m <- align_move(move_data, res = 4, unit = "mins")

# create spatial frames with frames_spatial:
r_list <- basemap_data[[1]]
r_times <- basemap_data[[2]]

\dontrun{
frames <- frames_spatial(m, r_list = r_list, r_times = r_times, r_type = "gradient",
                         fade_raster = TRUE)
frames[[100]] # take a look at one of the frames

# default blue is boring, let's change the colour scale of all frames
frames <- add_colourscale(frames, type = "gradient", colours = c("orange", "white", "darkgreen"),
                          legend_title = "NDVI")
frames[[100]]


# let's make up some classification data with 10 classes
r_list <- lapply(r_list, function(x){
  y <- raster::setValues(x, round(raster::getValues(x)*10))
  return(y)
})
# turn fade_raster to FALSE, since it makes no sense to temporally interpolate discrete classes
frames <- frames_spatial(m, r_list = r_list, r_times = r_times, r_type = "discrete",
                         fade_raster = FALSE)
frames[[100]]

# now, let's assign a colour per class value to frames
colFUN <- colorRampPalette(c("orange", "lightgreen", "darkgreen"))
cols <- colFUN(10)
frames <- add_colourscale(frames, type = "discrete", colours = cols, legend_title = "Classes")
frames[[100]]
}

}
\seealso{
\code{\link{frames_spatial}} \code{\link{frames_graph}} \code{\link{animate_frames}}
}
\author{
Jakob Schwalb-Willmann
}
