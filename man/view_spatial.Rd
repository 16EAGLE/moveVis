% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/view_spatial.R
\name{view_spatial}
\alias{view_spatial}
\title{View movements on an interactive map}
\usage{
view_spatial(
  m,
  render_as = "mapview",
  time_labels = TRUE,
  stroke = TRUE,
  path_colours = NA,
  path_legend = TRUE,
  path_legend_title = "Names",
  verbose = TRUE
)
}
\arguments{
\item{m}{\code{move} or \code{moveStack}. May contain a column named \code{colour} to control path colours (see \code{details}).}

\item{render_as}{character, either \code{'mapview'} to return a \code{mapview} map or \code{'leaflet'} to return a \code{leaflet} map.}

\item{time_labels}{logical, wether to display timestamps for each track fix when hovering it with the mouse cursor.}

\item{stroke}{logical, whether to draw stroke around circles.}

\item{path_colours}{character, a vector of colours. Must be of same length as number of individual tracks in \code{m} and refers to the order of tracks in \code{m}. If undefined (\code{NA}) and \code{m} contains a column named \code{colour}, colours provided within \code{m} are used (see details). Othwersie, colours are selected from a standard rainbow palette per individual track.}

\item{path_legend}{logical, wether to add a path legend from \code{m} or not. Legend tracks and colours will be ordered by the tracks' temporal apperances, not by their order in \code{m}.}

\item{path_legend_title}{character, path legend title. Default is \code{"Names"}.}

\item{verbose}{logical, if \code{TRUE}, messages and progress information are displayed on the console (default).}
}
\value{
An interatcive \code{mapview} or \code{leaflet} map.
}
\description{
\code{view_spatial} is a simple wrapper that displays movement tracks on an interactive \code{mapview} or \code{leaflet} map.
}
\details{
If argument \code{path_colours} is not defined (set to \code{NA}), path colours can be defined by adding a character column named \code{colour} to \code{m}, containing a colour code or name per row (e.g. \code{"red"}. This way, for example, column \code{colour} for all rows belonging to individual A can be set to \code{"green"}, while column \code{colour} for all rows belonging to individual B can be set to \code{"red"}.
Colours could also be arranged to change through time or by behavioral segments, geographic locations, age, environmental or health parameters etc. If a column name \code{colour} in \code{m} is missing, colours will be selected automatically. Call \code{colours()} to see all available colours in R.
}
\examples{
\dontrun{
library(moveVis)
library(move)

data("move_data")

# return a mapview map (mapview must be installed)
view_spatial(move_data)

# return a leaflet map (leaflet must be installed)
view_spatial(move_data, render_as = "leaflet")

# turn off time labels and legend
view_spatial(move_data, time_labels = FALSE, path_legend = FALSE)

}
}
\seealso{
\code{\link{frames_spatial}}
}
\author{
Jakob Schwalb-Willmann
}
