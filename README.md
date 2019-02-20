# moveVis <a href="http://movevis.org"><img align="right" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/moveVis_hex.png" /></a>

[![CRAN version](https://www.r-pkg.org/badges/version/moveVis)](https://CRAN.R-project.org/package=moveVis)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/last-month/moveVis?color=brightgreen)](https://CRAN.R-project.org/package=moveVis)
[![Build Status](https://travis-ci.org/16EAGLE/moveVis.svg?branch=master)](https://travis-ci.org/16EAGLE/moveVis) 
[![Coverage](https://codecov.io/gh/16eagle/moveVis/branch/master/graph/badge.svg)](https://codecov.io/gh/16EAGLE/moveVis)

## Introduction

<a href="http://movevis.org">`moveVis`</a> provides tools to visualize movement data (e.g. from GPS tracking) and temporal changes of environmental data (e.g. from remote sensing) by creating video animations. It works with <a href="https://github.com/cran/move">`move`</a>, <a href="https://github.com/edzer/sp">`sp`</a> and <a href="https://github.com/rspatial/raster">`raster`</a> class inputs and turns them into <a href="https://github.com/tidyverse/ggplot2">`ggplot2`</a> frames that can be further customized. <a href="http://movevis.org">`moveVis`</a> uses <a href="https://github.com/r-rust/gifski">`gifski`</a> (wraping the <a href="https://crates.io/crates/gifski">gifski</a> cargo crate) and <a href="https://github.com/ropensci/av">`av`</a> (binding to <a href="https://www.ffmpeg.org/">FFmpeg</a>) to render frames into animated GIF or video files.

<br>
<p align="center"><img width="100%" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/examp2.gif"></p>
<p align="center"><sub>Figure 1: Exemplary movement tracks nearby Lake of Constance and a gradient base layer faded over time</sub></p>
<br>

<br>
<p align="center"><img width="100%" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/examp1.gif"></p>
<p align="center"><sub>Figure 2: Exemplary movement tracks nearby Lake of Constance and a discrete base layer</sub></p>

## Installation

With version 0.10.0, the package has been rewritten from the ground up with the goal to make it easier to customize the appearance of movement animations. Thus, the logic of the package, its functions and their syntax have changed. 

<a href="http://movevis.org">`moveVis`</a> 0.10.0 is not on CRAN yet (but will be soon). Until then, to install <a href="http://movevis.org">`moveVis`</a> version 0.10.0, run:

```r
devtools::install_github("16EAGLE/moveVis")
```

Code written for <a href="http://movevis.org">`moveVis`</a> version <=0.9.9 will not work with the newer versions, but it is quite simple and thus highly recommended to switch to the new syntax due to a variety of advantages. <a href="http://movevis.org">`moveVis`</a> version <=0.9.9 can still be downloaded <a href="https://github.com/16EAGLE/moveVis/releases/tag/v0.9.9">here</a> and installed manually:

```r
setwd("your/download/directory")
install.packages("moveVis-0.9.9.tar.gz", repos = NULL)
```


## Function overview

<a href="http://movevis.org">`moveVis`</a> includes the following functions, sorted by the order they would be applied to create an animation from movement and environmental data:

#### Prepare movement data

* `df2move()` converts a `data.frame` into a `move` or `moveStack` object. This is useful if you do not usually work with the `move` classes and your tracks are present as `data.frames`.
* `align_move()` aligns single and multi-individual movement data to a uniform time scale with a uniform temporal resolution needed for creating an animation from it. Use this function to prepare your movement data for animation depending on the temporal resolution that suits your data.
* `subset_move()` subsets a `move` or `moveStack` by a given time span. This is useful if you want to create a movement animation of only a temporal subset of your data, e.g. a particular day.

#### Create animation frames

* `get_maptypes()` returns a character vector of available map types that can be used with `frames_spatial()`. `moveVis` supports OpenStreetMaps and Mapbox basemap imergay. Alternatively, you can provide custom imagery to `frames_spatial()`.
* `frames_spatial()` creates a list of `ggplot2` maps displaying movement. Each object represents a single frame. Each frame can be viewed or modified individually. The returned list of frames can be animated using `animate_frames()`.
* `frames_graph()` creates a list of `ggplot2` graphs displaying movement-environment interaction. Each object represents a single frame. Each frame can be viewed or modified individually. The returned list of frames can be animated using `animate_frames()`.

#### Adjust frames layout and appearacne

* `add_gg()` adds `ggplot2` functions (e.g. to add layers such as points, polygons, lines, or to change scales etc.) to the animation frames created with `frames_spatial()` or `frames_graph()`. Instead of creating your own `ggplot2` functions, you can use one of the other `moveVis` `add_``functions:
* `add_labels()` adds character labels such as title or axis labels to animation frames created with `frames_spatial()` or `frames_graph()`.
* `add_scalebar()` adds a scalebar to the animation frames created with `frames_spatial()` or `frames_graph()`.
* `add_northarrow()` adds a north arrow to the animation frames created with `frames_spatial()` or `frames_graph()`.
* `add_progress()` adds a progress bar to animation frames created with `frames_spatial()` or `frames_graph()`.
* `add_timestamps()` adds timestamps to animation frames created with `frames_spatial()` or `frames_graph()`.
* `add_text()` adds static or dynamically changing text to the animation frames created with `frames_spatial()` or `frames_graph()`.
* `add_colourscale()` adjusts the colour scales of the animation frames created with `frames_spatial()` and custom map imagery using the `r_list` argument.
* `join_frames()` side-by-side joins the `ggplot2` objects of two or more frames lists of equal lengths into a single list of `ggplot2` objects per frame using `cowplot::plot_grid`. This is useful if you want to side-by-side combine spatial frames returned by `frames_spatial()` with graph frames returned by `frames_graph()`.

#### Animate frames as GIF or video

* `suggest_formats()` returns a selection of suggested file formats that can be used with `out_file` of `animate_frames()` on your system.
* `animate_frames()` creates an animation from a list of frames computed with `frames_spatial()`, `frames_graph()` or  `join_frames()`.

#### View movement tracks
* `view_spatial()` displays movement tracks on an interactive `mapview` or `leaflet` map.

## Get started

#### Example 1: Creating a simple animation

Create a simple animation using a default base map by first aligning your movement data to a uniform time scale, creating a list of `ggplot2` frames and turning these frames into an animated `GIF`:

```R
library(moveVis)
library(move)
data("move_data")

# align move_data to a uniform time scale
move_data <- align_move(move_data, res = 240, digit = 0, unit = "secs")

# create spatial frames with a OpenStreetMap watercolour map
frames <- frames_spatial(move_data, path_colours = c("red", "green", "blue"),
                         map_service = "osm", map_type = "watercolor", alpha = 0.5)
frames[[100]] # preview one of the frames

# animate frames
animate_frames(frames, out_file = "/full/path/to/output/folder/moveVis_example.gif")
```

<p align="center"><img width="700" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/moveVis_readme/readme_example1_opt.gif"></p>


#### Example 2: Example 1 explained in detail

First, load the required packages for this example and the `moveVis` example movement data:

```R
library(moveVis)
library(move)
library(raster)
library(ggplot2)

data("move_data")
```

`move_data` is a `moveStack`, containing three individual tracks. Let's have a look at both timestamps and sampling rates:

```R
unique(timestamps(move_data))
timeLag(move_data, unit = "mins")
```

We can conclude that each track has a sampling rate of roughly 4 minutes, however sampling rates differ over time. Due to this, tracks do not share unique timestamps. For animation, unique frame times are needed, regardless if we want to animate a single track or multiple at once. Thus, we need to align `move_data` in order to
* make all tracks share unique timestamps that can be assigned to frames
* make all tracks share unique, steady sampling rates without gaps

You can use  <a href="http://movevis.org/reference/align_move.html">`align_move()`</a> to align `move_data` to a sampling rate of 4 minutes (240 seconds) at the seconds digit ":00":

```R
move_data <- align_move(move_data, res = 240, digit = 0, unit = "secs")
```

Instead, you could apply your own functions for aligning your data, e.g. using more advanced interpolation methods.

Now, as the movement tracks are aligned, we can pair them with a base map to create frames that can be turned into an animation later on. You can use your own custom base map imagery or choose from default map types. To get a list of all available default map types, use `get_maptypes()`. `moveVis` supports both `OpenStreetMap` and `mapbox` as default map services.

Using `OpenStreetMap`, you can get open-source streets map imagery and maps derived thereof. Using `mapbox`, you can get a variety of map imagery, including satellite, hybrid, light, dark maps and more. For `mapbox`, you need to register (for free) at https://www.mapbox.com/ to get a token that grants you free access (50 000 map downloads/month) and that can be used with the `map_token` argument of `frames_spatial()` (see <a href = "http://movevis.org/reference/frames_spatial.html">`?frames_spatial`</a> for details). 

In this example, we want to use the OpenStreetMap 'watercolour' imagery with a transparency of 50% to start with something nice looking. To create a list of spatial frames from `move_data` using a map, we can use `frames_spatial()`:

```R
frames <- frames_spatial(move_data, path_colours = c("red", "green", "blue"),
                         map_service = "osm", map_type = "watercolor", alpha = 0.5)
```

Instead of using `path_colours`, you can add a `colour` column to your `move` or `moveStack` object. This allows you to colour your movement tracks as you want, e.g. not only by individual track, but by behavioral segment, time, age, speed or something different (see <a href = "http://movevis.org/reference/frames_spatial.html">`?frames_spatial`</a> for details).

Have a look at the newly created `frames` list object and display a randomly selected frame to get a first impression, how your animation will look like:

```R
length(frames) # number of frames
frames[[100]] # display one of the frames
```

<p align="center"><img width="700" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/moveVis_readme/readme_example1_01.png"></p>

You can pass any list of frames like the one we just created to `animate_frames()`. This function will turn your frames into an animation, written as a GIF image or a video file. For now, we du not want to add any customizations to `frames` and just create a `GIF` from it. If you are not sure, which output formats can be used, run `suggest_formats()` that returns you a vector of file suffixes that can be created on your system. For making a `GIF` from `frames`, just run:

```R
animate_frames(frames, out_file = "/full/path/to/output/folder/moveVis_example.gif")
```

<p align="center"><img width="700" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/moveVis_readme/readme_example1_opt.gif"></p>

#### Example 3: Customizing frames

`moveVis` is entierly based on the `ggplot2` grammar of graphics. Each list element in `frames` is a `ggplot2` object that represents a single animation frame. Thus, it is possible to customize each frame individually using `ggplot2` functions. Instead, `moveVis` provides a set of functions for making it simpler to cutomize frames. We will use some of them in the following to customize `frames` that we created in the prior section:

```R
frames <- add_labels(frames, x = "Longitude", y = "Latitude") # add labels, e.g. axis labels
frames <- add_progress(frames) # add a progress bar
frames <- add_scalebar(frames, height = 0.015) # add a scale bar
frames <- add_northarrow(frames) # add a north arrow
frames <- add_timestamps(frames, move_data, type = "label") # add timestamps

## Have a look at one of the frames:
frames[[100]]
```

<p align="center"><img width="700" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/moveVis_readme/readme_example2_01.png"></p>

For further details on these functions, please see their help files. If you want to apply your own `ggplot2` syntax to `frames`, e.g. for drawing polygons, lines or points that are static or even change with time, you can do this frame-wise. In the following example, we customize one individual frame just as if you would work with a single `ggplot2` object:

```R
data <- data.frame(x = c(8.917, 8.924, 8.924, 8.916, 8.917),
                   y = c(47.7678, 47.7675, 47.764, 47.7646, 47.7678))

# just customize a single frame and have a look at it
frame_test <- frames[[100]] + geom_path(aes(x = x, y = y), data = data,
                                        colour = "red", linetype = "dashed")
frame_test
```

<p align="center"><img width="700" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/moveVis_readme/readme_example2_02.png"></p>


If you just want to change one or a small selection of frames, you could just manipulate those frames like shown above and assign the cusomized `ggplot2` frames to the equivalent elements in your `frames` list.

If you want to edit all frames, you can use the `add_gg()` function. Here, we want to mark a field on the map on all frames. For this, we use the `geom_path()` function of `ggplot2` with `add_gg()`:

```R
# or customize all frames at once using add_gg:
frames = add_gg(frames, gg = expr(geom_path(aes(x = x, y = y), data = data,
                                  colour = "red", linetype = "dashed")), data = data)
```

The field marking is now added to all frames. Let's add some text to describe the field marking:

```R
frames <- add_text(frames, "Static feature", x = 8.9205, y = 47.7633,
                   colour = "black", size = 3)

## Have a look at one of the frames:
frames[[100]]
```

<p align="center"><img width="700" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/moveVis_readme/readme_example2_03.png"></p>

`add_gg()` can also be used to customize each frame consecutively, e.g. to add dynamic marks that move or change with time. Both arguments `gg` and `data` can take lists of the same length as `frames`. If one of these arguments or both are lists, each list element is applied to the according element in `frames`. Let's add a another field mark that is slightly changing with each frame:

```R
## create data.frame containing corner coordinates
data <- data.frame(x = c(8.96, 8.955, 8.959, 8.963, 8.968, 8.963, 8.96),
                   y = c(47.725, 47.728, 47.729, 47.728, 47.725, 47.723, 47.725))
## make a list from it by replicating it by the length of frames
data <- rep(list(data), length.out = length(frames))

## now alter the coordinates to make them shift
data <- lapply(data, function(x){
  y <- rnorm(nrow(x)-1, mean = 0.00001, sd = 0.0001) 
  x + c(y, y[1])
})

## draw each individual polygon to each frame
frames = add_gg(frames, gg = expr(geom_path(aes(x = x, y = y), data = data,
                                  colour = "black")), data = data)

## add a text label
frames <- add_text(frames, "Dynamic feature", x = 8.959, y = 47.7305,
                   colour = "black", size = 3)

## Have a look at one of the frames:
frames[[100]]
```
<p align="center"><img width="700" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/moveVis_readme/readme_example2_04.png"></p>

Animate the the customized frames as we did in the prior section using `animate_frames()`. This time, let's make a `.mov` video:

```R
animate_frames(frames, "/full/path/to/output/folder/moveVis_example_2.mov")
```

<p align="center"><img width="700" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/moveVis_readme/readme_example2_opt.gif"></p>


#### Example 4: Base maps: using mapbox with moveVis

example code to be added soon, see <a href = "http://movevis.org/reference/join_frames.html">`?frames_spatial`</a> for details.

#### Example 5: Custom maps: gradient, discrete and RGB raster data

example code to be added soon, see <a href = "http://movevis.org/reference/join_frames.html">`?frames_spatial`</a> for details.

#### Example 6: Interaction graphs

example code to be added soon, see <a href = "http://movevis.org/reference/join_frames.html">`?frames_graph`</a> for details.

#### Example 7: Joining frames side by side

example code to be added soon, see <a href = "http://movevis.org/reference/join_frames.html">`?join_frames`</a> for details.

#### Example 8: View movement tracks

With the simple `view_spatial()` wrapper, movement tracks can be displayed on an interactive map using the very handy `mapview` or `leaflet` packages. This may be helpful if you want to explore data before animating them or check the effect of applying correction methods as done by `align_move()`.

```R
# in case, mapview or leaflet is not installed:
install.packages(c("leaflet", "mapview"))

library(moveVis)
library(move)
data("move_data")
 
# return a mapview map
view_spatial(move_data)
```

An interactive map is returned. If you use RStudio, it will be displayed on the RStudio viewer pane:

<br>
<p align="center"><img width="600" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/moveVis_readme/readme_example8.png"></p>
<br>

When hovering a point with the cursor, the timestamps of that point is displayed. Thanks to `mapview`, you may click on a point to open a pop-up box containing the point's attributes:

<br>
<p align="center"><img width="600" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/moveVis_readme/readme_example8_02.png"></p>
<br>

## Features to be added

Things and features that should be added in future versions of `moveVis` (feel free to contribute to this list using a pull request):

**Next version:**
* "keep tracks" setting to force paths to not disappear
* follow population mode
* follow individual mode
* day-/night-time visualization

**Some day:**
* 3D animations, e.g. for including altitude data


## Contact & bug reports

For bug reports, please use <https://github.com/16eagle/movevis/issues>. Feature requests and other contributions are welcome!


## What else are we doing?

The Department of Remote Sensing of the University of Würzburg has developed other R packages that might interest you:
 * <a target="_blank" href="http://jxsw.de/getSpatialData">getSpatialData</a>, a package to query, preview and download satellite data,
 * <a target="_blank" href="http://bleutner.github.io/RStoolbox/">RStoolbox</a>, a package providing a wide range of tools for every-day remote sensing processing needs,
 * <a target="_blank" href="https://github.com/RRemelgado/rsMove/">rsMove</a>, a package providing tools to query and analyze movement data using remote sensing.

For other news on the work at at the Department of Remote Sensing of the University of Würzburg, click <a target="_blank" href="http://remote-sensing.eu/">here</a>.


## Acknowledgements
          
This initiative is part of the <a target="_blank" href="https://www.geographie.uni-wuerzburg.de/en/fernerkundung/research/completed-projects/opt4environment/">Opt4Environment</a> project and was funded by the German Aerospace Center (DLR) on behalf of the Federal Ministry for Economic Affairs and Energy (BMWi) with the research grant <b>50 EE 1403</b>.

<p align="justify">
<div>
    <a href="https://www.geographie.uni-wuerzburg.de/en/fernerkundung/startseite/"><img width="21.89781%" src="https://www.uni-wuerzburg.de/typo3conf/ext/uw_sitepackage/Resources/Public/Images/uni-wuerzburg-logo.svg"></a>
    <a href="http://www.dlr.de/eoc/en/"><img width="16.78832%" src="https://upload.wikimedia.org/wikipedia/commons/thumb/f/f5/DLR_Logo.svg/744px-DLR_Logo.svg.png"></a>
     <a href="http://www.bmub.bund.de/"><img width="32.11679%" src="https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRX92Q6lhYFo0Rv7p7Y3obqFXsxRyjXMNKSJ_q9bAvXYdFd5wOF3Q"></a>
    <a href="http://www.orn.mpg.de/en/"><img width="29.19708%" src="https://www.molgen.mpg.de/188611/mpi_Seew_LogoText-1355515314.gif"></a>
</div>
</p>

