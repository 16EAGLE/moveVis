# moveVis <a href="http://movevis.org"><img align="right" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/moveVis_hex.png" /></a>

[![CRAN version](https://www.r-pkg.org/badges/version/moveVis)](https://CRAN.R-project.org/package=moveVis)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/last-month/moveVis?color=brightgreen)](https://CRAN.R-project.org/package=moveVis)
[![Build Status](https://travis-ci.org/16EAGLE/moveVis.svg?branch=master)](https://travis-ci.org/16EAGLE/moveVis) 
[![Coverage](https://codecov.io/gh/16eagle/moveVis/branch/master/graph/badge.svg)](https://codecov.io/gh/16EAGLE/moveVis)

## Introduction

<a href="http://movevis.org">`moveVis`</a> provides tools to visualize movement data (e.g. from GPS tracking) and temporal changes of environmental data (e.g. from remote sensing) by creating video animations. It works with <a href="https://github.com/cran/move">`move`</a>, <a href="https://github.com/edzer/sp">`sp`</a> and <a href="https://github.com/rspatial/raster">`raster`</a> class inputs and turns them into <a href="https://github.com/tidyverse/ggplot2">`ggplot2`</a> frames that can be further customized. <a href="http://movevis.org">`moveVis`</a> uses <a href="https://github.com/r-rust/gifski">`gifski`</a> (wraping the <a href="https://crates.io/crates/gifski">gifski</a> cargo crate) and <a href="https://github.com/ropensci/av">`av`</a> (binding to <a href="https://www.ffmpeg.org/">FFmpeg</a>) to render frames into animated GIF or video files.

<br>
<p align="center"><img width="100%" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/moveVis_readme/examp4.gif"></p>
<p align="center"><sub>Figure 1: Exemplary movement tracks nearby Lake of Constance on top of a OSM watercolor and a mapbox satellite base map</sub></p>
<br>
<br>
<p align="center"><img width="95%" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/moveVis_readme/examp2.gif"></p>
<p align="center"><sub>Figure 2: Exemplary movement tracks nearby Lake of Constance and a gradient base layer faded over time</sub></p>

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

<p align="center"><img width="100%" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/moveVis_readme/workflow_nmargin.png"></p>

<a href="http://movevis.org">`moveVis`</a> includes the following functions, sorted by the order they would be applied to create an animation from movement and environmental data:

#### Preparing movement tracks

* `df2move()` converts a `data.frame` into a `move` or `moveStack` object. This is useful if you do not usually work with the `move` classes and your tracks are present as `data.frames`.
* `align_move()` aligns single and multi-individual movement data to a uniform time scale with a uniform temporal resolution needed for creating an animation from it. Use this function to prepare your movement data for animation depending on the temporal resolution that suits your data.
* `subset_move()` subsets a `move` or `moveStack` by a given time span. This is useful if you want to create a movement animation of only a temporal subset of your data, e.g. a particular day.

#### Creating frames

* `get_maptypes()` returns a character vector of available map types that can be used with `frames_spatial()`. `moveVis` supports OpenStreetMaps and Mapbox basemap imergay. Alternatively, you can provide custom imagery to `frames_spatial()`.
* `frames_spatial()` creates a list of `ggplot2` maps displaying movement. Each object represents a single frame. Each frame can be viewed or modified individually. The returned list of frames can be animated using `animate_frames()`.
* `frames_graph()` creates a list of `ggplot2` graphs displaying movement-environment interaction. Each object represents a single frame. Each frame can be viewed or modified individually. The returned list of frames can be animated using `animate_frames()`.

#### Adapting frames

* `add_gg()` adds `ggplot2` functions (e.g. to add layers such as points, polygons, lines, or to change scales etc.) to the animation frames created with `frames_spatial()` or `frames_graph()`. Instead of creating your own `ggplot2` functions, you can use one of the other `moveVis` `add_``functions:
* `add_labels()` adds character labels such as title or axis labels to animation frames created with `frames_spatial()` or `frames_graph()`.
* `add_scalebar()` adds a scalebar to the animation frames created with `frames_spatial()` or `frames_graph()`.
* `add_northarrow()` adds a north arrow to the animation frames created with `frames_spatial()` or `frames_graph()`.
* `add_progress()` adds a progress bar to animation frames created with `frames_spatial()` or `frames_graph()`.
* `add_timestamps()` adds timestamps to animation frames created with `frames_spatial()` or `frames_graph()`.
* `add_text()` adds static or dynamically changing text to the animation frames created with `frames_spatial()` or `frames_graph()`.
* `add_colourscale()` adjusts the colour scales of the animation frames created with `frames_spatial()` and custom map imagery using the `r_list` argument.
* `join_frames()` side-by-side joins the `ggplot2` objects of two or more frames lists of equal lengths into a single list of `ggplot2` objects per frame using `cowplot::plot_grid`. This is useful if you want to side-by-side combine spatial frames returned by `frames_spatial()` with graph frames returned by `frames_graph()`.

#### Animating frames (as GIF or video)

* `suggest_formats()` returns a selection of suggested file formats that can be used with `out_file` of `animate_frames()` on your system.
* `animate_frames()` creates an animation from a list of frames computed with `frames_spatial()`, `frames_graph()` or  `join_frames()`.

#### Viewing movement tracks
* `view_spatial()` displays movement tracks on an interactive `mapview` or `leaflet` map.

## Get started

The following example shows how to make a simple animation using a default base map by first aligning your movement data to a uniform time scale, creating a list of `ggplot2` frames and turning these frames into an animated `GIF`:

```R
library(moveVis)
library(move)
data("move_data") # move class object
# if your tracks are present as data.frames, see df2move() for conversion

# align move_data to a uniform time scale
move_data <- align_move(move_data, res = 240, digit = 0, unit = "secs")

# create spatial frames with a OpenStreetMap watercolour map
frames <- frames_spatial(move_data, path_colours = c("red", "green", "blue"),
                         map_service = "osm", map_type = "watercolor", alpha = 0.5)
frames[[100]] # preview one of the frames

# animate frames
animate_frames(frames, out_file = "/full/path/to/example_1.gif")
```

<p align="center"><img width="700" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/moveVis_readme/readme_example1_opt.gif"></p>

## Examples

You can find detailed code examples on how to use `moveVis` here:

<a href = "http://movevis.org/articles/example-1.html">Example 1: Creating a simple movement animation</a>

<a href = "http://movevis.org/articles/example-2.html">Example 2: Customizing frames</a>

<a href = "http://movevis.org/articles/example-3.html">Example 3: Using a mapbox satellite base map</a>

<a href = "http://movevis.org/articles/example-4.html">Example 4: Custom base maps from raster data</a> (to be added soon)

<a href = "http://movevis.org/articles/example-5.html">Example 5: Interaction graphs</a> (to be added soon)

<a href = "http://movevis.org/articles/example-6.html">Example 6: Joining frames side by side</a> (to be added soon)

<a href = "http://movevis.org/articles/example-7.html">Example 7: View movement tracks</a>

## Code snippets

These commented `moveVis` code snippets, addressing specific issues or questions, could also be helpful to you:

<a href = "https://gist.github.com/16EAGLE/d69e3bed11fb6d08ee724868710ff876">How to adapt the path legend of frames created with frames_spatial()</a>

<a href = "https://gist.github.com/16EAGLE/1afc1c08d0b2e8696aec5d9f39894266">How to create a data.frame containing each track coordinate per frame</a>

<a href = "https://gist.github.com/16EAGLE/4bfb0ca589204c53041244aa705b456b">How to overlay frames with additional transparent rasters changing over time</a>

## Features to be added

Things and features that should be added in future versions of `moveVis` (feel free to contribute to this list using a pull request):

**Next version:**
* "keep tracks" setting to force paths to not disappear
* follow population mode
* follow individual mode
* day-/night-time visualization

**Some day:**
* 3D animations, e.g. for including altitude data

## Related packages

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

