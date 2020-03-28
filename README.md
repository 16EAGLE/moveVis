# moveVis <a href="http://movevis.org"><img align="right" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/moveVis_hex.png" /></a>

[![CRAN version](https://www.r-pkg.org/badges/version/moveVis)](https://CRAN.R-project.org/package=moveVis)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/last-month/moveVis?color=brightgreen)](https://CRAN.R-project.org/package=moveVis)
[![CRAN checks](https://cranchecks.info/badges/summary/moveVis)](https://cran.r-project.org/web/checks/check_results_moveVis.html)
[![Build Status](https://travis-ci.org/16EAGLE/moveVis.svg?branch=master)](https://travis-ci.org/16EAGLE/moveVis) 
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/16EAGLE/moveVis?branch=master&svg=true)](https://ci.appveyor.com/project/16EAGLE/moveVis)
[![Coverage](https://codecov.io/gh/16eagle/moveVis/branch/master/graph/badge.svg)](https://codecov.io/gh/16EAGLE/moveVis)
[![Package dependencies](https://tinyverse.netlify.com/badge/moveVis)](https://CRAN.R-project.org/package=moveVis)

## Introduction

<a href="http://movevis.org">`moveVis`</a> provides tools to visualize movement data (e.g. from GPS tracking) and temporal changes of environmental data (e.g. from remote sensing) by creating video animations. It works with <a href="https://github.com/cran/move">`move`</a> and <a href="https://github.com/rspatial/raster">`raster`</a> class inputs and turns them into <a href="https://github.com/tidyverse/ggplot2">`ggplot2`</a> frames that can be further customized. <a href="http://movevis.org">`moveVis`</a> uses <a href="https://github.com/r-rust/gifski">`gifski`</a> (wrapping the <a href="https://gif.ski">gifski</a> cargo crate) and <a href="https://github.com/ropensci/av">`av`</a> (binding to <a href="https://www.ffmpeg.org/">FFmpeg</a>) to render frames into animated GIF or video files. 

A <a href="https://doi.org/10.1111/2041-210X.13374">peer-reviewed open-access paper</a> accompanying `moveVis` has been published in *Methods in Ecology and Evolution*.

<br>
<p align="center"><img width="100%" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/moveVis_readme/examp4.gif"></p>
<p align="center"><sub>Figure 1: Example movement tracks nearby Lake of Constance on top of a OSM watercolor and a mapbox satellite base map</sub></p>
<br>
<br>
<p align="center"><img width="95%" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/moveVis_readme/examp2.gif"></p>
<p align="center"><sub>Figure 2: Example movement tracks nearby Lake of Constance and a gradient base layer faded over time</sub></p>

## Installation

With version 0.10.0, the package has been rewritten from the ground up with the goal to make it easier to customize the appearance of movement animations. Thus, the logic of the package, its functions and their syntax have changed. 

The latest stable version of <a href="http://movevis.org">`moveVis`</a> can be installed from CRAN:

```r
install.packages("moveVis")
```

The development version can be installed from GitHub:

```r
devtools::install_github("16EAGLE/moveVis")
```

Code written for <a href="http://movevis.org">`moveVis`</a> version <=0.9.9 will not work with newer versions, but it is quite simple and thus highly recommended to switch to the new syntax due to a variety of advantages. <a href="http://movevis.org">`moveVis`</a> version <=0.9.9 can still be downloaded <a href="https://github.com/16EAGLE/moveVis/releases/tag/v0.9.9">here</a> and installed manually:

```r
setwd("your/download/directory")
install.packages("moveVis-0.9.9.tar.gz", repos = NULL)
```


## Function overview

<a href="http://movevis.org">`moveVis`</a> includes the following functions, sorted by the order they would be applied to create an animation from movement and environmental data:

#### Preparing movement tracks

* `df2move()` converts a `data.frame` into a `move` or `moveStack` object. This is useful if you do not usually work with the `move` classes and your tracks are present as `data.frames`.
* `align_move()` aligns single and multi-individual movement data to a uniform time scale with a uniform temporal resolution needed for creating an animation from it. Use this function to prepare your movement data for animation depending on the temporal resolution that suits your data.
* `subset_move()` subsets a `move` or `moveStack` by a given time span. This is useful if you want to create a movement animation of only a temporal subset of your data, e.g. a particular day.

#### Creating frames

* `get_maptypes()` returns a character vector of available map types that can be used with `frames_spatial()`. `moveVis` supports OpenStreetMap and Mapbox basemap imagery. Alternatively, you can provide custom imagery to `frames_spatial()`.
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
* `get_frametimes()` extracts the timestamps associated with each frame of a list of frames created using `frames_spatial()` or `frames_graph()` and returns them as a vector.

#### Animating frames (as GIF or video)

* `suggest_formats()` returns a selection of suggested file formats that can be used with `out_file` of `animate_frames()` on your system.
* `animate_frames()` creates an animation from a list of frames computed with `frames_spatial()`, `frames_graph()` or  `join_frames()`.

#### Viewing movement tracks
* `view_spatial()` displays movement tracks on an interactive `mapview` or `leaflet` map.

#### Processing settings
* `use_multicore()` enables multi-core usage for computational expensive processing steps.
* `use_disk()` enables the usage of disk space for creating frames, which can prevent memory overload when creating frames for very large animations.

## Get started

The following example shows how to make a simple animation using a default base map by first aligning your movement data to a uniform time scale, creating a list of `ggplot2` frames and turning these frames into an animated `GIF`:

```R
library(moveVis)
library(move)

data("move_data", package = "moveVis") # move class object
# if your tracks are present as data.frames, see df2move() for conversion

# align move_data to a uniform time scale
m <- align_move(move_data, res = 240, digit = 0, unit = "secs")

# create spatial frames with a OpenStreetMap watercolour map
frames <- frames_spatial(m, path_colours = c("red", "green", "blue"),
                         map_service = "osm", map_type = "watercolor", alpha = 0.5) %>% 
  add_labels(x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
  add_northarrow() %>% 
  add_scalebar() %>% 
  add_timestamps(m, type = "label") %>% 
  add_progress()

frames[[100]] # preview one of the frames, e.g. the 100th frame

# animate frames
animate_frames(frames, out_file = "moveVis.gif")
```

<p align="center"><img width="700" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/moveVis_readme/examp5.gif"></p>

## Examples

You can find code examples on how to use `moveVis` here:

<a href = "http://movevis.org/articles/example-1.html">Example 1: Creating a simple movement animation</a>

<a href = "http://movevis.org/articles/example-2.html">Example 2: Customizing frames</a>

<a href = "http://movevis.org/articles/example-3.html">Example 3: Using a mapbox satellite base map</a>

<a href = "http://movevis.org/articles/example-7.html">Example 4: View movement tracks</a>

## Code snippets

These commented `moveVis` code snippets, addressing specific issues or questions, could also be helpful to you:

<a href = "https://gist.github.com/16EAGLE/8237db3ea0f6e773e8d47bf4ebb201b6">How to hold the last frame of an animation for a defined time and make it look good by using path_fade</a>

<a href = "https://gist.github.com/16EAGLE/16f08531f925f9de2286af277089e3d1">How to display the full traces of each path using trace_show and trace_colour with frames_spatial()</a>

<a href = "https://gist.github.com/16EAGLE/de21779c75aec8be27013f99dc311073">How to colour paths based on a continuous variable</a> 

<a href = "https://gist.github.com/16EAGLE/2a2ad684b3ea2c874cfcb5b364bc573c">How to assign multiple path colours per individual, e.g. to indicate behavioral segments</a>

<a href = "https://gist.github.com/16EAGLE/d69e3bed11fb6d08ee724868710ff876">How to adapt the path legend of frames created with frames_spatial()</a>

<a href = "https://gist.github.com/16EAGLE/1afc1c08d0b2e8696aec5d9f39894266">How to create a data.frame containing each track coordinate per frame</a>

<a href = "https://gist.github.com/16EAGLE/4bfb0ca589204c53041244aa705b456b">How to overlay frames with additional transparent rasters changing over time</a> (hacky, not a very optimal solution)


## Further resources

Detailed code examples explaining how to use specific functions are provided at the <a href="http://movevis.org/reference/index.html">function help pages</a>. User contributions such as code examples or tutorials are very welcome and are linked below as soon as they have been spotted somewhere on the web:

<a target="_blank" href = "http://animove.org/wp-content/uploads/2019/04/Daniel_Palacios_animate_moveVis.html">Animating animal tracks from multiple years over a common year with moveVis: An example with Blue Whale Argos tracks on Movebank</a> by <a target="_blank" href="https://twitter.com/danielequs">Daniel M. Palacios</a>, <a target="_blank" href = "https://mmi.oregonstate.edu/">Marine Mammal Institute</a>, Oregon State University

<a target="_blank" href = "https://chirpscholekster.nl/downloads/movevis_example_code.R">Reproducible example of how to combine animal tracking data, tidal water height data and a heightmap to visualize animal movement with moveVis</a> by <a target="_blank" href="https://twitter.com/hjvdkolk">Henk-Jan van der Kolk</a>, <a target="_blank" href = "https://nioo.knaw.nl/en">The Netherlands Institute of Ecology (NIOO-KNAW)</a>

<a target="_blank" href = "http://movebankworkshopraleighnc.netlify.com/2019outputfiles/envdatavizanimate">How to build animated tracking maps using tracking data in Movebank and environmental covariates in track and raster annotations from EnvDATA with moveVis</a> by <a target="_blank" href="https://www.ab.mpg.de/person/98230">Sarah C. Davidson</a>, Data Curator at <a target="_blank" href = "https://www.movebank.org/">Movebank</a>

## Features to be added

Things and features that should be added in future versions of `moveVis` (feel free to contribute to this list using a pull request):

**Near future:**

* follow population mode
* follow individual mode

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
    <a href="https://www.geographie.uni-wuerzburg.de/en/fernerkundung/startseite/"><img width="26%" src="https://www.uni-wuerzburg.de/typo3conf/ext/uw_sitepackage/Resources/Public/Images/uni-wuerzburg-logo.svg"></a>
    <a href="http://www.dlr.de/eoc/en/"><img width="17%" src="https://upload.wikimedia.org/wikipedia/commons/thumb/f/f5/DLR_Logo.svg/744px-DLR_Logo.svg.png"></a>
     <a href="https://www.bmu.de/en/"><img width="30%" src="https://upload.wikimedia.org/wikipedia/commons/b/b1/Logo_Bundesministerium_f%C3%BCr_Umwelt_Naturschutz_und_nukleare_Sicherheit.png"></a>
    <a href="https://www.ab.mpg.de/"><img width="20%" src="https://upload.wikimedia.org/wikipedia/en/c/c9/Max-Planck-Gesellschaft.svg"></a>
</div>
</p>

## Citation

An <a href="https://doi.org/10.1111/2041-210X.13374">open-access paper</a> accompanying the `moveVis` R package has been peer-reviewed by and published in *'Methods in Ecology and Evolution'* (see https://doi.org/10.1111/2041-210X.13374). Please cite moveVis, e.g. when you use it in publications or presentations, using the output of `citation("moveVis")` or as follows:

*Schwalb-Willmann, J.; Remelgado, R.; Safi, K.; Wegmann, M. (2020). moveVis: Animating movement trajectories in synchronicity with static or temporally dynamic environmental data in R. Methods in Ecology and Evolution. Accepted Author Manuscript. https://doi.org/10.1111/2041-210X.13374*

