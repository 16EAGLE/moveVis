# moveVis <a href="http://movevis.org"><img align="right" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/moveVis_hex.png" /></a>

[![CRAN version](https://www.r-pkg.org/badges/version/moveVis)](https://CRAN.R-project.org/package=moveVis)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/last-month/moveVis?color=brightgreen)](https://CRAN.R-project.org/package=moveVis)
[![Build Status](https://travis-ci.org/16EAGLE/moveVis.svg?branch=master)](https://travis-ci.org/16EAGLE/moveVis) 
[![Coverage](https://codecov.io/gh/16eagle/moveVis/branch/master/graph/badge.svg)](https://codecov.io/gh/16EAGLE/moveVis)

## Introduction

`moveVis` provides tools to visualize movement data (e.g. from GPS tracking) and temporal changes of environmental data (e.g. from remote sensing) by creating video animations. The `moveVis` package is closely connected to the `move` package and builds up on `ggplot2` grammar of graphics.

## Examples

<br>
<p align="center"><img width="93%" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/examp2.gif"></p>
<p align="center"><sub>Figure 1: White Storks movement nearby Lake Constance, using a dynamic MODIS NDVI layer in the background</sub></p>
<br>

<br>
<p align="center"><img width="100%" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/examp1.gif"></p>
<p align="center"><sub>Figure 2: White Storks movement nearby Lake Constance, using a static land cover/land use map in the background</sub></p>
<br>


## Installation

With version 0.10.0, the package has been rewritten from the ground up with the goal to make it easier to customize the appearance of movement animations. Thus, the logic of the package, its function and their syntax have changed. 

This repository contains version 0.10.0, the newest development version of `moveVis`. Code written for `moveVis` version <=0.9.9 will not work with this repository's version, but it is quite simple and thus highly recommended to switch to the new version due to a variety of advantages. To install `moveVis` version 0.10.0, run:

```r
devtools::install_github("16EAGLE/moveVis")
```

To install `moveVis` version 0.9.9 from `CRAN` and remain with the old syntax, run:

```r
install_packages("moveVis")
```

## Function overview

`moveVis` includes the following functions, sorted by the order they would be applied to create an animation from movement and environmental data:

#### Prepare movement data

* `align_move()` aligns single and multi-individual movement data to a uniform time scale with a uniform temporal resolution needed for creating an animation from it. Use this function to prepare your movement data for animation depending on the temporal resolution that suits your data.

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


## Get started

Example code to be added here and in the help files.


## To do

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
          
This initiative is part of the <a target="_blank" href="http://www.fernerkundung.geographie.uni-wuerzburg.de/forschung/projekte/laufende_projekte/opt4environment">Opt4Environment</a> project and was funded by the German Aerospace Center (DLR) on behalf of the Federal Ministry for Economic Affairs and Energy (BMWi) with the research grant <b>50 EE 1403</b>.

<p align="justify">
<a href="http://www.fernerkundung.geographie.uni-wuerzburg.de/en/lehrstuehle_und_arbeitsgruppen/department_of_remote_sensing/startseite//"><img width="150" height="100" src="https://www.uni-wuerzburg.de/typo3conf/ext/uw_sitepackage/Resources/Public/Images/uni-wuerzburg-logo.svg"></a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="http://www.dlr.de/eoc/en/"><img width="115" height="100" src="https://upload.wikimedia.org/wikipedia/commons/thumb/f/f5/DLR_Logo.svg/744px-DLR_Logo.svg.png"></a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="http://www.bmub.bund.de/"><img width="220" height="100" src="https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRX92Q6lhYFo0Rv7p7Y3obqFXsxRyjXMNKSJ_q9bAvXYdFd5wOF3Q"></a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="http://www.orn.mpg.de/en/"><img width="200" height="100" src="https://www.molgen.mpg.de/188611/mpi_Seew_LogoText-1355515314.gif"></a>

</p>
