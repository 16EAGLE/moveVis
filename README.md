# moveVis <a href="http://movevis.org"><img align="right" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/moveVis_hex.png" /></a>

[![CRAN version](https://www.r-pkg.org/badges/version/moveVis)](https://CRAN.R-project.org/package=moveVis)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/last-month/moveVis?color=brightgreen)](https://CRAN.R-project.org/package=moveVis)
[![Build Status](https://travis-ci.org/16EAGLE/moveVis.svg?branch=master)](https://travis-ci.org/16EAGLE/moveVis) 
[![Coverage](https://codecov.io/gh/16eagle/moveVis/branch/master/graph/badge.svg)](https://codecov.io/gh/16EAGLE/moveVis)

## Introduction

`moveVis` is an R package providing tools to visualize movement data (e.g. from GPS tracking) and temporal changes of environmental data (e.g. from remote sensing) by creating video animations. The package is closely connected to the `move` package and builds up on `ggplot2`. To be informed about updates, new features and the current version, visit [movevis.org](http://movevis.org).

## Examples

<p align="center"><img width="100%" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/examp1.gif"></p>
<p align="center"><sub>Figure 1: Output of animate_move(), showing White Storks movement nearby Lake Constance, using a static land cover/land use map in the background</sub></p>
<br>

<p align="center"><img width="100%" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/examp2.gif"></p>
<p align="center"><sub>Figure 2: Output of animate_move(), showing White Storks movement nearby Lake Constance, using a dynamic MODIS NDVI layer in the background</sub></p>
<br>

## Installation

To install the stable version from CRAN, please run:

```r
install.packages('moveVis')
```

To install the development version from this GitHub repository, please run:

```r
devtools::install_github("16EAGLE/moveVis")
```


## Contact & bug reports

moveVis is being developed and maintained by Jakob Schwalb-Willmann. For bug reports, please use <https://github.com/16eagle/movevis/issues> to contact me. Feature requests and other contributions are also welcome.


## What else are we doing?

The Department of Remote Sensing of the University of Würzburg has developed other R packages that might interest you:
 * <a target="_blank" href="http://jxsw.de/getSpatialData">getSpatgialData</a>, a package to query, preview and download satellite data,
 * <a target="_blank" href="http://bleutner.github.io/RStoolbox/">RStoolbox</a>, a package providing a wide range of tools for every-day remote sensing processing needs,
 * <a target="_blank" href="https://github.com/RRemelgado/rsMove/">rsMove</a>, a package providing tools to query and analyze movement data using remote sensing.

For other news on the work at at the Department of Remote Sensing of the University of Würzburg, click <a target="_blank" href="http://remote-sensing.eu/">here</a>.


## Acknowledgements
          
This initiative is part of the <a target="_blank" href="http://www.fernerkundung.geographie.uni-wuerzburg.de/forschung/projekte/laufende_projekte/opt4environment">Opt4Environment</a> project and was funded by the German Aerospace Center (DLR) on behalf of the Federal Ministry for Economic Affairs and Energy (BMWi) with the research grant <b>50 EE 1403</b>.

<p align="justify">
<a href="http://www.fernerkundung.geographie.uni-wuerzburg.de/en/lehrstuehle_und_arbeitsgruppen/department_of_remote_sensing/startseite//"><img width="150" height="100" src="https://www.uni-wuerzburg.de/typo3conf/ext/uw_sitepackage/Resources/Public/Images/uni-wuerzburg-logo.svg"></a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="http://www.dlr.de/eoc/en/"><img width="115" height="100" src="https://upload.wikimedia.org/wikipedia/commons/thumb/f/f5/DLR_Logo.svg/744px-DLR_Logo.svg.png"></a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="http://www.bmub.bund.de/"><img width="220" height="100" src="https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRX92Q6lhYFo0Rv7p7Y3obqFXsxRyjXMNKSJ_q9bAvXYdFd5wOF3Q"></a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="http://www.orn.mpg.de/en/"><img width="200" height="100" src="https://www.molgen.mpg.de/188611/mpi_Seew_LogoText-1355515314.gif"></a>

</p>
