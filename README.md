# moveVis
[![CRAN version](https://www.r-pkg.org/badges/version/moveVis)](https://CRAN.R-project.org/package=moveVis)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/last-month/moveVis?color=brightgreen)](https://CRAN.R-project.org/package=moveVis)

## Introduction

This is an R package providing tools to visualize movement data by creating path animations from geo-location point data. The package is under ongoing development, new functions including statistics visualaization will be added soon. The moveVis package is working hand in hand with the move package by using the move class. It is partly based on the animation package and uses a ggplot2 based plot architecture for frame creation.

## Installation

This is the official moveVis R package repository, including beta code versions before submitted to CRAN. For operational use of moveVis, please use the current stable CRAN version of moveVis.

To install stable version from CRAN, please execute:

```s
install.packages('moveVis')
```

To install the development version from this GitHub repository, please execute:

```s
devtools::install_github("16EAGLE/moveVis")
```

## Quick Guide for Movement Animation

This guide shortly explains how to prepare your own geo-location point data for the animate_move() function by creating a move class object from a data.frame class object. As an example, the provided example data (data.frame) are used. Instead, you could use any similar prepared data of yours. First, you will need to load the move and the moveVis package and possibly the example data:

```s
#Load packages
library(move)
library(moveVis)

#Load data (data.frame) (or use your own as data.frame)
data("move_data")
```

As the provided example data, your data.frame needs to have at least three columns: two columns for your coordinates (here "lat", "lon") and one for the date/time stamp (here "dt"). The date/time stamps need to be converted to POSIXct as follows:

```s
move_data$dt <- as.POSIXct(strptime(move_data$dt, "\%Y-\%m-\%d \%H:\%M:\%S", tz = "UTC"))
```

Your movement data need to be provided as move class objects to the animate_move() function. For each individual movement path you want to display simultaniously within a single animation, you will need one move class object. The move class objects per path are provided as a list. If your data.frame contains several individuals (e. g. differentiable by a "individuals" column, as the example data.frame does), then subset the data per individual and store the namings. If you just want to display a single path, you do not have to do this.

```s
#Differentiate data per individual
data_ani <- split(move(move_data$lon, move_data$lat, proj=CRS("+proj=longlat +ellps=WGS84"),
	time = move_data$dt, animal=move_data$individual, data=move_data))
```

Please note that the animate_move() function needs to know how to call the convert tool of the ImageMagick software package. By default, animate_move() trys to execute the "convert" command from the command line. To ensure that everything is going right, you should execute the get_imconvert() function prior to the animate_move() call (at least if you run it first). The get_imconvert() function checks, if the convert tool can be found on your system and downloads and installs ImageMagick automatically if necessary, depending on your system. Most Linux distributions have ImageMagick preinstalled. Store the output string of get_imconvert() to a variable to be able to hand it over to animate_move(). If you know the convert tool command or its directory, you can also specify it manually (see the "conv_dir" argument of animate_move()).

```s
#Find command or directory to convert tool of ImageMagick
conv_dir <- get_imconvert()
```

Last, you need to specify the output directory path and you can specify some optional variables such as the animation title (for details on all the arguments of animate_move() , read the animate_move() help).

```s
#Specify output directory
out_dir <- "/your/full/output/directory"

#Specify some optional appearance variables
img_title <- "Movement of the white stork population at Lake Constance, Germany"
img_sub <- paste0("including individuals ",indi_names)
img_caption <- "Projection: Geographical, WGS84; Sources: Movebank 2013; Google Maps"
```

Finally, you are now prepared to call animate_move(), which will have to work for a while depending on your input. Here, we use "frames_nmax" set to 50 to force the function to only produce 50 frames and then finish the GIF, regardless how many  input points you provided. Set "log_level" to 1 to be informed of anything the function is doing.

```s
#Call animate_move()
animate_move(data_ani, out_dir, conv_dir = conv_dir, tail_elements = 10,
             paths_mode = "simple", frames_nmax = 50,
             img_caption = img_caption, img_title = img_title,
             img_sub = img_sub, log_level = 1)
```

## Bug reports & contact

For bug reports, please use <https://github.com/16eagle/movevis/issues>. Feature requests and other contributions are also welcome.

## What else are we doing?

The Department of Remote Sensing of the University of Würzburg has developed other R packages that might interest you:
 * <a target="_blank" href="http://bleutner.github.io/RStoolbox/">RStoolbox</a>
 * <a target="_blank" href="https://github.com/RRemelgado/rsMove/">rsMove</a>

For other news on the work at at the Department of Remote Sensing of the University of Würzburg, click <a target="_blank" href="http://remote-sensing.eu/">here</a>.

## Aknowledgements
          
This initiative is part of the <a target="_blank" href="http://www.fernerkundung.geographie.uni-wuerzburg.de/en/lehrstuehle_und_arbeitsgruppen/department_of_remote_sensing/research/projects/current_projects/opt4environment//">Opt4Environment</a> project and was funded by the German Aerospace Center (DLR) on behalf of the Federal Ministry for Economic Affairs and Energy (BMWi) with the research grant <b>50 EE 1403</b>.

<p align="justify">
<a href="http://www.fernerkundung.geographie.uni-wuerzburg.de/en/lehrstuehle_und_arbeitsgruppen/department_of_remote_sensing/startseite//"><img width="150" height="100" src="https://www.uni-wuerzburg.de/typo3conf/ext/uw_sitepackage/Resources/Public/Images/uni-wuerzburg-logo.svg"></a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="http://www.eagle-science.org"><img width="100" height="100" src="http://remote-sensing.eu/wp-content/uploads/2016/05/eagle_master_full_noText.png"></a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="http://www.dlr.de/eoc/en/"><img width="115" height="100" src="https://upload.wikimedia.org/wikipedia/commons/thumb/f/f5/DLR_Logo.svg/744px-DLR_Logo.svg.png"></a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="http://www.bmub.bund.de/"><img width="220" height="100" src="https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRX92Q6lhYFo0Rv7p7Y3obqFXsxRyjXMNKSJ_q9bAvXYdFd5wOF3Q"></a>
</p>

