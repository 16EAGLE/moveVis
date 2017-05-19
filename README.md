# moveVis
[![CRAN version](https://www.r-pkg.org/badges/version/moveVis)](https://CRAN.R-project.org/package=moveVis)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/last-week/moveVis?color=brightgreen)](https://CRAN.R-project.org/package=moveVis)

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
install.packages('moveVis', repos = 'https://github.com/16eagle/movevis')
```

## Quick guide for movement animation

This guide explains shortly how to prepare your own geo-location point data for the animate_move() function by crearting a move class object from a data.frame class object. As an example, the provided example data (data.frame) are used. Instead, you could use any similar prepared data of yours. First, you will need to load the move and the moveVis package and possibly the example data:

```s
#Load packages
library(move)
library(moveVis)

#Load data (data.frame) (or use your own as data.frame)
data("move_data")
```

As the provided example data, your data.frame data need to have at least three columns: two columns for your coordinates (here "lat", "lon") and one for the date/time stamp (here "dt"). The date/time stamps need to be converted to a POSIXct as follows:

```s
move_data$dt <- as.POSIXct(strptime(move_data$dt, "\%Y-\%m-\%d \%H:\%M:\%S", tz = "UTC"))
```

Your movement data need to be provided as move class objects to the animate_move() function. For each individual movement path you want to display simultaniously within a single aniamtion, you will need one move class object. The move class objects per individual movement path are provided as a list. If your data.frame data contain several individuals as the example data.frame does, subset the data per individual and store there namings. If you just want to display one individual movement path, you do not have to do this.

```s
#Differentiate data per individual
indi_levels <- levels(move_data$individual)
indi_levels_n <- length(indi_levels)
for(i in 1:indi_levels_n){
  if(i == 1){
    indi_subset <- list(subset(move_data, individual == indi_levels[i]))
  }else{
    indi_subset <- c(indi_subset,list(subset(move_data,
                               individual == indi_levels[i])))
  }
}
indi_names <- paste(indi_levels, collapse = ", ")
```

Finally, the move class object list can be created:

```s
#Create move class object
for(i in 1:length(indi_subset)){
  if(i == 1){
     data_ani <- list(move(x=indi_subset[[i]]$lon,y=indi_subset[[i]]$lat,
                                 time=indi_subset[[i]]$dt,
                                 proj=CRS("+proj=longlat +ellps=WGS84"),
                                 animal=indi_levels[i]))
  }else{
     data_ani[i] <- list(move(x=indi_subset[[i]]$lon,y=indi_subset[[i]]$lat,
                                 time=indi_subset[[i]]$dt,
                                 proj=CRS("+proj=longlat +ellps=WGS84"),
                                 animal=indi_levels[i]))}
}
```

The animate_move() function needs to know how to call the convert tool of the ImageMagick software package. By default, animate_move() trys to execute the "convert" command from the command line. To ensure that everythin is going right, you should execute the get_imconvert() function prior to the animate_move() call. The get_imconvert() function checks, if the convert tool can be found on your system and downloads and installs ImageMagick automatically if necessary, depending on your system. Most Linux distributions have ImageMagick preinstalled. Store the output string of get_imconvert() to a variable to be able to hand it over to animate_move().

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

