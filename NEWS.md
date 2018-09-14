## moveVis 1.0.0
Future implementations (not yet implemented!)

<br>
**Next version:**
* "keep tracks" setting to force paths to not disappear
* segmentation coulouring
* optional user-defined font sizes of all fonts (axis, annotations etc.), colours based on ggplot2 syntax
* optional user-defined map elements position vectors

<br>
**Maybe next version:**
* user-defined area of interest (AOI), which scales map extent to specific area without excluding tracks "out of sight", enables move-in and move-out of individuals to AOI
* follow population mode
* follow individual mode
* day-/night-time visualization
* dynamic overplotting, e. g. polygons or points that change over time

<br>
**Some day:**
* 3D animations, e.g. for including altitude data

<br>

***

## moveVis 0.9.8
Updating unit tests for CRAN checks

<br>
**Bug fixes:**
* Bug causing unit tests to fail on machines with different external tools available
* added SystemRequirements field in DESCRIPTION
<br>

***

## moveVis 0.9.7
Windows library detection bugs fixed (dev. version)

<br>
**New features:**
* `time_pos_x` and `time_pos_x` allow to specify the location of the timestamp display
* `time_size` allows to specify the font size of the timestamp display
* added unit tests

<br>
**Bug fixes:**
* Bug causing moveVis to not properly detect installed extern libraries that can be called from the command line
* Bug causing moveVis to not use conv_dir if it contains a Windows path to a tool of an extern library

<br>

***

## moveVis 0.9.6
video support, automatic time harmonization, bug fixes (CRAN version)

<br>
**New functions:**
* `get_libraries()` – handles all extern libraries that are needed for video formats. Just run it to get information on what you need to install. Replaces get_imconvert(), which can still be called for code compatibility reasons
* `get_formats()` – outputs all file formats that can be used with moveVis depending on which libraries are installed on the system.

<br>
**New features:**
* moveVis supports now multiple video formats in addition to GIF, if it is linked to a video library (either FFmpeg of libav)
* animate_move() now detects temporal resolution and uniformity of timestamps automatically to determine, if interpolation needs to be applied to calculate uniform frame times per frame tick
* animate_move() now calculates and displays the final animation duration derived from the total number of output frames and the fps prior to generating each frame, so that the user can already approximate which size the output animation will have.
* added "frames_tres" to animate_move() to change temporal resolution through linear interpolation
* added "frames_pixres" to animate_move() to adjust frame ppi
* added "paths_na.hold" to animate_move() for defining how to deal with data gaps (hold or not hold last path location)
* added "time_bar_col" to animate_move() for changing colouring of the time bar at the top border of the map
* added "out_format" to animate_move() for defining output file format
* added "indi_names" to animate_move() for defining individual names vector manually
* added "scalebar_dist" to animate_move() for defining the scalebar length manually
* added "overwrite" to animate_move() to regulate output file writing behaviour

<br>
**Improvements:**
* it is now possible to use the 'layer' argument to specify one (static) raster object without specifying 'layer_dt', which is only needed, if several raster objects are handed over as a list.
* in some situations, the movement data extent was bigger than the cropped layer extent: this works now
* in some situations, stat animation lines were cut off, this should be resolved now
* recoding of multiple processing steps to increase processing speed and stability
* increased error handler by several errors likely to occure
* several bug fixes

<br>
**Contributors:**
* AniMove 2017 participants

<br>

***

## moveVis 0.9.5
adding frames_layout, static_data etc., improvements to workflow

<br>
**New features:**
* RGB stats support added for animate_stats() and animate_move()
* added animate_move() argument "time_scale" to enable a time scale
* added animate_move() argument "frame_layout" for user friendly, complex layouting of the output GIF by specifiying the plot locations
* added animate_move() argument "layer_stretch"
* added animate_move() expert arguments "conv_cmd" and "conv_frames"

<br>
**Improvements:**
* several major bug fixes
* major animate_raster() bug fixes, reenabling use of RGB rasterStack inputs
* stats legend is now locatable using frame_layout
* fixed a bug of get_imconvert() that caused it to have issues with finding convert.exe on Windows machines, if ImageMagick is preinstalled

<br>
**Contributors:**
* Bart Kranstauber

<br>

***

## moveVis 0.9.4
adding animate_stats() and stats arguments for animate_move()

<br>
**New features:**
* `animate_stats()` – Enables creation of statistic plot animations visualizing possible relationships between movement paths and basemap. Define individual plot designs based on ggplot2 syntax.
* stats arguments for animate_move(): Enables statistic plot animations side-by-side with the spatial plot animation of animate_move. Use the animate_stats() arguments with animate_move().
* extent_factor argument for animate_move(): Increase the distance between the spatial extents of the movement paths and the basemap.

<br>
**Improvements:**
* Several minor bug fixes

<br>

***

## moveVis 0.9.3
fixing major bug

<br>
**Improvements:**
* Fixed a major bug causing an unavoidable error when creating animations with animate_move() using user defined basemaps (Error message: 'Error in eval(expr, envir, enclos) : Object 'frame_l' not found')

<br>

***

## moveVis 0.9.2
adding frame_width and frame_height arguments

<br>
**New features:**
* Added two new arguments to animate_move() function for changing output frame resolution
 
<br>
**Bug report contributors:**
* vestlink (at) github

<br>

***

## moveVis 0.9.1
fixing important bug; adding animate_raster()

<br>
**New features:**
* Added moveStack support: animate_move() now takes also moveStack objects in addition to list objects
* `animate_raster()` – for animating just basemaps/for doing the same as with animate_move(), but without movement data

<br>
**Improvements:**
* Bug fix: Function now takes movebank.org data --> there had been a static code line preventing the read-out of movement coordinates, which is now solved.
* Several major and minor bug fixes

<br>
**Bug report contributors:**
* bart1 (at) github
* Patrick Scherler

<br>

***

## moveVis 0.9.0
initial release

<br>
**Initial features:**
* `animate_move()`
* `get_imconvert()`

<br>
**Initial example data sets:**
* data(move_data)

<br>

***
This document should provide a broad overview on changes that are applied to the moveVis R package. There is no warranty for completeness, since minor changes might not be included. All improvement and feature descriptions are bundled per release version. The document is currently maintained by Jakob Schwalb-Willmann.
