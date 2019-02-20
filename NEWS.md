## moveVis 0.10.0
moveVis rewrite introducing a new logic and new functions

<br>
**New features:**
* movement data preparation, frames creation, layout/appearance editing and animation are now seperated, allowing you to customize each of these steps individually.
* any `ggplot2` syntax can now be added to every single frame, allowing you to customize almost everything that can be customized using `ggplot2`
* to make customization easier, a variety of `add_` functions wrapping pre-defined `ggplot2` functions are included. For example, `add_gg` offers dynamic overplotting, e.g. for polygons or points that change over time
* track colours can now be provided as a `colour` column in your movement data, allowing segmentation coulouring or colouring based on a logic, you defined beforhand
* map elements can be positioned freely or individual map-elements can be created as `ggplot2` functions
* Your animation does not have to be a fixed-size squared standard extent: With the `ext` argument of `frames_spatial()` you can define user-defined area of interest (AOI), which scales the frame extent to a specific area
* A new base map feature based on Miles McBains slippymath package. `moveVis` supports Mapbox (free, registration needed) and OpenStreetMap (free) as base map services. See the manual of `frames_spatial()` for details.

<br>
**New functions:**
* `align_move()`, `get_maptypes()`, `frames_spatial()`, `frames_graph()`, `add_gg()`, `add_labels()`, `add_scalebar()`, `add_northarrow()`, `add_progress()`, `add_timestamps()`, `add_text()`, `add_colourscale()`, `join_frames()`, `suggest_formats()`, `animate_frames()`: see the README and the description of the individual functions for further details.

* `subset_move()`: subset movement data for a particular time span
* `df2move()`: simple wrapper that converts data.frames into move or moveStack objects
* `view_spatial()`: view movement data on an interactive map using `mapview` or `leaflet`

<br>
**Deprecated functions:**
* `animate_move()`, `animate_raster()`, `animate_stats()`, `get_formats()`, `get_libraries()`: These functions have been replaced by a new logic and new functions.

***

## moveVis 0.9.9
Reorganizing standard basemap usage by moveVis

<br>
**New features:**
* Static basemaps defined through `map_type` are now retrieved from either OpenStreetMaps for thematic imagery or Microsoft Bing Maps for satellite/hybrid imagery. The usage of OpenStreetMaps is free. For intensive use of the Bing Maps basemap options, please provide your own Bing Maps API key through the `api_key` argument, after registering at Microsoft here: https://msdn.microsoft.com/en-us/library/ff428642.aspx. Google Maps services will be implemented in a future update for restricted usage using an API key.
* New baemaps options via `map_type`: "satellite" (default), "hybrid", "roadmap", "roadmap_dark", "roadmap_bw", "roadmap_watercolor".
* New argument `map_zoom` for `animate_move`: Increase or decrease the degree of detail of a static basemap.
* New argument `map_crop` for `animate_move`: Define, if a static basemap should be cropped to the extent of `m` or if a wider extent of optimal resolution should be used.

<br>
**Bug fixes:**
* Major bug causing moveVis to break with each available static basemap option due to changes of the Google Maps API policies.
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
