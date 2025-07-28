***

## moveVis 1.0.0
Updating `moveVis` to support `move2` for representing trajectory data and `terra` for representing raster data.

**Changes:**

* Argument `res` of `align_move` now expects a units object for representing the target time resolution (see `?moveVis::align_move`).
* `frames_spatial` now expects `terra` `SpatRasterDataset` for multi-temporal raster data via the `r` argument. See `?moveVis::frames_spatial` for details.

**Deprecations:**

* Arguments `r_list` and `r_times` of `frames_spatial` are deprecated. Use argument `r` instead and supply a `terra` `SpatRasterDataset` to bundle multiple multi-layered rasters of a time series. See `?moveVis::frames_spatial` for details.


## moveVis 0.10.6
New S3 class and methods to represent frames, lazy plotting, improvements.

**New features:**

* Frames are now represented by a new native `moveVis` S3 class that is outputted by all `frames_*()` functions such as `frames_spatial()`. The class comes with native printing and indexing methods. Frames can be easily subsetted (`[`), plotted (`[[`) and checked for length (`length()`). The default print method displays a summary of the created frames.
* With the native `moveVis` class, frames are now rendered lazy when plotted and not being hold in memory. As data duplication has been reduced to the minimum, this change should reduces memory use of `frames_spatial()`. All functions work with the new class. This change mostly effects how things work under the hood and does not alter the user interface.
* added documentation search using `DocSearch` to web page
* Argument `digit` of `align_move()` is deprecated, since digits are now calculated automatically.
* Multiple improvements to `align_move()`, including printing of the detected resolution to which data are aligned.
* Added `path_colours` to `frames_graph()` to be able to colour paths equal to paths in `frames_spatial()`
* Added `basemaps` as dependency for downloading and caching basemaps, removed native basemap retrieval and plotting functions
* Added `trace_size` as argument to `frames_spatial()` to manually control trace size instead of having to use the same size as `tail_size`, e.g. in cases where a tail is not used (i.e. when `tail_length=0`)

**Deprecations:**

* Argument `m` of `add_timestamps()` has been deprecated since the new `moveVis` class contains all necessary data to obtain frame times. Thus, it is not needed anymore and therefore ignored if still being used.

**Bug fixes:**

* Bug that caused `align_move()` to break with an unspecific error message when at least one trajectory of `m` covered a time range shorter than the requested temporal resolution. The function now warns the user when a temporal resolution is selected that cannot be applied to at least one trajectory and excludes the trajectory/trajectories in question from the returned object.
* Bug that caused `frames_spatial()` to interrupt with an error when trying to interpolate `r_list` with `fade_raster=T`
* Fixed bug in adding margin to user `ext` in `frames_spatial()`
* Fixed bug with paths crossing end of grid (aka dateline) when `cross_dateline=TRUE` in `frames_spatial()`
* Fixing bug causing no path to be displayed at all when `tail_length=0` (now correctly showing points without tails)


<br>

***

## moveVis 0.10.5
Revised distance calculations & handling of dateline crossings. Released on CRAN: 2020-03-28

**New features:**

* Argument `cross_dateline` of `frames_spatial()` to indicate that unprojected tracks in lon/lat cross the dateline. Instead of clipping frames at longitudes -180 and 180, `moveVis` extends frames and connects tracks across the dateline.
* Distance calculations are now based on `lwgeom`
* Refined base map plotting (see additional arguments `...` of `frames_spatial()` for available options)
* Added `NA` handling for base maps (e.g. rasterized labels)
* Dropped dependencies

**Bug fixes:**

* Minor errors and warnings caused by reverse dependency changes of package `sf`

<br>

***

## moveVis 0.10.4-1
Bug patch. Released on CRAN: 2020-02-12

**Bug fixes:**

* Major bug introduced with version 0.10.4 that caused memory overload and very slow processing speeds when using `frames_spatial()`

<br>

***

## moveVis 0.10.4
Publication, added parallelization, new memory settings and bug fixes. Released on CRAN: 2020-02-07

**Peer-reviewed publication:**

* An <a href="https://doi.org/10.1111/2041-210X.13374">open-access paper</a> accompanying the `moveVis` R package has been peer-reviewed by and published in *'Methods in Ecology and Evolution'* (see https://doi.org/10.1111/2041-210X.13374).
* Reference: *Schwalb-Willmann, J.; Remelgado, R.; Safi, K.; Wegmann, M. (2020). moveVis: Animating movement trajectories in synchronicity with static or temporally dynamic environmental data in R. Methods in Ecology and Evolution. 2020; 11: 664–669. https://doi.org/10.1111/2041-210X.13374*.
* Citation: Please use `citation("moveVis")` to cite moveVis, e.g. when you use it in publications or presentations.

**New features:**

* Many computationally expensive `moveVis` tasks can now be parallelized, as multi-core support for `moveVis` has been implemented (see `use_multicore()`)
* Added a new example data set `whitestork_data`, representing coordinates and acquisition times of 15 White Storks migrating from Lake of Constance, Germany, to Africa.

**New functions:**

* `use_multicore()` lets you turn on multi-core support of `moveVis` to increase computational time through parallelization.
* `use_disk()` enables the usage of disk space for creating frames. This can prevent memory overload when creating frames for very large animations.

**Bug fixes:**

* Bug that caused a gap between traces and paths.

<br>

***

## moveVis 0.10.3
Bug fixes. Released on CRAN: 2019-10-06.

**New features:**

* Argument `ext` of `frames_spatial()` now clips all tracks to `ext` if `ext` is smaller than the extent of `m`.
* Argument `units` of `add_scalebar()` to switch between `km` and `miles`

**Bug fixes:**

* Bug that caused the scale bar distance calculated by `add_scalebar()` to be `0` for smaller-scale extents. Distance rounding is now iterated by digit to solve this.
* Bug in `frames_spatial()` that resulted in wrong plot extents when an extent smaller than the extent of m was provided using argument 'ext'. In such a case, movement tracks are now clipped to small extents and move outside of the visible frame instead of altering the frame extent.
* Bug that causes traces to cover paths if `trace_show = TRUE`. Traces are now always displayed behind paths.

<br>

***

## moveVis 0.10.2
Adding some small but practical features. Released on CRAN: 2019-04-30.

**New features:**

* Argument `path_alpha` of `frames_spatial()` for defining path transparency.
* Argument `tail_colour` of `frames_spatial()` makes it now possible to define the colour of the last tail element, to which the path colour is faded. Default is "white".
* Argument `trace_show` of `frames_spatial()` for displaying the trace of the complete path (instead that it vanishes after the tail).
* Argument `trace_colour` of `frames_spatial()` for defining the colour of the trace.
* Argument `na.colour` of `add_colourscale()` for defining the colouring of NA values.
* Argument `na.show` of `add_colourscale()` to show or hide NA values in discrete background rasters.
* Argument `end_pause` of `animate_frames()` to hold the last frame, adding a pause of a user-defined duration to the end of an animation.
* Argument `path_fade` of `frames_spatial()` to define whether paths should be faded towards the last frame or not. Useful in combination with `trace_show = T` and when using `end_pause` of `animate_frames()`.
* `moveVis` now stores the timestamps represented by each frame as an attribute `time` for each `ggplot` frame. Frame times can now be accessed using `get_frametimes()`.
* `add_timestamps()` can now extract timestamps directly from frames, which makes defining `m` optional.

**New functions:**

* `get_frametimes()` lets you extract the timestamps associated with each frame of a list of frames created using `frames_spatial` or `frames_graph`.

**Other improvements:**

* `add_colourscale()` now calculates scale and legend with frame-wide fixed limits, when `colours` is defined as a named vector, e.g. `c("-1" = "red", "0" = "blue", "1" = "green")`.
* `moveVis` now displays the approximated duration of an animation on the console, when creating frames or rendering frames.


<br>

***

## moveVis 0.10.1
Bug fixes. Released on CRAN: 2019-04-11.

**Bug fixes:**

* Bug that prevented the use of equal colours for multiple individuals
* Bug that prevented the use of multiple colours per individual, e.g. colouring by behavioral segments/change points
* Bug that caused an error when using `fade_raster = T` in `frames_spatial()` due to unequal vector lengths returned by the internal interpolation function (issue #45)
* Bug that caused false time assignments or an error, if `fade_raster = F` and/or `r_times` contained time elements outside the time range covered by the timestamps of `m`.
<br>

***

## moveVis 0.10.0
moveVis rewrite introducing a new logic and new functions. Released on CRAN: 2019-03-20

<br>
**New features:**

* movement data preparation, frames creation, layout/appearance editing and animation are now seperated, allowing you to customize each of these steps individually.
* any `ggplot2` syntax can now be added to every single frame, allowing you to customize almost everything that can be customized using `ggplot2`
* to make customization easier, a variety of `add_` functions wrapping pre-defined `ggplot2` functions are included. For example, `add_gg` offers dynamic overplotting, e.g. for polygons or points that change over time
* track colours can now be provided as a `colour` column in your movement data, allowing segmentation coulouring or colouring based on a logic, you defined beforhand
* map elements can be positioned freely or individual map-elements can be created as `ggplot2` functions
* Your animation does not have to be a fixed-size squared standard extent: With the `ext` argument of `frames_spatial()` you can define user-defined area of interest (AOI), which scales the frame extent to a specific area
* A new base map feature based on Miles McBains slippymath package. `moveVis` supports Mapbox (free, registration needed). OpenStreetMap and Carto (both free) as base map services. See the manual of `frames_spatial()` for details.
* `frames_spatial()` now takes the argument `equidistant` to define whether the map should be stretched to an equidistant (squared) extent (x and y axis representing same distance) or not (projection-native ratio).
* `frames_spatial()` now uses `coord_sf` to display projections (see details section of `frames_spatial()`).

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
Reorganizing standard basemap usage by moveVis. Released on CRAN: 2018-11-23

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
Updating unit tests for CRAN checks. Released on CRAN: 2018-09-14

<br>
**Bug fixes:**

* Bug causing unit tests to fail on machines with different external tools available
* added SystemRequirements field in DESCRIPTION
<br>

***

## moveVis 0.9.7
Windows library detection bugs fixed. Released on CRAN: 2018-09-07

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
video support, automatic time harmonization, bug fixes. Released on CRAN: 2017-11-01

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
adding frames_layout, static_data etc., improvements to workflow. Released on CRAN: 2017-08-20

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
adding animate_stats() and stats arguments for animate_move(). Released on CRAN: 2017-06-28

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
fixing major bug. Released on CRAN: 2017-06-28

<br>
**Improvements:**

* Fixed a major bug causing an unavoidable error when creating animations with animate_move() using user defined basemaps (Error message: 'Error in eval(expr, envir, enclos) : Object 'frame_l' not found')

<br>

***

## moveVis 0.9.2
adding frame_width and frame_height arguments. Released on CRAN: 2017-06-28

<br>
**New features:**

* Added two new arguments to animate_move() function for changing output frame resolution
 
<br>
**Bug report contributors:**
* vestlink (at) github

<br>

***

## moveVis 0.9.1
fixing important bug; adding animate_raster(). Released on CRAN: 2017-05-23

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
initial release. Released on CRAN: 2017-04-28

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
