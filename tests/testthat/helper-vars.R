map_token = Sys.getenv("moveVis.map_token")
if(map_token != "") run_mapbox <- TRUE else run_mapbox <- FALSE

test_dir = Sys.getenv("moveVis.test_dir")
if(test_dir != ""){ if(!dir.exists(test_dir)) dir.create(test_dir)} else{ test_dir <- tempdir()}

data("move_data", package = "moveVis", envir = environment())
data("basemap_data", package = "moveVis", envir = environment())

## some pretty bad and boring fake movement data for testing
m.df <- rbind(data.frame("lat" = seq(47.7500, 47.7600, length.out = 210),
                         "lon" = seq(8.9295, 8.9500, length.out = 210),
                         "time" = seq.POSIXt(ISOdatetime(2018, 5, 14, 10, 54, 21, tz = "UTC"), by = "58 min", length.out = 210),
                         "name" = "hinz", "colour" = "green"),
              data.frame("lat" = seq(47.7500, 47.7600, length.out = 210)+0.02,
                         "lon" = seq(8.9295, 8.9500, length.out = 210),
                         "time" = seq.POSIXt(ISOdatetime(2018, 5, 13, 5, 55, 13, tz = "UTC"), by = "1 hour", length.out = 210),
                         "name" = "kunz",
                         "colour" = "red"),
              data.frame("lat" = seq(47.7500, 47.7600, length.out = 210)-0.02,
                         "lon" = seq(8.9295, 8.9500, length.out = 210),
                         "time" = seq.POSIXt(ISOdatetime(2018, 5, 15, 5, 55, 00, tz = "UTC"), by = "66 min", length.out = 210),
                         "name" = "heinz",
                         "colour" = "blue"),
              data.frame("lat" = seq(47.7500, 47.7600, length.out = 210)-0.04,
                         "lon" = seq(8.9295, 8.9500, length.out = 210),
                         "time" = seq.POSIXt(ISOdatetime(2018, 5, 25, 0, 00, 00, tz = "UTC"), by = "54 min", length.out = 210),
                         "name" = "rob",
                         "colour" = "blue"))

m <- move::move(x = m.df$lon, y = m.df$lat, time = m.df$time, data = m.df, animal = m.df$name,
                proj = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
m.aligned <- align_move(m)
ts <- sort(unique(timestamps(m)))

## some fake-dated raster base maps
r_list = basemap_data[[1]]
r_times <- ts[seq(1, length(ts), length.out = length(r_list))]
