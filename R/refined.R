source("https://raw.githubusercontent.com/16EAGLE/16EAGLE/master/R/base.R")
r_load(c("raster", "sf", "move", "ggplot2", "gganimate", "gifski", "gapminder", "magick", "av", "slippymath",
         "curl", "glue", "purrr", "RStoolbox", "geosphere"))

# fake movemenet
m.coord <- cbind(seq(47.7500, 47.7600, length.out = 1010), seq(8.9295, 8.9500, length.out = 1010))
m <- rbind.data.frame(cbind(m.coord, 1, 1:nrow(m.coord)),
                      cbind(m.coord[,1]+0.02, m.coord[,2], 2, 1:nrow(m.coord)),
                      cbind(m.coord[,1]-0.02, m.coord[,2], 3, 1:nrow(m.coord)), stringsAsFactors = F)
colnames(m) <- c("lat", "lon", "id", "frame")
m <- m[order(m$frame),]

# calcualte square extent
m.ext <- st_bbox(c(xmin = min(m$lon), xmax = max(m$lon), ymin = min(m$lat), ymax = max(m$lat)), crs = st_crs("+proj=longlat +ellps=WGS84"))
gg.ext <- .squared(m.ext, margin_factor = 1.1)

# calculate tiles and get mapbox imagery
tg <- bb_to_tg(gg.ext, max_tiles = 20)
q <- paste0("https://api.mapbox.com/v4/mapbox.satellite/{zoom}/{x}/{y}.jpg90", "?access_token=", token)
images <-pmap(tg$tiles, function(x, y, zoom){
  outfile <- glue("/home/UNI-WUERZBURG.EU/jas24nx/Documents/tmp/{x}_{y}.jpg")
  curl_download(url = glue(q), destfile = outfile) 
  return(outfile)
},zoom = tg$zoom)

# composite imagery
r <- tg_composite(tg, images)
r <- crop(projectRaster(r, crs = crs("+proj=longlat +ellps=WGS84")), extent(gg.ext[1], gg.ext[3], gg.ext[2], gg.ext[4]))

# split m by size of tail, requires m with col lon, lat, id and frame time (integer)
m.split <- .split(m, n_tail = 19, cols = c("green", "red", "blue"))

# create frames function
test <- .gg(m.split[1:30], ggRGB(r, 1, 2, 3, ggObj = T), print_plot = F)

# render video
av_capture_graphics(.gg(m.split[1:30], ggRGB(r, r = 1, g = 2, b = 3, ggObj = T)),
                    file.path(tempdir(), 'x.mp4'), 900, 800, res = 144, framerate = 20) #, vfilter =' framerate=fps=10')
utils::browseURL(video_file)
