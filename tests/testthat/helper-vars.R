data("move_data", package = "moveVis", envir = environment())

move_data$dt <- as.POSIXct(strptime(move_data$dt, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
vars.global <- list(m = move::move(move_data$lon, move_data$lat, proj=sp::CRS("+proj=longlat +ellps=WGS84"),
                                   time = move_data$dt, animal=move_data$individual, data=move_data),
                    conv_dir = get_libraries(),
                    out_dir = paste0(tempdir(), "/tests"))
if(!dir.exists(vars.global$out_dir)) dir.create(vars.global$out_dir)