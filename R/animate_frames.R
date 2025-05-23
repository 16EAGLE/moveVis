#' Animate frames
#'
#' \code{animate_frames} creates an animation from \code{moveVis} frames computed with \code{\link{frames_spatial}}, \code{\link{frames_graph}} or \code{\link{join_frames}}.
#'
#' @inheritParams add_gg
#' @param out_file character, the output file path, e.g. "/dir/to/file.mov". The file extension must correspond to a file format known by the available renderers of the running system. Use \code{\link{suggest_formats}} to get a vector of suggested known file formats.
#' @param fps numeric, the number of frames to be displayed per second. Default is 2.
#' @param width numeric, width of the output animation in pixels.
#' @param height numeric, height of the output animation in pixels.
#' @param res numeric, resolution of the output animation in ppi.
#' @param end_pause numeric, defining how many seconds the last frame of the animation should be hold to add a pause at the the end of the animation. Default is 0 seconds to not add a pause.
#' @param display logical, whether the animation should be displayed after rendering or not.
#' @param overwrite logical, wether to overwrite an existing file, if \code{out_file} is already present.
#' @param ... additional arguments to be passed to the render function.
#'
#' @details An appropriate render function is selected depending on the file extension in \code{out_file}: For \code{.gif} files, \code{gifski::save_gif} is used, for any other (video) format, \code{av::av_capture_graphics} is used.
#'
#' @return None or the default image/video viewer displaying the animation
#' 
#' @importFrom av av_encode_video
#' @importFrom gifski gifski
#' @importFrom ggplot2 quo
#' @importFrom lubridate dseconds
#' @importFrom utils tail head
#' 
#' @author Jakob Schwalb-Willmann
#' 
#' @examples
#' library(moveVis)
#' library(move)
#' 
#' data("m", "basemap_data")
#' # align movement
#' m <- align_move(m, res = 4, unit = "mins")
#' 
#' # create spatial frames with frames_spatial:
#' r_list <- basemap_data[[1]]
#' r_times <- basemap_data[[2]]
#' 
#' \dontrun{
#' frames <- frames_spatial(m, r_list = r_list, r_times = r_times, r_type = "gradient",
#'                          fade_raster = TRUE)
#' 
#' # customize
#' frames <- add_colourscale(frames, type = "gradient",
#'                           colours = c("orange", "white", "darkgreen"), legend_title = "NDVI")
#' frames <- add_northarrow(frames, position = "bottomleft")
#' frames <- add_scalebar(frames, colour = "white", position = "bottomright")
#' 
#' frames <- add_progress(frames)
#' frames <- add_timestamps(frames, m, type = "label")
#' 
#' # check available formats
#' suggest_formats()
#' 
#' # animate frames as GIF
#' animate_frames(frames, out_file = tempfile(fileext = ".gif"))
#' 
#' # animate frames as mov
#' animate_frames(frames, out_file = tempfile(fileext = ".gif"))
#' }
#' @seealso \code{\link{frames_spatial}} \code{\link{frames_graph}} \code{\link{join_frames}}
#' 
#' @export

animate_frames <- function(frames, out_file, fps = 25, width = 700, height = 700, res = 100, end_pause = 0, display = TRUE, overwrite = FALSE, verbose = TRUE, ...){
  
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  if(!inherits(frames, "moveVis")) out("Argument 'frames' needs to be a moveVis frames* object. See e.g. frames_spatial()).", type = 3)
  
  if(!is.character(out_file)) out("Argument 'out_file' must be of type 'character'.", type = 3)
  of_split <- strsplit(out_file, "/")[[1]]
  if(length(of_split) > 1) if(isFALSE(dir.exists(paste0(utils::head(of_split, n = -1), collapse = "/")))) out("Target directory of 'out_file' does not exist.", type = 3)
  if(all(file.exists(out_file), !isTRUE(overwrite))) out("Defined output file already exists and overwriting is disabled.", type = 3)
  num.args <- c(fps = fps, width = width, height = height, res = res)
  catch <- sapply(1:length(num.args), function(i) if(!is.numeric(num.args[[i]])) out(paste0("Argument '", names(num.args)[[i]], "' must be of type 'numeric'."), type = 3))
  
  out_ext <- tolower(utils::tail(unlist(strsplit(out_file, "[.]")), n=1))
  out("Rendering animation...")
  class(frames$m) <- class(frames$m) %>% setdiff("move2")
  if(end_pause > 0){
    n.add <- round(end_pause*fps)
    
    # add frames
    if(length(frames$r) > 1){
      frames$r <- c(frames$r, rep(frames$r[max(frames$m$frame)], n.add))
    }
    
    frames$m <- rbind(
      frames$m,
      do.call(
        rbind, 
        lapply(1:n.add, function(x){
          nf <- frames$m[frames$m$frame == max(frames$m$frame),]
          nf$frame <- nf$frame + 1
          return(nf)
        })
      )
    )
    
    #frames <- append(frames, rep(utils::tail(frames, n = 1), times = n.add))
    out(paste0("Number of frames: ", toString(length(frames)-n.add), " + ", toString(n.add), " to add \u2248 ", toString(dseconds(end_pause)), " of pause at the end"))
  }
  .stats(n.frames = length(frames), fps)
  
  #frames_expr <- expression(moveVis:::.lapply(frames, function(x) quiet(print(x))))
  
  # create PNGs
  frames_dir <- paste0(tempdir(), "/moveVis/frames/")
  dir.create(frames_dir, recursive = T)
  
  tryCatch({
    file <- file.path(frames_dir, "frame_%05d.png")
    grDevices::png(file, width = width, height = height, res = res)
    graphics::par(ask = FALSE)
    .lapply(1:length(frames), function(i){
      quiet(print(frames[[i]]))
    }, moveVis.n_cores = 1)
    grDevices::dev.off()
    frames_files <- list.files(frames_dir, full.names = TRUE)
    
    # animate PNGs
    if(out_ext == "gif"){
      if(length(frames) > 800) out("The number of frames exceeds 800 and the GIF format is used. This format may not be suitable for animations with a high number of frames, since it causes large file sizes. Consider using a video file format instead.", type = 2)
      gifski(frames_files, gif_file = out_file, width = width, height = height, delay = (1/fps), progress = verbose)
      #save_gif(.lapply(frames, function(x) quiet(print(x)), moveVis.n_cores = 1), gif_file = out_file, width = width, height = height, delay = (1/fps), progress = verbose, res = res, ...)
    }else{
      av_encode_video(frames_files, output = out_file, framerate = fps, verbose = verbose, ...)
      #av_capture_graphics(.lapply(frames, function(x) quiet(print(x)), moveVis.n_cores = 1), output = out_file, width = width, height = height, res = res, framerate = fps, verbose = verbose, ...) #, vfilter =' framerate=fps=10') 
    }
  }, error = function(e){
    unlink(frames_dir, recursive = TRUE)
    out(paste0("Error creating animation: ", as.character(e)), type = 3)
  }, finally = unlink(frames_dir, recursive = TRUE))
  
  if(isTRUE(display)) utils::browseURL(out_file)
}