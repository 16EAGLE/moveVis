#' Render an individual frame
#' 
#' This function renders an individual frame. It yields the same result as if an individual frame is extracted using default subsetting \code{[[]]}.
#' 
#' @param frames frames as an object of class moveVis.
#' @param x frames as an object of class moveVis.
#' @param i numeric, index number of the frame to be rendered.
#' @param ... additional arguments, currently not used.
#' 
#' @export
#' 
#' @importFrom patchwork wrap_plots
#' @importFrom basemaps gg_raster
#' 
#' @examples
#' library(moveVis)
#' library(move2)
#' library(terra)
#'  
#' data("move_data", package = "moveVis")
#' r <- readRDS(example_data(file = "raster_NDVI.rds"))
#'  
#' # align movement
#' m <- align_move(move_data, res = units::set_units(4, "min"))
#'  
#' # create spatial frames 
#' frames <- frames_spatial(m, r, r_type = "gradient", fade_raster = TRUE)
#'  
#' # viewing frames calling this function:
#' render_frame(frames) # displays the last frame 
#' render_frame(frames, i = 100) # displays frame 100 
#'  
#' # alternatively, you can simply use `[[` to do this:
#' frames[[100]] # displays frame 100
render_frame <- function(frames, i = length(frames)){
  
  # checking subscript
  if(length(i) > 1) out("Subscript must be of length 1.", type = 3)
  if(i > length(frames)) out(paste0("Subscript out of bounds. Length of frames is ", length(frames), "."), type = 3)
  
  # make sure there always is a correct engine selected
  engine <- "ggplot2" # make this an argument at some point
  if(engine == "ggplot2"){
    if(inherits(frames, "frames_spatial")){
      gg <- gg.spatial(
        x = .df4gg(
          m = frames$m,
          i = i,
          tail_length = frames$aesthetics$tail_length,
          path_size = frames$aesthetics$path_size,
          tail_size = frames$aesthetics$tail_size,
          tail_colour = frames$aesthetics$tail_colour,
          trace_show = frames$aesthetics$trace_show,
          trace_size = frames$aesthetics$trace_size,
          trace_colour = frames$aesthetics$trace_colour,
          path_fade = frames$aesthetics$path_fade),
        y = gg_raster(
          r = frames$r[[if(length(frames$r) > 1) i else 1]],
          r_type = frames$aesthetics$r_type,
          maxpixels = frames$aesthetics$maxpixels,
          alpha = frames$aesthetics$alpha,
          maxColorValue = frames$aesthetics$maxColorValue,
          interpolate = frames$aesthetics$interpolate,
          add_coord = FALSE),
        m_names = frames$m$name,
        m_colour = frames$m$colour,
        path_end = frames$aesthetics$path_end,
        path_join = frames$aesthetics$path_join,
        path_mitre = frames$aesthetics$path_mitre,
        path_arrow = frames$aesthetics$path_arrow,
        path_alpha = frames$aesthetics$path_alpha,
        path_legend = frames$aesthetics$path_legend,
        path_legend_title = frames$aesthetics$path_legend_title,
        path_size = frames$aesthetics$path_size,
        equidistant = frames$aesthetics$equidistant,
        tail_length = frames$aesthetics$tail_length
      )
    }
    if(inherits(frames, "frames_graph")){
      if(frames$graph_type == "flow"){
        gg <- .gg_flow(
          x = frames$m[frames$m$frame <= i,],
          y = frames$m,
          path_legend = frames$aesthetics$path_legend,
          path_legend_title = frames$aesthetics$path_legend_title,
          path_size = frames$aesthetics$path_size,
          val_seq = frames$aesthetics$val_seq
        )
      }
      if(frames$graph_type == "hist"){
        gg <- .gg_hist(
          x = frames$hist_data[[i]],
          y = do.call(rbind, frames$hist_data),
          path_legend = frames$aesthetics$path_legend,
          path_legend_title = frames$aesthetics$path_legend_title,
          path_size = frames$aesthetics$path_size,
          val_seq = frames$aesthetics$val_seq,
          r_type = frames$aesthetics$r_type
        )
      }
    }
    if(inherits(frames, "frames_joined")){
      .plots <- lapply(1:length(frames$frames_list), function(ii){
        if(!frames$render_all_legends){
          if(ii != 1) frames$frames_list[[ii]]$aesthetics$path_legend <- FALSE
        }
        frames$frames_list[[ii]][[i]]
      })
      
      gg <- wrap_plots(
        lapply(.plots, function(.x) .x), guides = frames$wrap_plots_guides, design = frames$wrap_plots_design, 
        frames$wrap_plot_args
      )
    }
      
    # any additions?
    if(!is.null(frames$additions)){
      for(ix in 1:length(frames$additions)){
        .x <- frames$additions[[ix]]
        if(length(.x[["arg"]]) > 0) for(j in 1:length(.x[["arg"]])) assign(names(.x[["arg"]])[[j]], .x[["arg"]][[j]])
        if(length(.x[["data"]]) > 0) assign("data", .x[["data"]][[i]])
        gg <- gg + eval(.x[["expr"]][[i]])
      }
    }
    return(gg)
  }
}
