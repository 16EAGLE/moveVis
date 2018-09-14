if(length(grep("convert", vars.global$conv_dir)) > 0){
  context("test-animate_raster (gradient) GIF")
  test_that("animate_raster (gradient) GIF", {
    expect_null(animate_raster(vars.global$r_bm, out_dir = vars.global$out_dir, conv_dir = vars.global$conv_dir, layer_type = "gradient",
                               out_format = "gif", out_name = "mVar"))
  })
  test_that("animate_raster (gradient) with timestamps GIF", {
    expect_null(animate_raster(vars.global$r_bm, layer_dt = vars.global$dt_bm, out_dir = vars.global$out_dir, conv_dir = vars.global$conv_dir, layer_type = "gradient",
                               out_format = "gif", out_name = "mVar_dt"))
  })
}

if(length(grep("ffmpeg", vars.global$conv_dir)) > 0){
  context("test-animate_raster (gradient) MOV")
  test_that("animate_raster (gradient) MOV", {
    expect_null(animate_raster(vars.global$r_bm, out_dir = vars.global$out_dir, conv_dir = vars.global$conv_dir, layer_type = "gradient",
                               out_format = "mov", out_name = "mVar"))
  })
  test_that("animate_raster (gradient) with timestamps MOV", {
    expect_null(animate_raster(vars.global$r_bm, layer_dt = vars.global$dt_bm, out_dir = vars.global$out_dir, conv_dir = vars.global$conv_dir, layer_type = "gradient",
                               out_format = "mov", out_name = "mVar_dt"))
  })
}
