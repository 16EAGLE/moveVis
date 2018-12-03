if(length(grep("convert", vars.global$conv_dir)) > 0){
  context("test-animate_move GIF")
  test_that("animate_move GIF", {
    skip_on_cran()
    expect_null(animate_move(vars.global$m, vars.global$out_dir, vars.global$conv_dir, tail_elements = 10,
                             paths_mode = "true_data", frames_nmax = 20,
                             log_level = 3, extent_factor = 0.0002,
                             out_format = "gif"))
  })
}

if(length(grep("ffmpeg", vars.global$conv_dir)) > 0){
  context("test-animate_move MOV")
  test_that("animate_move MOV", {
    skip_on_cran()
    expect_null(animate_move(vars.global$m, vars.global$out_dir, vars.global$conv_dir, tail_elements = 10,
                             paths_mode = "true_data", frames_nmax = 20,
                             log_level = 3, extent_factor = 0.0002,
                             out_format = "mov"))
  })
}