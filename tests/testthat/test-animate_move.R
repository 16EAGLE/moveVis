context("test-animate_move GIF")
test_that("animate_move GIF", {
  expect_null(animate_move(vars.global$m, vars.global$out_dir, vars.global$conv_dir, tail_elements = 10,
                           paths_mode = "true_data", frames_nmax = 20,
                           log_level = 3, extent_factor = 0.0002,
                           out_format = "gif"))
})

context("test-animate_move MOV")
test_that("animate_move MOV", {
  expect_null(animate_move(vars.global$m, vars.global$out_dir, vars.global$conv_dir, tail_elements = 10,
                           paths_mode = "true_data", frames_nmax = 20,
                           log_level = 3, extent_factor = 0.0002,
                           out_format = "mov"))
})