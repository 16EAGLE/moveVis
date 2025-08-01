skip_on_cran()
context("deprecated")

#if("deprecated" %in% which_tests){
test_that("depreacated functions", {
  expect_error(animate_move())
  expect_error(animate_stats())
  expect_error(animate_raster())
  expect_error(get_formats())
  expect_error(get_libraries())
  expect_error(subset_move())
  expect_error(df2move())
})
#}