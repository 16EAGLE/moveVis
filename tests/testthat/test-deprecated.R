skip_on_cran()
context("deprecated")

# if("deprecated" %in% which_tests){
test_that("depreacated functions", {
  expect_warning(animate_move())
  expect_warning(animate_stats())
  expect_warning(animate_raster())
  expect_warning(get_formats())
  expect_warning(get_libraries())
})
# }
