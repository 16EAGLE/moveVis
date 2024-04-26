skip_on_cran()
context("suggest_formats")

# if("suggest_formats" %in% which_tests){
test_that("suggest_formats", {
  expect_is(suggest_formats(), "character")
  expect_gt(length(suggest_formats()), 0)
})
# }
