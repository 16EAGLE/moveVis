skip_on_cran()
context("get_frametimes")

test_that("get_frametimes", {
  frames <- frames_spatial(m.aligned, r_grad, r_times, r_type = "gradient", verbose = F)
  expect_is(get_frametimes(frames), "POSIXct")
  expect_equal(length(get_frametimes(frames)), length(unique(move::timestamps(m.aligned))))
  
  attr(frames[[100]], "time") <- NA
  expect_warning(get_frametimes(frames)) # missing some timestamps
  
  frames <- lapply(frames, function(x){
    attr(x, "time") <- NULL
    return(x)
  })
  expect_error(get_frametimes(frames)) # missing all timestamps
})
