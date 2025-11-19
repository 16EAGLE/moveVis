skip_on_cran()
context("align_move")

#if("align_move" %in% which_tests){
test_that("align_move (default)", {
  # correct calls
  x <- expect_is(align_move(m, verbose = F), "move2")
  expect_length(na.omit(unique(unlist(move2::mt_time_lags(x, units = "secs")))), 1)
  
  x <- expect_is(align_move(m, verbose = F, res = "max"), "move2")
  expect_length(na.omit(unique(unlist(move2::mt_time_lags(x, units = "secs")))), 1)
  
  x <- expect_is(align_move(m, res = "mean", verbose = F), "move2")
  expect_length(na.omit(unique(unlist(move2::mt_time_lags(x, units = "secs")))), 1)
  
  x <- expect_is(align_move(m, res = units::set_units(4, "min"), verbose = F), "move2")
  expect_length(na.omit(unique(unlist(move2::mt_time_lags(x, units = "secs")))), 1)
  
  x <- expect_is(align_move(
    m, res = units::set_units(4, "min"), start_end_time = round.POSIXt(range(mt_time(m)), units = "hours"),
    verbose = F
  ), "move2")
  expect_equal(nrow(x), 458)
  expect_length(na.omit(unique(unlist(move2::mt_time_lags(x, units = "secs")))), 1)
  
  # false calls
  expect_error(align_move(NA, verbose = F)) # wrong class
  expect_error(align_move(m, res = FALSE, verbose = F))
  expect_error(align_move(m, start_end_time = T, verbose = F))
  expect_error(expect_warning(align_move(m, res = 1, unit = "days", verbose = F)))
  
  # warnings
  expect_warning(align_move(m, digit = "max", verbose = F))
})
#}

test_that("Fill most proximate value when `fill_na_vals = TRUE`", {
  m <- move_data
  m[["x"]] <- sample(100, size = nrow(m), replace = TRUE)
  
  a <- align_move(m, res = units::set_units(2, "min"), verbose = FALSE)
  
  # Check each track separately, as timestamps should not be matched
  # for interpolation across tracks. Each interpolated value in `a` should
  # match the value in `m` where the min timestamp criterion is met
  for (track in unique(a$track)) {
    a1 <- move2::filter_track_data(a, .track_id = track)
    m1 <- move2::filter_track_data(m, .track_id = track)
    
    idx <- sapply(a1$timestamp, function(x) which.min(abs(x - m1$timestamp)))
    expect_equal(a1$x, m1$x[idx])
  }
})
