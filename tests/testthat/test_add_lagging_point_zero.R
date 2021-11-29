test_that("add_lagging_point_zero() returns objects of the correct class.", {
  expect_type(add_lagging_point_zero(c(0.01, 0.002)), "double")
})

test_that("add_lagging_point_zero() returns numbers to the same precision.", {
  expect_true(all(nchar(add_lagging_point_zero(c(0.01, 0.002), 10)) == 12))
})

test_that("add_lagging_point_zero() returns the correct number of decimal places.", {
  ndp <- function(x) nchar(stringr::str_split_fixed(x, "[.]", 2)[, 2])
  expect_equal(ndp(add_lagging_point_zero(c(0.01, 0.002), 10)), c(10, 10))
})

