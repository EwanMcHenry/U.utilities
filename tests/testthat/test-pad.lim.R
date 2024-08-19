library(testthat)

test_that("pad.lim adds padding correctly", {
  expect_equal(pad.lim(c(1, 2)), c(0.95, 2.05))
  expect_equal(pad.lim(c(5, 10), map.pad = 0.1), c(4.5, 10.5))
  expect_equal(pad.lim(c(-5, 0), map.pad = 0.2), c(-6, 1))
})
