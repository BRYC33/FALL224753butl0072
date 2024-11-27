library(testthat)

test_that("mu is correctly returned", {
  result <- mycurve(10, 5, 6)
  expect_equal(result$mu, 10)
})

test_that("sigma is correctly returned", {
  result <- mycurve(10, 5, 6)
  expect_equal(result$sigma, 5)
})
