x <- c(-1, 4, 5, -1, -2, 7)
.dst <- dst_empirical(x)

test_that("mean and variance works with dst_empirical", {
  expect_equal(mean(x), mean(.dst))
  v <- mean(x^2) - mean(x)^2
  expect_equal(v, variance(.dst))
  expect_equal(sqrt(v), stdev(.dst))
})


x <- c(-1, 4, 5, NA, -2, 7)
.dst <- dst_empirical(x)

test_that("mean and variance works with dst_empirical -- with NA and no duplicates", {
  expect_identical(
    mean(x^2, na.rm = TRUE) - mean(x, na.rm = TRUE)^2,
    variance(.dst)
  )
})
