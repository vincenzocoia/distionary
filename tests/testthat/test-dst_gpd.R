test_that("GPD quantities work out, shape > 0", {
  loc <- 0
  scale <- 1
  shape <- 1
  .dst <- dst_gpd(loc = loc, scale = scale, shape = shape)
  med <- loc + scale * (2^shape - 1) / shape
  expect_equal(eval_cdf(.dst, c(loc - 1, loc, med)), c(0, 0, 0.5))
  expect_equal(eval_quantile(.dst, c(0, 0.5, 1)), c(loc, med, Inf))
  expect_equal(eval_density(.dst, c(loc - 1, Inf)), c(0, 0))
  expect_true(all(diff(eval_density(.dst, loc + 1:10)) < 0))
  pdf <- get_density(.dst)
  expect_equal(integrate(pdf, loc, Inf)$value, 1, tolerance = 0.0001)
  expect_true(all(realise(.dst, 10) >= loc))
})


test_that("GPD quantities work out, shape = 0", {
  loc <- 0
  scale <- 1
  shape <- 0
  .dst <- dst_gpd(loc = loc, scale = scale, shape = shape)
  med <- log(2)
  expect_equal(eval_cdf(.dst, c(loc - 1, loc, med, Inf)), c(0, 0, 0.5, 1))
  expect_equal(eval_quantile(.dst, c(0, 0.5, 1)), c(loc, med, Inf))
  expect_equal(eval_density(.dst, c(loc - 1, Inf)), c(0, 0))
  pdf <- get_density(.dst)
  expect_true(all(diff(pdf(loc + 1:10)) < 0))
  expect_equal(integrate(pdf, loc, Inf)$value, 1, tolerance = 0.0001)
  expect_true(all(realise(.dst, 10) >= loc))
})

test_that("GPD quantities work out, shape < 0", {
  loc <- 0
  scale <- 2
  shape <- -1
  rightend <- loc - scale / shape
  .dst <- dst_gpd(loc = loc, scale = scale, shape = shape)
  med <- loc + scale * (2^shape - 1) / shape
  expect_equal(
    eval_cdf(.dst, c(loc - 1, loc, med, rightend, rightend + 1, Inf)),
    c(0, 0, 0.5, 1, 1, 1)
  )
  expect_equal(
    eval_quantile(.dst, c(-1, 0, 0.5, 1, 2)),
    c(NaN, loc, med, rightend, NaN)
  )
  expect_equal(
    eval_density(.dst, c(loc - 1, rightend + 1, Inf)),
    c(0, 0, 0)
  )
  pdf <- get_density(.dst)
  expect_equal(integrate(pdf, loc, rightend)$value, 1, tolerance = 0.0001)
  r <- realise(.dst, 10)
  expect_true(all(r > loc & r < rightend))
})
