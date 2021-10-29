y <- c(5, 2, 4, 2, NA, 2, -3, 8, 9)
w <- c(2, 2, 0, 2, 10, 1, NA, -1, 1)
dat <- data.frame(y = y, w = w)

edist <- dst_empirical(y, data = dat)
cdf <- get_cdf(edist)
qf <- get_quantile(edist)
sf <- get_survival(edist)
# plot(cdf)
# plot(qf)

set.seed(5)
edist2 <- dst_empirical(rnorm(10))

test_that("unweighted empirical distribution works", {
  expect_true(is_empirical(edist))
  expect_true(is_empirical(edist2))
  expect_true(is_distribution(edist))
  expect_true(is_distribution(edist2))
  expect_equal(plateaus(get_cdf(edist2)), 0:10 / 10)
  expect_identical(stats::knots(cdf), plateaus(qf))
  expect_equal(plateaus(cdf), 1 - plateaus(sf))
  expect_identical(cdf(qf(1 / 8)), 1 / 8)
  expect_identical(cdf(qf(1 / 8 + 0.001)), 4 / 8)
  expect_identical(cdf(qf(1 / 8 - 0.001)), 1 / 8)
  expect_identical(qf(4:5 / 8), c(2, 4))
  expect_identical(qf(cdf(2)), 2)
  expect_identical(qf(cdf(2 + 0.001)), 2)
  expect_identical(qf(cdf(2 - 0.001)), -3)
  expect_identical(qf(0:1), c(-3, 9))
  set.seed(1)
  s1 <- sample(sort(unique(y)), size = 100, replace = TRUE, prob = c(1, 3, 1, 1, 1, 1))
  set.seed(1)
  s2 <- realise(edist, 100)
  expect_identical(s1, s2)
  set.seed(1)
  at <- c(y, rnorm(10))
  expect_identical(eval_survival(edist, at), 1 - cdf(at))
})

# wdist <- dst_empirical(y, data = dat, weights = w) # Error
dat <- dat[-8, ]
wdist <- dst_empirical(y, data = dat, weights = w)
cdf <- get_cdf(wdist)
qf <- get_quantile(wdist)
sf <- get_survival(wdist)
# plot(cdf)
# plot(qf)

test_that("weighted step function works", {
  expect_identical(stats::knots(cdf), c(2, 5, 9))
  expect_identical(stats::knots(cdf), plateaus(qf))
  expect_identical(plateaus(cdf), 1 - plateaus(sf))
  expect_identical(cdf(c(2, 5, 9)), c(5, 7, 8) / 8)
})


test_that("step points are correct", {
  expect_identical(
    plateaus(get_cdf(wdist)),
    c(0, cumsum(discontinuities(wdist)[["size"]]))
  )
  expect_identical(
    stats::knots(get_cdf(wdist)),
    discontinuities(wdist)[["location"]]
  )
})
