set.seed(1)
x <- rnorm(10)
y <- rnorm(5)
p <- runif(5)

qf <- eqf(x)
pmf <- epmf(x)
cdf <- ecdf(x)

test_that("vectorization", {
  expect_identical(
    sapply(p, qf),
    qf(p)
  )
  expect_identical(
    sapply(y, pmf),
    pmf(y)
  )
})


test_that("eqf provides inverse of ecdf at original values", {
  expect_identical(x, qf(cdf(x)))
})

test_that("eqf = stats::quantile() with type = 1", {
  expect_equivalent(
    qf(0:10 / 10),
    stats::quantile(x, probs = 0:10 / 10, type = 1)
  )
  eps <- 0.000001
  expect_equivalent(
    qf(1:10 / 10 - eps),
    stats::quantile(x, probs = 1:10 / 10 - eps, type = 1)
  )
  expect_equivalent(
    qf(0:9 / 10 + eps),
    stats::quantile(x, probs = 0:9 / 10 + eps, type = 1)
  )
})

test_that("NAs are ignored", {
  qf_na <- eqf(c(x, NA))
  pmf_na <- epmf(c(x, NA))
  expect_identical(qf(p), qf_na(p))
  expect_identical(pmf(y), pmf_na(y))
})


test_that("eqf boundaries work as expected", {
  expect_identical(1, cdf(qf(1)))
  expect_identical(range(x), qf(0:1))
})

test_that("epmf provides proportions", {
  expect_identical(pmf(y), rep(0, length(y)))
  expect_identical(pmf(x), rep(1 / length(x), length(x)))
  z <- c(0, 1, 1, 1)
  pmf2 <- epmf(z)
  expect_identical(pmf2(0:1), c(0.25, 0.75))
})
