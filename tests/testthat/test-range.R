test_that("range for normal dst works", {
  inf <- c(-Inf, Inf)
  expect_equal(range(dst_norm(0, 1)), inf)
  expect_equal(range(dst_norm(10000, 454)), inf)
  expect_equal(range(dst_norm(-565, 1321321312321)), inf)
  expect_equal(range(dst_norm(-1, 0.5)), inf)
})


test_that("range for uniform dst works", {
  expect_equal(range(dst_unif(0, 1)), c(0, 1))
  expect_equal(range(dst_unif(-165465, 654654645)), c(-165465, 654654645))
  expect_equal(range(dst_unif(0, 0.5)), c(0, 0.5))
  expect_equal(range(dst_unif(-0.5, 1.46546)), c(-0.5, 1.46546))
})

test_that("range for gpd dst works", {
  expect_equal(range(dst_gpd(0, 1, 46)), c(0, Inf))
  expect_equal(range(dst_gpd(-0.2, 1, 1)), c(-0.2, Inf))
  expect_equal(range(dst_gpd(0.2, 1, 0.6)), c(0.2, Inf))
  expect_equal(range(dst_gpd(-2.26, 3, 0.26)), c(-2.26, Inf))
  expect_equal(range(dst_gpd(0, 1, -46)), c(0, 1 / 46))
  expect_equal(range(dst_gpd(-2, 45.21, -35.23)), c(-2, -0.71671871))
  expect_equal(range(dst_gpd(0.265, 8, -4.54)), c(0.265, 2.02711453744))
  expect_equal(
    range(dst_gpd(-4.56, 5.48, -654)),
    c(-4.56, (-4.56 - (5.48 / -654)))
  )
})


test_that("range for degenerate dst works", {
  expect_equal(range(dst_degenerate(0)), c(0, 0))
  expect_equal(range(dst_degenerate(-165465)), c(-165465, -165465))
  expect_equal(range(dst_degenerate(0.2)), c(0.2, 0.2))
  expect_equal(range(dst_degenerate(-2.26)), c(-2.26, -2.26))
})

test_that("range for finite dst works", {
  expect_equal(range(dst_finite(1:5, probs = rep(0.2, 5))), c(1, 5))
  expect_equal(
    range(dst_finite(c(-1, 2, 654, 66, 5),
      probs = c(0.1, 0.3, 0.5, 0.05, 0.05)
    )),
    c(-1, 654)
  )
  expect_equal(
    range(dst_finite(c(-1, -2, -5.565, -1.1, 0),
      probs = rep(0.2, 5)
    )),
    c(-5.565, 0)
  )
})

test_that("range for empirical dst works", {
  car <- data.frame(
    hp = c(
      110, 110, 93, 110, 175, 105, 245, 62, 95, 123, 123,
      180, 180, 180, 205, 215, 230, 66, 52, 65, 97, 150,
      150, 245, 175, 66, 91, 113, 264, 175, 335, 109
    )
  )
  cars <- data.frame(
    speed = c(
      4, 4, 7, 7, 8, 9, 10, 10, 10, 11, 11, 12, 12, 12,
      12, 13, 13, 13, 13, 14, 14, 14, 14, 15, 15, 15, 16,
      16, 17, 17, 17, 18, 18, 18, 18, 19, 19, 19, 20, 20,
      20, 20, 20, 22, 23, 24, 24, 24, 24, 25
    ), dist = c(
      2, 10, 4, 22, 16, 10, 18, 26, 34, 17, 28, 14, 20, 24,
      28, 26, 34, 34, 46, 26, 36, 60, 80, 20, 26, 54, 32,
      40, 32, 40, 50, 42, 56, 76, 84, 36, 46, 68, 32, 48,
      52, 56, 64, 66, 54, 70, 92, 93, 120, 85
    )
  )
  expect_equal(range(dst_empirical(hp, data = car)), c(52, 335))
  expect_equal(range(dst_empirical(speed, data = cars)), c(4, 25))
  expect_equal(range(dst_empirical(dist, data = cars)), c(2, 120))
})
