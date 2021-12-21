test_that("Environment specification works.", {
  d <- dst_parametric("norm", mean = 0, sd = 1, .variable = "continuous")
  expect_false(identical(d$env, ".GlobalEnv"))
  expect_false(identical(d$env, .GlobalEnv))
  expect_equal(eval_cdf(d, at = -3:3), stats::pnorm(-3:3))
  d <- dst_parametric("norm", mean = 0, sd = 1, .variable = "continuous",
                      .env = .GlobalEnv)
  expect_equal(d$env, ".GlobalEnv")
  expect_equal(eval_cdf(d, at = -3:3), stats::pnorm(-3:3))
  d <- dst_parametric("norm", mean = 0, sd = 1, .variable = "continuous",
                      .env = ".GlobalEnv")
  expect_equal(d$env, ".GlobalEnv")
  expect_equal(eval_cdf(d, at = -3:3), stats::pnorm(-3:3))
  d <- dst_parametric("norm", mean = 0, sd = 1, .variable = "continuous",
                      .env = "package:stats")
  expect_equal(d$env, "package:stats")
  expect_equal(eval_cdf(d, at = -3:3), stats::pnorm(-3:3))
  d <- dst_parametric("norm", mean = 0, sd = 1, .variable = "continuous",
                      .env = "package:base")
  expect_equal(d$env, "package:base")
  expect_error(eval_cdf(d, at = -3:3))
})
