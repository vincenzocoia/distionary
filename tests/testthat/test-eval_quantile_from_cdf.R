test_that("Quantile function works for several distributions", {
  p <- 1:99/100
  d <- list(
    dst_beta(3, 5),
    dst_binom(15, 0.2),
    dst_norm(0, 1),
    dst_norm(3.87e287, 1),
    dst_norm(3.87e287, 1e-200),
    dst_nbinom(10, 0.5),
    dst_exp(10),
    dst_gpd(-100, 342, 3)
  )
  for (d_ in d) {
    expect_equal(
      distionary:::eval_quantile.dst(d_, at = p),
      eval_quantile(d_, at = p),
      tolerance = 1e-6
    )
  }
})


# eval_quantile(d, at = NA)
# eval_quantile(d, at = NaN)
# eval_quantile(d, at = numeric(0L))
# eval_quantile(d, at = 0)
# eval_quantile(d, at = c(0, NA, NaN))
# eval_quantile(d, at = c(1, 0.4, NA, 0.2, NaN, 0))
# psteep <- function(x) pmax(pmin(1e10 * (x - 1e30), 1), 0)
# d_steep <- dst_parametric("steep", .variable = "continuous")
# expect_equal(psteep(eval_quantile(d, at = 1:10/10)), 1:10/10)
# pfixed <- function(x) as.numeric(x < -1e30)
# d_fixed <- dst_parametric("fixed", .variable = "continuous")
# expect_equal(eval_quantile(d, at = 1:10/10), -1e30)
# pflat <- function(x) {
#   res <- rep(0.5, length(x))
#   res[x < 0.5] <- pmax(x[x < 0.5], 0)
#   res[x > 100] <- pmin(x[x > 100] - 99.5, 1)
#   res
# }
# d_flat <- dst_parametric("flat", .variable = "continuous")
# expect_equal(eval_quantile(d_flat, at = 0.5), 0.5)
# expect_error(eval_quantile.dst(dst_norm(0, 1), at = 1))
#
#
