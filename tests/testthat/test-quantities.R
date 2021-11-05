test_that("quantities align with numeric computations.", {
  distributions <- list(
    dst_norm(0, 1),
    dst_norm(4, 2),
    dst_gpd(0, 1, 1 / 4 - 0.01),
    dst_gpd(0, 1, 1 / 3 - 0.01),
    dst_gpd(0, 1, 1 / 2 - 0.01),
    dst_gpd(0, 1, 1 - 0.01),
    dst_gpd(0, 1, 1 + 0.01),
    dst_gpd(0, 1, 0),
    dst_gpd(0, 1, -1),
    dst_exp(1),
    dst_exp(2)
    # dst_f(3, 1),
    # dst_f(4, 1),
    # dst_f(6, 1),
    # dst_f(8, 1),
    # dst_f(8.2, 1),
    # dst_t(1),
    # dst_t(2),
    # dst_t(3),
    # dst_t(4),
    # dst_t(4.2)
  )
  for (d in distributions) {
    ## Means
    if (!is.null(.quantities[[d$name]][["mean"]])) {
      mu1 <- mean(d)
      if (is.infinite(mu1)) {
        expect_error(mean.dst(d))
      } else {
        expect_equal(mu1, mean.dst(d))
      }
    }
    ## Medians
    if (!is.null(.quantities[[d$name]][["median"]])) {
      med1 <- median(d)
      expect_equal(med1, median.dst(d))
    }
    ## Variance
    if (!is.null(.quantities[[d$name]][["variance"]])) {
      var1 <- variance(d)
      ## Problematic for dst_gpd -- for some reason, variance.dst is looking
      ##  for a function pgpd (but the cdf is coded up in eval_cdf.gpd).
      expect_equal(var1, variance.dst(d))
    }
  }
})

