test_that("quantities align with numeric computations.", {
  distributions <- list(
    dst_norm(0, 1),
    dst_gpd(0, 1, 1 / 4 - 0.01),
    dst_gpd(0, 1, 1 / 3 - 0.01),
    dst_gpd(0, 1, 1 / 2 - 0.01),
    dst_gpd(0, 1, 1 - 0.01),
    dst_gpd(0, 1, 1 + 0.01),
    dst_gpd(0, 1, 0),
    dst_gpd(0, 1, -1)
  )
  for (d in distributions) {
    ## Means
    mu1 <- mean(d)
    if (is.infinite(mu1)) {
      expect_error(mean.dst(d))
    } else {
      expect_equal(mu1, mean.dst(d))
    }
    ## Medians
    med1 <- median(d)
    expect_equal(med1, median.dst(d))
  }
})

