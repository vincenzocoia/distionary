test_that("quantities align with numeric computations.", {
  d <- dst_norm(0, 1)
  mu1 <- mean(d)
  class(d) <- tail(class(d), 1)
  mu2 <- mean(d)
  expect_equal(mu1, mu2)
})
