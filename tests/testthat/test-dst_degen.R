test_that("Distribution quantities are appropriate", {
  .dst <- dst_degenerate(0.5)
  expect_equal(eval_pmf(.dst, -10:10), rep(0, 21))
  expect_equal(eval_pmf(.dst, 0.5), 1)
  expect_equal(eval_cdf(.dst, -10:10), c(rep(0, 11), rep(1, 10)))
  expect_equal(eval_cdf(.dst, 0.5), 1)
  expect_equal(realise(.dst, 10), rep(0.5, 10))
  expect_equal(eval_quantile(.dst, 1:9 / 10), rep(0.5, 9))
})
