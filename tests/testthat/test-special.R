# test_that("Normal Special Functions Work", {
#   transformed_norm <- dst_norm(0, 1) + 2
#   norm <- dst_norm(2, 1)
#   class(transformed_norm) <- class(transformed_norm)[-1]

#   # subscript out of bounds error
#   # expect_identical(mean(norm), mean(transformed_norm))
#   # expect_identical(median(norm), median(transformed_norm))

#   expect_identical(stdev(norm), stdev(transformed_norm))
#   expect_identical(
#     eval_cdf(norm, c(0, 2, 5)),
#     eval_cdf(transformed_norm, c(0, 2, 5))
#   )
#   expect_identical(
#     eval_density(norm, c(0, 2, 5), ),
#     eval_density(transformed_norm, c(0, 2, 5))
#   )

#   transformed_norm <- 2 + dst_norm(0, 1)
#   expect_identical(norm, transformed_norm)
#   expect_identical(mean(norm), mean(transformed_norm))
#   expect_identical(median(norm), median(transformed_norm))
#   expect_identical(stdev(norm), stdev(transformed_norm))
#   expect_identical(
#     eval_cdf(norm, c(0, 2, 5)),
#     eval_cdf(transformed_norm, c(0, 2, 5))
#   )
#   expect_identical(
#     eval_density(norm, c(0, 2, 5), ),
#     eval_density(transformed_norm, c(0, 2, 5))
#   )
# })


test_that("Degenerate Special functions work", {
  transformed_degen <- dst_degenerate(2) + 2
  degen <- dst_degenerate(4)
  class(degen) <- class(degen)[-1]

  expect_identical(mean(degen), mean(transformed_degen))
  expect_identical(median(degen), median(transformed_degen))
  expect_identical(stdev(degen), stdev(transformed_degen))
  # expect_identical(
  #   eval_cdf(degen, c(0, 2, 5)),
  #   eval_cdf(transformed_degen, c(0, 2, 5))
  # )
  expect_identical(
    eval_pmf(degen, c(0, 2, 5), strict = FALSE),
    eval_pmf(transformed_degen, c(0, 2, 5), strict = FALSE)
  )

  transformed_degen <- dst_degenerate(3) - 3
  degen <- dst_degenerate(0)
  class(degen) <- class(degen)[-1]
  expect_identical(mean(degen), mean(transformed_degen))
  expect_identical(median(degen), median(transformed_degen))
  expect_identical(stdev(degen), stdev(transformed_degen))
  expect_identical(
    eval_cdf(degen, c(0, 2, 5)),
    eval_cdf(transformed_degen, c(0, 2, 5))
  )
  expect_identical(
    eval_pmf(degen, c(0, 2, 5)),
    eval_pmf(transformed_degen, c(0, 2, 5))
  )

  transformed_degen <- 2 - dst_degenerate(0)
  degen <- dst_degenerate(-2)
  class(degen) <- class(degen)[-1]
  expect_identical(mean(degen), mean(transformed_degen))
  expect_identical(median(degen), median(transformed_degen))
  expect_identical(stdev(degen), stdev(transformed_degen))
  expect_identical(
    eval_cdf(degen, c(0, 2, 5)),
    eval_cdf(transformed_degen, c(0, 2, 5))
  )
  expect_identical(
    eval_pmf(degen, c(0, 2, 5)),
    eval_pmf(transformed_degen, c(0, 2, 5))
  )

  transformed_degen <- 2 * dst_degenerate(3)
  degen <- dst_degenerate(6)
  class(degen) <- class(degen)[-1]
  expect_identical(mean(degen), mean(transformed_degen))
  expect_identical(median(degen), median(transformed_degen))
  expect_identical(stdev(degen), stdev(transformed_degen))
  expect_identical(
    eval_cdf(degen, c(0, 2, 5)),
    eval_cdf(transformed_degen, c(0, 2, 5))
  )
  expect_identical(
    eval_pmf(degen, c(0, 2, 5)),
    eval_pmf(transformed_degen, c(0, 2, 5))
  )

  transformed_degen <- dst_degenerate(6) / 2
  degen <- dst_degenerate(3)
  class(degen) <- class(degen)[-1]
  expect_identical(mean(degen), mean(transformed_degen))
  expect_identical(median(degen), median(transformed_degen))
  expect_identical(stdev(degen), stdev(transformed_degen))
  expect_identical(
    eval_cdf(degen, c(0, 2, 5)),
    eval_cdf(transformed_degen, c(0, 2, 5))
  )
  expect_identical(
    eval_pmf(degen, c(0, 2, 5)),
    eval_pmf(transformed_degen, c(0, 2, 5))
  )

  transformed_degen <- 2 / dst_degenerate(3)
  degen <- dst_degenerate(1.5)
  class(degen) <- class(degen)[-1]
  expect_identical(mean(degen), mean(transformed_degen))
  expect_identical(median(degen), median(transformed_degen))
  expect_identical(stdev(degen), stdev(transformed_degen))
  expect_identical(
    eval_cdf(degen, c(0, 2, 5)),
    eval_cdf(transformed_degen, c(0, 2, 5))
  )
  expect_identical(
    eval_pmf(degen, c(0, 2, 5)),
    eval_pmf(transformed_degen, c(0, 2, 5))
  )
})

# test_that("Finite Special functions work", {
#   transformed_finite <- dst_finite(1:5, rep(0.2, 5)) + 5
#   fin <- dst_finite(6:10, rep(0.2, 5))
#   class(fin) <- class(fin)[-1]

# # FIXME: keep getting subscript out of bounds error
# expect_identical(mean(fin), mean(transformed_finite))
# expect_identical(median(degen), median(transformed_finite))
# expect_identical(stdev(fin), stdev(transformed_finite))
# FIXME: Couldn't find cdf
# expect_identical(
#   eval_cdf(fin, c(0, 2, 5)),
#   eval_cdf(transformed_finite, c(0, 2, 5))
# )
# FIXME: No applicable Method for 'eval_pmf' applied to an object of class "NULL"
# expect_identical(
#   eval_pmf(fin, c(0, 2, 5)),
#   eval_pmf(transformed_finite, c(0, 2, 5))
# )

transformed_finite <- dst_finite(1:5, rep(0.2, 5)) - 5
fin <- dst_finite(-4:0, rep(0.2, 5))
class(fin) <- class(fin)[-1]
# FIXME: Subscript out of bounds error
# expect_identical(mean(fin), mean(transformed_finite))
# expect_identical(median(fin), median(transformed_finite))
# expect_identical(stdev(fin), stdev(transformed_finite))
# FIXME: Eval_cdf dst is not made
# expect_identical(
#   eval_cdf(fin, c(0, 2, 5)),
#   eval_cdf(transformed_finite, c(0, 2, 5))
# )
# FIXME: No applicable Method for 'eval_pmf' applied to an object of class "NULL"
# expect_identical(
#   eval_pmf(fin, c(0, 2, 5)),
#   eval_pmf(transformed_finite, c(0, 2, 5))
# )

# transformed_finite <- 5 - dst_finite(0:40, rep(0.025, 40))
# fin <- dst_finite(-35:5, rep(0.025, 40))
# class(fin) <- class(fin)[-1]
# expect_identical(mean(fin), mean(transformed_finite))
# expect_identical(median(fin), median(transformed_finite))
# expect_identical(stdev(fin), stdev(transformed_finite))
# expect_identical(
#   eval_cdf(fin, c(0, 2, 5)),
#   eval_cdf(transformed_finite, c(0, 2, 5))
# )
# expect_identical(
#   eval_pmf(fin, c(0, 2, 5)),
#   eval_pmf(transformed_finite, c(0, 2, 5))
# )

# transformed_finite <- 2 * dst_finerate(3)
# fin <- dst_finerate(6)
# class(fin) <- class(fin)[-1]
# expect_identical(mean(fin), mean(transformed_finite))
# expect_identical(median(fin), median(transformed_finite))
# expect_identical(stdev(fin), stdev(transformed_finite))
# expect_identical(
#   eval_cdf(fin, c(0, 2, 5)),
#   eval_cdf(transformed_finite, c(0, 2, 5))
# )
# expect_identical(
#   eval_pmf(fin, c(0, 2, 5)),
#   eval_pmf(transformed_finite, c(0, 2, 5))
# )

# transformed_finite <- dst_finerate(6) / 2
# fin <- dst_finerate(3)
# class(fin) <- class(fin)[-1]
# expect_identical(mean(fin), mean(transformed_finite))
# expect_identical(median(fin), median(transformed_finite))
# expect_identical(stdev(fin), stdev(transformed_finite))
# expect_identical(
#   eval_cdf(fin, c(0, 2, 5)),
#   eval_cdf(transformed_finite, c(0, 2, 5))
# )
# expect_identical(
#   eval_pmf(fin, c(0, 2, 5)),
#   eval_pmf(transformed_finite, c(0, 2, 5))
# )

# transformed_finite <- 2 / dst_finerate(3)
# fin <- dst_finerate(1.5)
# class(fin) <- class(fin)[-1]
# expect_identical(mean(fin), mean(transformed_finite))
# expect_identical(median(fin), median(transformed_finite))
# expect_identical(stdev(fin), stdev(transformed_finite))
# expect_identical(
#   eval_cdf(fin, c(0, 2, 5)),
#   eval_cdf(transformed_finite, c(0, 2, 5))
# )
# expect_identical(
#   eval_pmf(fin, c(0, 2, 5)),
#   eval_pmf(transformed_finite, c(0, 2, 5))
# )
# })

test_that("Uniform Special functions work", {
  transformed_unif <- dst_unif(0, 5) + 2
  unif <- dst_unif(2, 7)
  class(unif) <- class(unif)[-1]

  # FIXME: Subscript out of bounds
  # expect_identical(mean(unif), mean(transformed_unif))
  # expect_identical(median(unif), median(transformed_unif))
  # expect_identical(stdev(unif), stdev(transformed_unif))
  # expect_identical(
  #   eval_cdf(unif, c(0, 2, 5)),
  #   eval_cdf(transformed_unif, c(0, 2, 5))
  # )
  # expect_identical(
  #   eval_pmf(unif, c(0, 2, 5)),
  #   eval_pmf(transformed_unif, c(0, 2, 5))
  # )

  transformed_unif <- dst_unif(0, 5) - 3
  unif <- dst_unif(-3, 2)
  class(unif) <- class(unif)[-1]
  # expect_identical(mean(unif), mean(transformed_unif))
  # expect_identical(median(unif), median(transformed_unif))
  # expect_identical(stdev(unif), stdev(transformed_unif))
  # expect_identical(
  #   eval_cdf(unif, c(0, 2, 5)),
  #   eval_cdf(transformed_unif, c(0, 2, 5))
  # )
  expect_identical(
    eval_density(unif, c(0, 2, 5), strict = FALSE),
    eval_density(transformed_unif, c(0, 2, 5), strict = FALSE)
  )

  # transformed_unif <- 2 - dst_unif(0, 5)
  # unif <- dst_unif(-3, 2)
  # class(unif) <- class(unif)[-1]
  # expect_identical(mean(unif), mean(transformed_unif))
  # expect_identical(median(unif), median(transformed_unif))
  # expect_identical(stdev(unif), stdev(transformed_unif))
  # expect_identical(
  #   eval_cdf(unif, c(0, 2, 5)),
  #   eval_cdf(transformed_unif, c(0, 2, 5))
  # )
  # expect_identical(
  #   eval_pmf(unif, c(0, 2, 5)),
  #   eval_pmf(transformed_unif, c(0, 2, 5))
  # )

  # transformed_unif <- 2 * dst_unif(0, 5)
  # unif <- dst_unif(0, 10)
  # class(unif) <- class(unif)[-1]
  # expect_identical(mean(unif), mean(transformed_unif))
  # expect_identical(median(unif), median(transformed_unif))
  # expect_identical(stdev(unif), stdev(transformed_unif))
  # expect_identical(
  #   eval_cdf(unif, c(0, 2, 5)),
  #   eval_cdf(transformed_unif, c(0, 2, 5))
  # )
  # expect_identical(
  #   eval_pmf(unif, c(0, 2, 5)),
  #   eval_pmf(transformed_unif, c(0, 2, 5))
  # )

  # transformed_unif <- dst_unif(0, 5) / 2
  # unif <- dst_unif(0, 5)
  # class(unif) <- class(unif)[-1]
  # expect_identical(mean(unif), mean(transformed_unif))
  # expect_identical(median(unif), median(transformed_unif))
  # expect_identical(stdev(unif), stdev(transformed_unif))
  # expect_identical(
  #   eval_cdf(unif, c(0, 2, 5)),
  #   eval_cdf(transformed_unif, c(0, 2, 5))
  # )
  # expect_identical(
  #   eval_pmf(unif, c(0, 2, 5)),
  #   eval_pmf(transformed_unif, c(0, 2, 5))
  # )

  # transformed_unif <- 2 / dst_unif(0, 5)
  # unif <- dst_unif(0, 2.5)
  # class(unif) <- class(unif)[-1]
  # expect_identical(mean(unif), mean(transformed_unif))
  # expect_identical(median(unif), median(transformed_unif))
  # expect_identical(stdev(unif), stdev(transformed_unif))
  # expect_identical(
  #   eval_cdf(unif, c(0, 2, 5)),
  #   eval_cdf(transformed_unif, c(0, 2, 5))
  # )
  # expect_identical(
  #   eval_pmf(unif, c(0, 2, 5)),
  #   eval_pmf(transformed_unif, c(0, 2, 5))
  # )
})
