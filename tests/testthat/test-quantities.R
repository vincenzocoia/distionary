<<<<<<< Updated upstream
=======
#' Check hard-coded quantities
#'
#' Checks that the hard-coded quantities for the distributions in the
#' `.quantities` list match the default computations (for example,
#' integrating the quantile function to calculate the mean).
#'
#' @param distribution A distribution object to check.
#' @param name Name of the quantity to check, such as "mean". Character.
#' Should also be the name of a function.
#' @param tolerance The tolerance level in the `expect_equal()` call
#' when comparing the two quantities.
#' @return Invisible, if the name doesn't exist in the `.quantity` list
#' for that distribution, or the output of an `expect_` function from
#' the testthat package.
#' @note If a quantity is infinite, the default computation is expected
#' to be an error (such as through a divergent integral), and so
#' `expect_error()` is used in that situation.
check_quantity <- function(distribution, name, tolerance = 1e-3) {
  name_exists <- !is.null(.quantities[[distribution$name]][[name]])
  if (name_exists) {
    name.dst <- paste0(name, ".dst")
    quantity_from_list <- rlang::exec(name, distribution)
    if (is.infinite(quantity_from_list)) {
      expect_error(rlang::exec(name.dst, distribution))
    } else {
      quantity_default <- rlang::exec(name.dst, distribution, subdivision = 2000)
      expect_equal(quantity_from_list, quantity_default, tolerance = tolerance)
    }
  }
  invisible()
}

>>>>>>> Stashed changes
test_that("quantities align with numeric computations.", {
  distributions <- list(
    # dst_norm(0, 1),
    # dst_norm(4, 2),  # Passed
    # dst_pois(1),
    # dst_pois(3),
<<<<<<< Updated upstream
    # dst_pois(5)  # Passed
=======
    # dst_pois(5)
>>>>>>> Stashed changes
    # dst_unif(0, 1),
    # dst_unif(-1, 1),
    # dst_unif(-2, 0.8), # Passed
    # dst_beta(2,3),
    # dst_beta(0.5,0.9) # Passed
    # dst_binom(10, 0.6),
    # dst_binom(8, 0.05),
<<<<<<< Updated upstream
    # dst_binom(2, 0.89), # Passed
    # dst_nbinom(10, 0.5) # Passed
    # dst_nbinom(6, 0.05), #
=======
    # dst_binom(2, 0.89)
     dst_nbinom(6, 0.05) # Error: the integral is probably divergent
>>>>>>> Stashed changes
    # dst_nbinom(3, 0.87),
    # dst_nbinom(1.5, 0.5), # Passed
    # dst_geom(0.4),
    # dst_geom(1),
    # # dst_geom(0.05) # Passed
    # dst_exp(1),
    # dst_exp(2), # Passed
    dst_weibull(1, 1),
    dst_weibull(1.1, 2.1),
    dst_weibull(0.85, 3.55)




    # dst_gpd(0, 1, 1 / 4 - 0.01),
    # dst_gpd(0, 1, 1 / 3 - 0.01),
    # dst_gpd(0, 1, 1 / 2 - 0.01),
    # dst_gpd(0, 1, 1 - 0.01),
    # dst_gpd(0, 1, 1 + 0.01)
    # dst_gpd(0, 1, 0),
<<<<<<< Updated upstream
    # dst_gpd(0, 1, -1),

    # dst_f(3, 1)
=======
    # dst_gpd(0, 1, -1)
    # dst_exp(1),
    # dst_exp(2),
    # dst_geom(0.5),
    # dst_geom(0.2),
    # dst_geom(1)
    # dst_geom(0.05 #seems it will raise "Error: the integral is probably divergent" every time we used 0.05
    # dst_chisq(3) # remove hard-coded median for now
    # dst_chisq(5),
    # dst_chisq(1),
    # dst_cauchy(0, 1) # not fixed yet
    # dst_weibull(1, 1),
    # dst_weibull(2, 1)
    # dst_weibull(1.1, 2.1)
    #dst_weibull(0.85, 3.55)
    # dst_f(3, 1),
>>>>>>> Stashed changes
    # dst_f(4, 1),
    # dst_f(6, 1),
    # dst_f(8, 1),
    # dst_f(8.2, 1),
    #dst_t(1)
    # dst_t(2),
    # dst_t(3),
    # dst_t(4),
    # dst_t(4.2),
    # dest_lnorm

  )
  for (d in distributions) {
    ## Means
    if (!is.null(.quantities[[d$name]][["mean"]])) {
      mu1 <- mean(d)
      if (is.infinite(mu1)) {
        expect_error(mean.dst(d))
      } else {
        expect_equal(mu1, mean.dst(d), tolerance = 1e-03)
      }
    }
    ## Medians
    if (!is.null(.quantities[[d$name]][["median"]])) {
      med1 <- median(d)
      expect_equal(med1, median.dst(d), tolerance = 1e-03)
    }
    ## Variance
    if (!is.null(.quantities[[d$name]][["variance"]])) {
      var1 <- variance(d)
      ## Problematic for dst_gpd -- for some reason, variance.dst is looking
      ##  for a function pgpd (but the cdf is coded up in eval_cdf.gpd).
      expect_equal(var1, variance.dst(d), tolerance = 1e-03)
    }
  }
})

