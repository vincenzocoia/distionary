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
      quantity_default <- rlang::exec(name.dst, distribution)
      expect_equal(quantity_from_list, quantity_default, tolerance = tolerance)
    }
  }
  invisible()
}

test_that("quantities align with numeric computations.", {
  distributions <- list(
    #dst_norm(0, 1),
    #dst_norm(4, 2),  # Passed
    #dst_pois(1),
    #dst_pois(3),
    #dst_pois(5),  # Passed
    #dst_unif(0, 1),
    #dst_unif(-1, 1),
    #dst_unif(-2, 0.8), # Passed
    #dst_beta(2,3),
    #dst_beta(0.5,0.9), # Passed
    #dst_binom(10, 0.6),
    #dst_binom(8, 0.05),
    #dst_binom(2, 0.89), # Passed
    #dst_nbinom(10, 0.5), # Passed
    #dst_nbinom(6, 0.05), #
    #dst_nbinom(3, 0.87),
    #dst_nbinom(1.5, 0.5), # Passed
    #dst_geom(0.4),
    #dst_geom(1),
    #dst_geom(0.05), # Passed
    #dst_exp(1),
    #dst_exp(2), # Passed
    # dst_weibull(1, 1),
    # dst_weibull(1.1, 2.1),
    # dst_weibull(0.85, 3.55)




    #dst_gpd(0, 1, 1 / 4 - 0.01),
    #dst_gpd(0, 1, 1 / 3 - 0.01),
    #dst_gpd(0, 1, 1 / 2 - 0.01),
    #dst_gpd(0, 1, 1 - 0.01),
    #dst_gpd(0, 1, 1 + 0.01),
    #dst_gpd(0, 1, 0),
    #dst_gpd(0, 1, -1)

    dst_f(3, 6)
    #dst_f(4, 1),
    #dst_f(6, 1),
    #dst_f(8, 1),
    #dst_f(8.2, 1),
    #dst_t(1),
    #dst_t(2),
    #dst_t(3),
    #dst_t(4),
    #dst_t(4.2)
  )
  for (d in distributions) {
    print(d$name)
    check_quantity(d, "mean")
    check_quantity(d, "variance")
    check_quantity(d, "skewness")
    check_quantity(d, "kurtosis_exc")
    check_quantity(d, "median")
  }
})

