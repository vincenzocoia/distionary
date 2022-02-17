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
#' @param verbose Logical; print the distribution being tested to screen?
#' @return Invisible, if the name doesn't exist in the `.quantity` list
#' for that distribution, or the output of an `expect_` function from
#' the testthat package.
#' @note If a quantity is infinite, the default computation is expected
#' to be an error (such as through a divergent integral), and so
#' `expect_error()` is used in that situation.
check_quantity <- function(distribution, name, tolerance = 1e-3,
                           verbose = FALSE) {
  if (verbose) {
    cat("Testing function: ", name, "\n")
    cat("Distribution: ", distribution$name, "\n")
    cat("Parameters: ", paste(parameters(distribution), ","), "\n\n")
  }
  name_exists <- !is.null(.quantities[[distribution$name]][[name]])
  if (name_exists) {
    name.dst <- paste0(name, ".dst")
    quantity_from_list <- rlang::exec(name, distribution)
    if (is.infinite(quantity_from_list) || is.na(quantity_from_list)) {
      expect_error(rlang::exec(name.dst, distribution))
    } else {
      quantity_default <- rlang::exec(name.dst, distribution,
                                      subdivisions = 2000)
      expect_equal(quantity_from_list, quantity_default, tolerance = tolerance)
    }
  }
  invisible()
}

test_that("quantities align with numeric computations.", {
  distributions <- list(
    # dst_norm(0, 1),               ##***##
    # dst_norm(4, 2)                ##***##
    # dst_pois(1),                  ##***##
    # dst_pois(3),                  ##***##
    # dst_pois(5)                   ##***##
    # dst_unif(0, 1),               ##***##
    # dst_unif(-1, 1),              ##***##
    # dst_unif(-2, 0.8)             ##***##
    # dst_beta(2,3),                ##***##
    # dst_beta(0.5,0.9)             ##***##
    # dst_binom(10, 0.6),           ##***##
    # dst_binom(8, 0.05),           ##***##
    # dst_binom(2, 0.89)            ##***##
    # dst_nbinom(6, 0.05)           # TEST ONLY skewness: 1 vs NaN; kurtosis_exc: the integral is probably divergent
    # dst_nbinom(3, 0.87)           # TEST ONLY kurtosis_exc: the integral is probably divergent
    # dst_nbinom(1.5, 0.5)          ##***##
    # dst_gpd(0, 1, 1 / 4 - 0.01),  ##***##
    # dst_gpd(0, 1, 1 / 3 - 0.01)   ##***##
    # dst_gpd(0, 1, 1 / 2 - 0.01)   # TEST ONLY skewness: Integral did not converge (Skewness exists when param3 < 1/3)
    # dst_gpd(0, 1, 1 - 0.01)       # TEST ONLY skewness: Integral did not converge (Skewness exists when param3 < 1/3)
    # dst_gpd(0, 1, 1 + 0.01)       # TEST ONLY mean/skewness: Integral did not converge.(Mean exists when param3 < 1, Skewness exists when param3 < 1/3)
    # dst_gpd(0, 1, 0)              ##***##
    # dst_gpd(0, 1, -1)             ##***##
    # dst_exp(1)                    ##***##
    # dst_exp(2)                    ##***##
    # dst_geom(0.5)                 ##***##
    # dst_geom(0.2)                 ##***##
    # dst_geom(1)                   # TEST ONLY skewness/kurtosis_exc: need fixed.(Skewness and Kurtosis do not exist when p = 1)
    # dst_geom(0.05)                # TEST ONLY kurtosis_exc: "Error: the integral is probably divergent" every time we used 0.05
    # dst_chisq(3),                 ##***## remove hard-coded median for now
    # dst_chisq(5),                 ##***##
    # dst_chisq(1)                  ##***##
    # dst_cauchy(0, 1)              # TEST ONLY mean: need fixed. skewness: Integral did not converge. (skewness is undefined for Cauchy)
    # dst_weibull(1, 1)             ##***##
    # dst_weibull(2, 2)             # kurtosis_exc: -8 vs 0
    # dst_weibull(1.1, 2.1)         # kurtosis_exc: -9 vs 4
    # dst_weibull(0.85, 3.55)       # kurtosis_exc: -5 vs 10
    # dst_f(3, 1)                   # mean/skewness: Integral did not converge. median: skip
    # dst_f(4, 1)                   # mean/skewness: Integral did not converge. median: skip
    # dst_f(6, 1),                  # mean/skewness: Integral did not converge. median: skip
    # dst_f(8, 1),                  # mean/skewness: Integral did not converge. median: skip
    # dst_f(8.2, 1)                 # mean/skewness: Integral did not converge. median: skip
    # dst_t(1)                      # mean: need fixed. skewness: Integral did not converge.
    # dst_t(2),                     # mean: need fixed. skewness: Integral did not converge.
    # dst_t(3),                     # mean: need fixed. skewness: Integral did not converge.
    # dst_t(4),                     # mean: need fixed. skewness: Integral did not converge.
    # dst_t(4.2),                   # mean: need fixed. skewness: Integral did not converge.
  )
  for (d in distributions) {
    print(d$name)
    check_quantity(d, "mean")
    check_quantity(d, "variance")
    check_quantity(d, "skewness")
    check_quantity(d, "kurtosis_exc", verbose = TRUE)
    check_quantity(d, "median")
  }
})

