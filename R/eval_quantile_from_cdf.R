#' Evaluate Quantiles using a CDF
#'
#' Intended for internal use only.
#'
#' @param cdf Function representing the cdf.
#' @param discon A data frame of discontinuities, as in the
#' output of \code{discontinuities()}.
#' @param at A vector of values for which to evaluate the
#' quantile function.
#' @param tol,maxiter Tolerance and maximum number of iterations
#' @export
eval_quantile_from_cdf <- function(distribution, at, tol, maxiter) {
  n <- length(at)
  if (n == 0) return(numeric(0L))
  ord <- order(at)
  at <- at[ord]
  x <- at
  i_zero <- which(at == 0)
  i_na <- which(is.na(at))
  i_positive <- setdiff(seq_len(n), c(i_zero, i_na))
  x[i_zero] <- NA
  r <- encapsulate_p(distribution, at[i_positive])
  low <- rep(r[1L], n)
  high <- r[2L]
  for (i in i_positive) {
    p <- at[i]
    if (isTRUE(p == at[i - 1L])) {
      x[i] <- low[i + 1L] <- x[i - 1L]
    } else {
      x[i] <- low[i + 1L] <- left_inverse(
        distribution, p = p, low = low[i], high = high, tol = tol,
        maxiter = maxiter
      )
    }
  }
  x[ord] <- x
  x
}

#' Find a range of possible outcomes
#'
#' Find a range of possible outcomes where the cdf evaluates to values
#' that encapsulate the range of probabilities contained in the vector `p`.
#'
#' @param distribution Distribution.
#' @param p Vector of values between 0 and 1 (inclusive). `NA` values are not
#' anticipated; nor are values not between 0 and 1.
encapsulate_p <- function(distribution, p) {
  if (length(p) == 0) return(c(NA, NA))
  p_min <- min(p)
  p_max <- max(p)
  left <- -1
  right <- 1
  cdf_p <- eval_cdf(distribution, at = p)
  cdf_left <- eval_cdf(distribution, at = left)
  cdf_right <- eval_cdf(distribution, at = right)
  while (cdf_left >= p_min) {
    left <- 2 * left
    cdf_left <- eval_cdf(distribution, at = left)
  }
  while (cdf_right < p_max) {
    right <- 2 * right
    cdf_right <- eval_cdf(distribution, at = right)
  }
  c(left, right)
}


#' Algorithm to Compute Left Inverse
#'
#' Calculates the smallest value for which a function
#' \code{f} evaluates to be greater than or equal to
#' \code{y} -- that is, the left inverse of \code{f}
#' at \code{y}. Intended for internal use only.
#' @param distribution A distribution with a specified cdf.
#' @param p Single value for which to calculate the left inverse.
#' @param low,high Single numeric values forming a range
#' within which to search for the solution.
#' @param tol Maximum tolerance for the solution.
#' @param maxiter Number of iterations to attempt before
#' quitting, if the tolerance has not been reached. by then.
#' @details This algorithm works by progressively
#' cutting the specified range in half, so that the width
#' of the range after k iterations is 1/2^k times the
#' original width.
#' @export
left_inverse <- function(distribution, p, low, high, tol, maxiter) {
  stopifnot(low <= high)
  if (is.na(p)) return(p)
  max_tol <- tol
  w <- Inf
  i <- 0L
  while (w > tol && i <= maxiter) {
    i <- i + 1L
    mid <- (high + low) / 2
    cdf_mid <- eval_cdf(distribution, at = mid)
    if (p <= cdf_mid) {
      high <- mid
      discrete <- prev_discrete(
        distribution, from = high, n = 1L, include_from = TRUE
      )
    } else {
      low <- mid
      discrete <- next_discrete(
        distribution, from = low, n = 1L, include_from = TRUE
      )
    }
    low_high <- narrow_by_discretes(
      distribution, p = p, low = low, high = high, discrete = discrete
    )
    low <- low_high[1L]
    high <- low_high[2L]
    if (low == high) return(low)
    w <- high - low
    cdf_ends <- eval_cdf(distribution, at = c(low, high))
    slope <- diff(cdf_ends) / w
    tol <- max_tol / slope
  }
  if (i == maxiter && w > tol) {
    warning(
      "Maximum number of iterations reached before ",
      "tolerance was achieved."
    )
  }
  mid
}

#' Narrow a range of possible values by a discrete
narrow_by_discretes <- function(distribution, p, low, high, discrete) {
  if (is.na(discrete) || !length(discrete) ||
      discrete < low || discrete > high) {
    return(c(low, high))
  }
  f_upper <- prob_left(distribution, of = discrete, inclusive = TRUE)
  f_lower <- prob_left(distribution, of = discrete, inclusive = FALSE)
  if (f_lower <= p && p <= f_upper) {
    low <- high <- discrete
  } else if (p < f_lower) {
    high <- discrete
  } else {
    low <- discrete
  }
  c(low, high)
}

