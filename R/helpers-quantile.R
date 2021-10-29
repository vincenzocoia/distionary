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
eval_quantile_from_cdf <- function(cdf, discon, at, tol, maxiter) {
  n_breaks <- nrow(discon)
  breaks <- discon[["location"]]
  cdf_high <- cdf(breaks)
  cdf_low <- cdf_high - discon[["size"]]
  n_x <- length(at)
  if (identical(n_x, 0L)) {
    return(numeric(0L))
  }
  ox <- order(at)
  at <- at[ox]
  dup_x <- duplicated(at)
  res <- at
  x_lte_0 <- at <= 0
  x_gt_1 <- at > 1
  x_lte_1 <- at <= 1
  res[x_lte_0] <- -Inf
  res[x_gt_1] <- Inf
  i <- sum(x_lte_0, na.rm = TRUE)
  n_x <- sum(x_lte_1, na.rm = TRUE)
  break_id <- 0L
  while (i < n_x) {
    remaining_xs <- at[seq(i + 1, n_x)]
    next_x <- remaining_xs[1L]
    higher_breaks <- next_x <= cdf_high
    break_id <- which(higher_breaks)[1L]
    above_all_breaks <- identical(sum(higher_breaks), 0L)
    this_break <- breaks[break_id]
    this_cdf_high <- cdf_high[break_id]
    this_cdf_low <- cdf_low[break_id]
    if (above_all_breaks) {
      if (identical(n_breaks, 0L)) {
        low <- get_lower(cdf, level = at[1L])
      } else {
        low <- breaks[n_breaks]
      }
      high <- get_higher(cdf, level = at[n_x])
      this_break <- Inf
      this_cdf_high <- 1
      this_cdf_low <- 1
    } else if (identical(break_id, 1L)) {
      low <- get_lower(cdf, level = at[1L])
      high <- this_break
    } else {
      low <- breaks[break_id - 1L]
      high <- this_break
    }
    n_x_in_batch <- sum(remaining_xs <= this_cdf_high)
    n_x_below_discont <- sum(remaining_xs <= this_cdf_low)
    n_x_in_discont <- n_x_in_batch - n_x_below_discont
    x_ids_in_discont <- i + n_x_below_discont + seq_len(n_x_in_discont)
    res[x_ids_in_discont] <- this_break
    for (x_id in i + seq_len(n_x_below_discont)) {
      if (dup_x[x_id]) {
        res[x_id] <- res[x_id - 1L]
      } else {
        answer <- left_inverse(cdf,
          at = at[x_id],
          low = low, high = high,
          tol = tol, maxiter = maxiter
        )

        low <- answer
        res[x_id] <- answer
      }
    }
    i <- i + n_x_in_batch
  }
  unordered_res <- res
  unordered_res[ox] <- res
  unordered_res
}

#' Algorithm to Compute Left Inverse
#'
#' Calculates the smallest value for which a function
#' \code{f} evaluates to be greater than or equal to
#' \code{at} -- that is, the left inverse of \code{f}
#' at \code{at}. Intended for internal use only.
#' @param f A function to compute the left inverse of.
#' @param at Value for which to calculate the left inverse.
#' Not vectorized.
#' @param low,high Single numeric values forming a range
#' within which to search for the solution.
#' @param tol Tolerance for the solution. The actual
#' solution will be within plus or minus half this value.
#' @param maxiter Number of iterations to attempt before
#' quitting, if the tolerance has not been reached. by then.
#' @details This algorithm works by progressively
#' cutting the specified range in half, so that the width
#' of the range after k iterations is 1/2^k times the
#' original width.
#' @export
left_inverse <- function(f, at, low, high, tol, maxiter) {
  stopifnot(low < high)
  if (is.na(at)) {
    return(at)
  }
  w <- high - low
  i <- 0L
  while (w > tol && i <= maxiter) {
    i <- i + 1L
    mid <- (high + low) / 2
    val <- f(mid)
    if (val >= at) {
      high <- mid
    } else {
      low <- mid
    }
  }
  if (i == maxiter && w > tol) {
    warning(
      "Maximum number of iterations reached before",
      " tolerance was achieved."
    )
  }
  mid
}

#' Find an Encapsulating Range of Quantiles
#'
#' Given a quantile level, these functions
#' will find a quantile that evaluates to
#' be higher/lower than the provided quantile level.
#' Intended for internal use only.
#' @param cdf Function representing the cdf
#' @param level Quantile level defining the
#' @rdname practical_limits
#' @export
get_higher <- function(cdf, level) {
  # discon <- discontinuities(object)
  # if (!identical(nrow(discon), 0L)) {
  # 	r <- range(discon[["location"]])
  # 	low <- r[1] - 0.0001
  # 	high <- r[2] + 0.0001
  # } else {
  # 	rf <- get_randfn(object)
  # 	if (!is.null(rf)) {
  # 		x1 <- rf(1)
  # 		x2 <- rf(1)
  # 		while(x1 == x2) {
  # 			x2 <- rf(1)
  # 		}
  # 		low <- min(x1, x2)
  # 		high <- max(x1, x2)
  # 	}
  # }
  warning("This function doesn't work properly yet!")
  # if (cdf(low) >= x) {
  # 	stop("cdf at low value must evaluate to <p.")
  # }
  # if (cdf(high) < x) {
  # 	stop("cdf at high value must evaluate to >=p.")
  # }
  1000
}

#' @rdname practical_limits
#' @export
get_lower <- function(cdf, level) {
  warning("This function doesn't work properly yet!")
  -1000
}
