#' @export
eval_cdf.gev <- function(distribution, at) {
  r <- range(distribution)
  left_at <- at[at < r[1L]]
  right_at <- at[at > r[2L]]
  with(parameters(distribution), {
    t <- gev_t_function(at, location = location, scale = scale, shape = shape)
    res <- exp(-t)
    res[left_at] <- 0
    res[right_at] <- 1
    res
  })
}

#' @export
eval_quantile.gev <- function(distribution, at) {
  with(parameters(distribution), {
    invalid_at <- at < 0 | at > 1
    if (shape == 0) {
      res <- location - scale * log(-log(at))
    } else {
      res <- location + scale * ((-log(at))^(-shape) - 1) / shape
    }
    res[invalid_at] <- NaN
    res
  })
}

#' @export
eval_density.gev <- function(distribution, at, strict = TRUE) {
  r <- range(distribution)
  left_at <- at[at < r[1L]]
  right_at <- at[at > r[2L]]
  with(parameters(distribution), {
    t <- gev_t_function(at, location = location, scale = scale, shape = shape)
    res <- t^(shape + 1) / scale * exp(-t)
    res[left_at] <- 0
    res[right_at] <- 0
    res
  })
}

#' 't()' function for calculating GEV quantities
#'
#' @param x Argument of the function; vectorized.
#' @inheritParams dst_gev
#' @seealso See the Wikipedia entry for the GEV distribution,
#' https://en.wikipedia.org/wiki/Generalized_extreme_value_distribution
#' @note The shape parameter is not vectorized. This function is only
#' intended to be used when location, scale, and shape are scalars.
gev_t_function <- function(x, location, scale, shape) {
  z <- (x - location) / scale
  if (shape == 0) {
    exp(-z)
  } else {
    (1 + shape * z)^(-1 / shape)
  }
}
