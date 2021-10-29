#' Range of Distribution
#'
#' Range returns a 2 index vector with the 0th index
#' containing the minimum value, and the 1st index containing the maximum value
#' for a given distribution.
#'
#' @param ... Single distribution to compute range from.
#' @param na.rm Not used; vestige of the `base::range()` S3 generic.
#' @details If there are no methods for the distribution's class,
#' the range is calculated
#' using `eval_quantile()` at 0 and at 1.
#' @examples
#' a <- dst_gpd(0, 1, 0.5)
#' b <- dst_unif(0, 1)
#' c <- dst_norm(3, 4)
#' range(a)
#' range(b)
#' range(c)
#' @rdname range
#' @export
range.dst <- function(..., na.rm = FALSE) {
  ellipsis <- rlang::list2(...)
  n <- length(ellipsis)
  if (n > 1) {
    stop("Can only find the range of one distribution; received ", n)
  }
  eval_quantile(ellipsis[[1L]], at = 0:1)
}
