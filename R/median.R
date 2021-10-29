#' @importFrom stats median
#'
#' @param x Distribution to calculate median from.
#' @rdname moments
#' @export
median.dst <- function(x, ...) {
  eval_quantile(x, at = 0.5)
}
