#' Extreme Value Index
#'
#' @param distribution Distribution to obtain EVI from.
#'
#' @return A single numeric.
#' @examples
#' evi(dst_gpd(0, 1, 2))
#' evi(dst_gpd(0, 1, -1))
#' evi(dst_unif(0, 10))
#' evi(dst_norm(5, 5))
#' evi(dst_empirical(1:10))
#' @export
evi <- function(distribution) {
  UseMethod("evi")
}

#' @export
evi.dst <- function(distribution) {
	if (variable(distribution) == "discrete") {
		return(NaN)
	}
	warning("Cannot find EVI for this distribution; returning NA.")
	NA
}
