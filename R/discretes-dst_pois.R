#' @export
#' @inheritParams next_discrete
next_discrete.pois <- function(distribution, from, n = 1L,
							   include_from = FALSE, ...) {
	next_discrete_natural(from, n = n, include_from = include_from)
}

#' @export
#' @inheritParams next_discrete
prev_discrete.pois <- function(distribution, from, n = 1L,
							   include_from = FALSE, ...) {
	prev_discrete_natural(from, n = n, include_from = include_from)
}
