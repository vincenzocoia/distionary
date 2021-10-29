#' @export
#' @inheritParams next_discrete
next_discrete.finite <- function(distribution, from, n = 1L,
								 include_from = FALSE, ...) {
	all_discretes <- distribution$probabilities$location
	next_discrete_finite(all_discretes,
						 from = from, n = n,
						 include_from = include_from
	)
}

#' @export
#' @inheritParams next_discrete
prev_discrete.finite <- function(distribution, from, n = 1L,
								 include_from = FALSE, ...) {
	all_discretes <- distribution$probabilities$location
	prev_discrete_finite(all_discretes,
						 from = from, n = n,
						 include_from = include_from
	)
}
