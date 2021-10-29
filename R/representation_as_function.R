#' Get a distribution's representation as a function
#'
#' @param distribution Distribution to extract a representation from.
#' @param representation Character, such as `"cdf"`. In general, a suffix
#' to an `eval_` function.
#' @return A function with one argument (`at`) that's fed into the
#' corresponding `eval_` function.
representation_as_function <- function(distribution, representation) {
	eval_f_name <- paste0("eval_", representation)
	# eval_f <- get(eval_f_name)
	# function(at) eval_f(distribution, at = at)
	function(at) rlang::exec(eval_f_name, distribution, at = at)
}
