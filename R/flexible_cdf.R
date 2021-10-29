#' Find the probability left or right of a number
#'
#' Almost a direct application of cdf and survival function,
#' but allows for the cdf to be calculated using `<` and the survival
#' function to be calculated using `>=`.
#'
#' @param distribution Distribution to find probabilities of.
#' @param of Find the probability to the left or right *of* this number.
#' Could be a vector.
#' @param inclusive Should `of` be included in the probability calculation?
#' Logical.
#' @rdname flexible_cdf
prob_left <- function(distribution, of, inclusive) {
	p_left <- eval_cdf(distribution, at = of)
	if (!inclusive) {
		p_break <- eval_pmf(distribution, at = of, strict = FALSE)
		p_left <- p_left - p_break
	}
	p_left
}

#' @inheritParams prob_left
prob_right <- function(distribution, of, inclusive) {
	p_right <- eval_survival(distribution, at = of)
	if (inclusive) {
		p_break <- eval_pmf(distribution, at = of, strict = FALSE)
		p_right <- p_right + p_break
	}
	p_right
}
