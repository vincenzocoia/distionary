#' @export
flip.unif <- function(distribution) {
	mutate_parameters(distribution, min = -max, max = -min)
}

#' @export
shift.unif <- function(distribution, constant) {
	mutate_parameters(distribution, min = min + constant, max = max + constant)
}

#' @export
multiply.unif <- function(distribution, constant) {
	if (constant == 0) return(dst_degenerate(0))
	if (constant < 0) return(flip(multiply(distribution, -constant)))
	mutate_parameters(distribution, min = min * constant, max = max * constant)
}
