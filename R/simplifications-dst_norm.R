#' @export
flip.norm <- function(distribution) {
	mutate_parameters(distribution, mean = -mean)
}

#' @export
shift.norm <- function(distribution, constant) {
	mutate_parameters(distribution, mean = mean + constant)
}

#' @export
multiply.norm <- function(distribution, constant) {
	if (constant == 0) return(dst_degenerate(0))
	mutate_parameters(distribution,
					  mean = mean * constant,
					  variance = variance * constant^2)
}
