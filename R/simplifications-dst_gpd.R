#' @export
shift.gpd <- function(distribution, constant) {
	mutate_parameters(distribution, location = location + constant)
}

#' @export
multiply.gpd <- function(distribution, constant) {
	if (constant < 0) {
		flip(multiply(distribution, -constant))
	} else if (constant == 0) {
		dst_degenerate(0)
	} else {
		mutate_parameters(distribution,
						  location = location * constant,
						  scale = scale * constant)
	}
}
