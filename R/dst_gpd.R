#' Generalized Pareto Distribution
#'
#' Makes a distribution belonging to the family of
#' generalized Pareto distributions (GPD).
#' @param location,scale,shape Parameters of the GPD.
#' @return Object of class "dst" of a GPD.
#' @examples
#' d <- dst_gpd(0, 1, 1)
#' plot(d, "survival", to = 20)
#' @export
dst_gpd <- function(location, scale, shape) {
	if (scale == 0) {
		return(dst_degenerate(location))
	}
	if (scale < 0) {
		stop("'scale' parameter must be non-negative.")
	}
	dst_parametric("gpd", location = location, scale = scale, shape = shape,
				   .variable = "continuous")
}
