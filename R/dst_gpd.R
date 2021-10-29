#' Generalized Pareto Distribution
#'
#' Makes a distribution belonging to the family of
#' generalized Pareto distributions (GPD).
#' @param location,scale,shape Parameters of the GPD.
#' @return Object of class "dst" of a GPD.
#' @examples
#' dst_gpd(0, 1, 1)
#' @export
dst_gpd <- function(location, scale, shape) {
	if (scale <= 0) {
		stop("'scale' parameter must be positive.")
	}
  dst_parametric("gpd", location = location, scale = scale, shape = shape,
                 .variable = "continuous")
}
