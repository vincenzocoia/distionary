#' Generalized Extreme Values Distribution
#'
#' Makes a distribution belonging to the family of
#' gev distributions.
#'
#' @param location
#' @param scale
#' @param shape
#'
#' @example
#' dst_gev(0, 1, 1)
#'
#' @export
dst_gev <- function(location, scale, shape){
  if (scale <= 0){
    stop("scale' parameter must be positive")
  }
  dst_parametric("gev",
                 location = location, scale = scale, shape = shape,
                 .variable = "continuous")
}
