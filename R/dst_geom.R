#' Geometric Distribution
#'
#' Makes a distribution belonging to the family of
#' geometric distributions.
#'
#' @param p probability of success in each trial. 0 < p <= 1
#'
#' @example dst_geom(0.4)
#'
#' @export
dst_geom <- function(p){
  if(p < 0 | p > 1){
    stop('p must be within 0 and 1')
  }
  dst_parametric("geom",
                 prob = p,
                 .variable = "discrete")
}
