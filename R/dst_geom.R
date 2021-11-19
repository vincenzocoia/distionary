#' Geometric Distribution
#'
#' Makes a distribution belonging to the family of
#' geometric distributions.
#'
#' @param prob probability of success in each trial. 0 < p <= 1
#'
#' @examples
#' dst_geom(0.4)
#'
#' @export
dst_geom <- function(prob){
  if(prob <= 0 | prob > 1){
    stop('prob must be greater than 0 and less or equal to 1')
  }
  dst_parametric("geom",
                 prob = prob,
                 .variable = "discrete")
}
