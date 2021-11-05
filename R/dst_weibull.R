#' Weibull Distribution
#'
#' Makes a distribution belonging to the family of
#' Weibull distributions.
#'
#' @param lambda > 0 scale
#' @param gamma > 0 shape
#'
#' @example dst_weibull(1, 1)
#'
#' @export
dst_weibull <- function(lambda, gamma){
  if(lambda <= 0){
    stop('lambda must be positive')
  }
  if(gamma <= 0){
    stop('gamma must be positive')
  }
  dst_parametric("weibull",
                 shape = gamma,
                 scale = lambda,
                 .variable = "continuous")
}
