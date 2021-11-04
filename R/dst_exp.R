#' Exponential Distribution
#'
#' Makes a distribution belonging to the family of
#' exponential distributions.
#'
#' @param lambda rate
#'
#' @example dst(1)
#'
#' @export
dst_exp <- function(lambda){
  if(lambda <= 0){
    stop('lambda must be greater than 0')
  }
  dst_parametric("exp",
                 rate = lambda,
                 .variable = "continuous")
}



