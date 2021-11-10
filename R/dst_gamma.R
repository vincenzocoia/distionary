#' Gamma Distribution
#'
#' Makes a distribution belonging to the family of
#' Gamma distributions.
#'
#' @param alpha shape > 0
#' @param beta rate > 0
#'
#' @examples
#' dst_gamma(2, 1)
#'
#' @export
dst_gamma <- function(alpha, beta){
  if (alpha <= 0) {
    stop("alpha must be positive.")
  }
  if (beta <= 0) {
    stop("beta must be positive.")
  }
  dst_parametric("gamma",
                 shape = alpha,
                 rate = beta,
                 .variable = "continuous")
}



