#' Gamma Distribution
#'
#' Makes a distribution belonging to the family of
#' Gamma distributions.
#'
#' @param shape shape positive
#' @param rate rate positive
#'
#' @examples
#' dst_gamma(2, 1)
#'
#' @export
dst_gamma <- function(shape, rate){
  if (alpha <= 0) {
    stop("shape paramter must be positive.")
  }
  if (beta <= 0) {
    stop("rate parameter must be positive.")
  }
  dst_parametric("gamma",
                 shape = shape,
                 rate = rate,
                 .variable = "continuous")
}



