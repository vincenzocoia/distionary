#' Chi-squared Distribution
#'
#' Makes a distribution belonging to the family of
#' Chi-squared distributions.
#'
#' @param k degrees of freedom
#'
#' @examples
#' dst_chisq(3)
#'
#' @export
dst_chisq <- function(k){
  if (k < 0){
    stop('k must be non-negative')
  }
  dst_parametric("chisq",
                 df = k,
                 .variable = "continuous")
}
