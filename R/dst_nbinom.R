#' Negative binomial Distribution
#'
#' Makes a distribution belonging to the family of
#' negative binomial distributions.
#'
#'@param prob probabilities
#'@param size target for number of successful trials; non-negative
#'
#'@examples
#' dst_nbinom(0.5, 10)
#'
#'
#'@export
dst_nbinom <- function(size, prob){
  if(size < 0){
    stop('size must be positive')
  }
  if(prob < 0 | prob > 1){
    stop('prob must be within 0 and 1')
  }
  dst_parametric("nbinom",
                 size = size,
                 prob = prob,
                 .variable = "discrete")
}
