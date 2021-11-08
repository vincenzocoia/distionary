#' Binomial Distribution
#'
#' Makes a distribution belonging to the family of
#' binomial distributions.
#'
#' @param size number of trials
#' @param p success probability for each trial
#'
#' @examples
#' dst_binom(10, 0.6)
#'
#' @export
dst_binom <- function(size, p){
  if(size < 0){
    stop("Size must be non-negative")
  }
  if (p < 0 | p > 1){
    stop('p must be within 0 and 1')
  }
  dst_parametric("binom",
                 size = size, prob = p,
                 .variable = "discrete")
}
