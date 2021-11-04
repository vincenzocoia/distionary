#' Hypergeometric Distribution
#'
#' Makes a distribution belonging to the family of
#' hypergeometric distributions.
#'
#' @param N the population size
#' @param K the number of success states in the population
#' @param n the number of draws
#' @param k the number of observed successes
#'
#'
#'
#' @example dst_hyper(1, 5, 10, 3)
#'
#' @export
dst_hyper <- function(k, K, N, n){
  if (N < 0){
    stop('N must be non-negative')
  }
  if (K < 0){
    stop('K must be non-negative')
  }
  if (n < 0){
    stop('n must be non-negative')
  }
  if (k < 0){
    stop('k must be non-negative')
  }
  dst_parametric('hyper',
                 x = k,
                 m = K,
                 n = N - K,
                 k = n,
                 .variable = "discrete")
}
