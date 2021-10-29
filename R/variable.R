#' Random Variable of a Distribution
#'
#' Information about a random variable of a distribution.
#'
#' @param distribution Distribution.
#' @return As of now, the type of variable that it is -- one
#' of "continuous", "discrete", or "mixed".
#' @rdname variable
#' @export
variable <- function(distribution) {
  attributes(distribution)[["variable"]]
}
