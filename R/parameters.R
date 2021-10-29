#' Parameters of a Distribution
#'
#' @param distribution Distribution.
#' @export
parameters <- function(distribution) UseMethod("parameters")

#' @export
parameters.dst <- function(distribution) {
  distribution[["parameters"]]
}
