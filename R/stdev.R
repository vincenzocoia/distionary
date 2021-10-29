#' @rdname moments
#' @export
stdev <- function(distribution) {
  UseMethod("stdev")
}


#' @rdname moments
#' @export
stdev.dst <- function(distribution) {
  ss <- variance(distribution)
  sqrt(ss)
}
