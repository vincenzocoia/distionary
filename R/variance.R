#' @rdname moments
#' @export
variance <- function(distribution) {
  UseMethod("variance")
}

#' @export
variance.dst <- function(distribution, ...) {
  mu <- mean(distribution)
  sf <- representation_as_function(distribution, "survival")
  sf2 <- function(t) 1 + sf(mu + sqrt(t)) - sf(mu - sqrt(t))
  int <- stats::integrate(sf2, 0, Inf, ...)
  int$value
}
