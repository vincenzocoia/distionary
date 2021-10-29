#' @export
mean.parametric <- function(x, ...) {
  ellipsis::check_dots_empty()
	quantity_parametric(x, "mean")
}

#' @export
variance.parametric <- function(distribution) {
	quantity_parametric(distribution, "variance")
}

#' @export
skewness.parametric <- function(distribution) {
	quantity_parametric(distribution, "skewness")
}

#' @export
kurtosis_exc.parametric <- function(distribution) {
	quantity_parametric(distribution, "kurtosis_exc")
}

#' @export
evi.parametric <- function(distribution) {
	quantity_parametric(distribution, "evi")
}

#' @export
range.parametric <- function(..., na.rm = FALSE) {
  distribution <- rlang::list2(...)[[1L]]
	quantity_parametric(distribution, "range")
}

#' @export
median.parametric <- function(x, ...) {
	quantity_parametric(distribution, "median")
}

quantity_parametric <- function(distribution, quantity) {
	d_name <- distribution$name
	q_expr <- .quantities[[d_name]][[quantity]]
	if (!is.null(q_expr)) {
		rlang::eval_tidy(q_expr, data = parameters(distribution))
	} else {
		rlang::exec("NextMethod", .env = rlang::caller_env(n = 1))
	}
}
