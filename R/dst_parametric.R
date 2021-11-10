#' Convert p/d/q/r Representations to a Distribution
#'
#' When a distribution has `p`, `d`, `q`, `r` functions available (such as
#' `pnorm()`, `dnorm()`, etc.), `dst_parametric()` creates a distribution
#' that draws on these four functions.
#'
#' @param .name Name of the distribution. Specifically, a string with
#' the suffix of the `p`, `d`, `q`, `r` functions (such as `"norm"`).
#' @param ... Name-value pairs of parameters. Names must be found in the
#' parameter names of the `p`, `d`, `q`, `r` functions.
#' @param .variable Type of random variable represented by the distribution.
#' Warning: defaults to "unknown", where density, pmf, and hazard cannot be
#' evaluated.
#' @details The `r` function is not needed, but is used if available.
#' @examples
#' d <- dst_parametric("norm", mean = 0, sd = 1, .variable = "continuous")
#' eval_density(d, at = -3:3)
#' eval_pmf(d, at = -3:3, strict = FALSE)
#' eval_hazard(d, at = -3:3)
#' @export
dst_parametric <- function(
	.name, ..., .variable = c("unknown", "continuous", "discrete", "mixed")) {
  if(!(.name %in% list('t', 'f', 'exp', 'gpd', 'gev', 'unif',
                         'pois', 'norm', 'geom', 'lnorm', 'hper',
                         'gamma', 'chisq', 'nbinom', 'cauchy', 'weibull'))){
    stop('This distribution is not available')
  }
	v <- match.arg(.variable)
	dots <- rlang::enquos(...)
	param_names <- names(dots)
	vctrs::vec_as_names(param_names, repair = "check_unique")
	params <- lapply(dots, rlang::eval_tidy)
	names(params) <- names(dots)
	res <- list(name = .name,
				parameters = params)
	new_distribution(res, variable = v, class = c(.name, "parametric"))
}

#' @export
eval_cdf.parametric <- function(distribution, at) {
	function_name <- paste0("p", distribution$name)
	rlang::exec(function_name, at, !!!distribution$parameters)
}

#' @export
eval_survival.parametric <- function(distribution, at) {
	function_name <- paste0("p", distribution$name)
	if ("lower.tail" %in% names(formals(function_name))) {
		rlang::exec(function_name, at, !!!distribution$parameters, lower.tail = FALSE)
	} else {
		1 - rlang::exec(function_name, at, !!!distribution$parameters)
	}
}

#' @export
eval_density.parametric <- function(distribution, at, strict = TRUE) {
	v <- variable(distribution)
	if (v == "continuous") {
		function_name <- paste0("d", distribution$name)
		return(rlang::exec(function_name, at, !!!distribution$parameters))
	}
	if (strict) {
		stop("Distribution is of variable type '", v, "'; density only ",
			 "exists for continuous variables. Perhaps you'd like to ",
			 "evaluate outside of strict mode?")
	}
	if (v == "discrete") {
		return(rep(0, length(at)))
	}
	stop("Evaluating non-strict density for a parametric distribution is ",
		 "not yet available. Check that your distribution's variable type ",
		 "is specified correctly, or for mixed variables, consider creating ",
		 "separate continuous and discrete distributions and",
		 "running `distplyr::mix()`.")
}

#' @export
eval_pmf.parametric <- function(distribution, at, strict = TRUE) {
	v <- variable(distribution)
	if (v == "discrete") {
		function_name <- paste0("d", distribution$name)
		return(suppressWarnings(
			rlang::exec(function_name, at, !!!distribution$parameters)
		))
	}
	if (strict) {
		stop("Distribution is of variable type '", v, "'; pmf only ",
			 "exists for discrete variables. Perhaps you'd like to ",
			 "evaluate outside of strict mode?")
	}
	if (v == "continuous") {
		return(rep(0, length(at)))
	}
	stop("Evaluating non-strict pmf for a parametric distribution is ",
		 "not yet available. Check that your distribution's variable type ",
		 "is specified correctly, or for mixed variables, consider creating ",
		 "separate continuous and discrete distributions ",
		 "and running `distplyr::mix()`.")
}

#' @export
eval_quantile.parametric <- function(distribution, at) {
	function_name <- paste0("q", distribution$name)
	if (exists(function_name)) {
	  rlang::exec(function_name, at, !!!distribution$parameters)
	} else {
	  NextMethod()
	}
}

#' @export
realise.parametric <- function(distribution, n = 1) {
	function_name <- paste0("r", distribution$name)
	if (exists(function_name)) {
	  rlang::exec(function_name, n, !!!distribution$parameters)
	} else {
	  NextMethod()
	}
}
