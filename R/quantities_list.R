#' Specific Formulas for Quantities
#'
#' Formulas for quantities (such as mean, variance, skewness, EVI, etc.)
#' of select parametric distributions.
#'
#' @details A list, where each distribution gets a (named) entry, with name
#' given by the suffix of `dst_` (such as "norm", "unif", etc.). Each
#' distribution's entry is itself a named list of expressions, where the
#' name is the name of the quantity matching the distplyr function name:
#'
#' - mean
#' - median
#' - variance
#' - skewness
#' - kurtosis_exc
#' - range
#' - evi
#'
#' Each expression is allowed to
#' refer to the distribution's parameters by name.
#'
#'
#' @note Although R allows us to evaluate distributional representations
#' of certain parametric distributions through functions with
#' `p`, `d`, `q`, and `r` prefixes (such as `pnorm()`, `dnorm()`, etc.),
#' R does not "come with" formulas for quantities such as mean, variance,
#' EVI, etc. Although these quantities can be computed from a distributional
#' representation (such as integrating the quantile function to get the mean),
#' it's often inefficient to rely on such computations. We therefore include
#' formulas here, and check them using testthat.
.quantities <- list(
	gpd = rlang::exprs(
		mean = ifelse(shape < 1, location + scale / (1 - shape), Inf),
		variance = ifelse(shape < 1 / 2,
						  scale^2 / (1 - shape)^2 / (1 - 2 * shape),
						  Inf),
		skewness = ifelse(shape < 1 / 3,
						  2 * (1 + shape) * sqrt(1 - 2 * shape) /
						  	(1 - 3 * shape),
						  Inf),
		kurtosis_exc = ifelse(shape < 1 / 4,
							  3 * (1 - 2 * shape) * (2 * shape^2 + shape + 3) /
							  	((1 - 3 * shape) * (1 - 4 * shape)) - 3,
							  Inf),
		range = c(location,
				  ifelse(shape >= 0, Inf, location - (scale / shape))),
		evi = shape
	),
	lnorm = rlang::exprs(
		mean = exp(meanlog + variancelog / 2),
		median = exp(meanlog),
		variance = {
			ev <- exp(variancelog)
			(ev - 1) * ev * exp(2 * meanlog)
		},
		skewness = {
			ev <- exp(variancelog)
			(ev + 2) * sqrt(ev - 1)
		},
		kurtosis_exc = {
			e4 <- exp(4 * variancelog)
			e3 <- exp(3 * variancelog)
			e2 <- exp(2 * variancelog)
			e4 + 2 * e3 + 3 * e2 - 6
		},
		range = c(0, Inf)
	),
	norm = rlang::exprs(
		mean = mean,
		median = mean,
		variance = variance,
		skewness = 0,
		kurtosis_exc = 0,
		range = c(-Inf, Inf),
		evi = 0
	),
	pois = rlang::exprs(
		mean = lambda,
		variance = lambda,
		skewness = lambda^(-0.5),
		kurtosis_exc = 1 / lambda,
		range = c(0, Inf)
	),
	unif = rlang::exprs(
		mean = (min + max) / 2,
		median = (min + max) / 2,
		variance = (min - max)^2 / 12,
		skewness = 0,
		kurtosis_exc = -6 / 5,
		range = c(min, max),
		evi = -1
	),
	beta = rlang::exprs(
		mean = shape1 / (shape1 + shape2),
		variance = shape1 * shape2 / (shape1 + shape2)^2 /
			(shape1 + shape2 + 1),
		skewness = 2 * (shape2 - shape1) * sqrt(shape1 + shape2 + 1) /
			(shape1 + shape2 + 2) / sqrt(shape1 * shape2),
		#kurtosis_exc = FILL_THIS_IN,
		#evi = FILL_THIS_IN,
		range = c(0, 1)
	)
)


rlang::exprs(
	mean = FILL_THIS_IN,
	median = FILL_THIS_IN,
	variance = FILL_THIS_IN,
	skewness = FILL_THIS_IN,
	kurtosis_exc = FILL_THIS_IN,
	range = c(FILL_THIS_IN, FILL_THIS_IN),
	evi = FILL_THIS_IN
)
