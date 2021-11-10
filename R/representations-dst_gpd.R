#' @export
eval_cdf.gpd <- function(distribution, at) {
	with(parameters(distribution), {
		if (shape == 0) {
			left <- at < location
			z <- (at - location) / scale
			res <- 1 - exp(-z)
			res[left] <- 0
			res
		} else {
			if (shape > 0) {
				rightend <- Inf
			} else {
				rightend <- location - scale / shape
			}
			left <- at < location
			right <- at > rightend
			z <- (at - location) / scale
			res <- 1 - (1 + shape * z)^(-1 / shape)
			res[left] <- 0
			res[right] <- 1
			res
		}
	})
}

#' @export
eval_quantile.gpd <- function(distribution, at, ...) {
	with(parameters(distribution), {
		invalid <- at < 0 | at > 1
		if (shape == 0) {
			res <- location - scale * log(1 - at)
			res[invalid] <- NaN
			res
		} else {
			if (shape > 0) {
				rightend <- Inf
			} else {
				rightend <- location - scale / shape
			}
			t <- 1 / (1 - at)
			res <- location + scale * (t^shape - 1) / shape
			res[invalid] <- NaN
			res
		}
	})
}

#' @export
eval_density.gpd <- function(distribution, at, strict = TRUE) {
	with(parameters(distribution), {
		z <- (at - location) / scale
		if (shape == 0) {
			outside <- at < location
			res <- exp(-z) / scale
			res[outside] <- 0
			res
		} else {
			if (shape > 0) {
				rightend <- Inf
			} else {
				rightend <- location - scale / shape
			}
			outside <- at < location | at > rightend
			res <- (1 + shape * z)^(-1 / shape - 1) / scale
			res[outside] <- 0
			res
		}
	})
}

#' @export
eval_survival.gpd <- function(distribution, at) {
  1 - eval_cdf(distribution, at = at)
}
