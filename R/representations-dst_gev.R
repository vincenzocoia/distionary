#' @export
eval_cdf.gev <- function(distribution, at) {
  with(parameters(distribution), {
    s = (at - location) / scale
    if (shape == 0) {
      res <- exp(-exp(- s))
      res
    } else {
      if (shape * s > -1) {
        res <- exp(-(1 + shape * s) ^ (-1 / shape))
        res
      } else {
        if (shapen > 0) {
          res <- 0
          res
        } else {
          res <- 1
          res
        }
      }
    }
  })
}

#' @export
eval_quantile.gev <- function(distribution, at, ...) {
  with(parameters(distribution), {
    invalid <- at < 0 | at > 1
    if (shape == 0) {
      res <- location - scale * log(- log(p) )
      res[invalid] <- NaN
      res
    } else {
      if (shape > 0) {
        invalid_at <- at == 1
        res <- location + scale * ((- log(p)) ^ (- shape) - 1) / shape
        res[invalid_at] <- NaN
        res
      } else {
        invalid_at <- at == 0
        res <- location + scale * ((- log(p)) ^ (- shape) - 1) / shape
        res[invalid_at] <- NaN
        res
      }
    }
  })
}

#' @export
eval_density.gev <- function(distribution, at, strict = TRUE) {
  with(parameters(distribution), {
    s = (at - location) / scale
    if (shape == 0) {
      res <- d = log( 1 / scale) - s - exp(- s)
      res
    } else {
      if (shape * s > -1) {
        ss <- 1 + shape * s
        sp <- 1 / shape
        res <- ss^(- (1 + sp)) * exp(- ss^(- sp))
        res
      } else {
        res <- 0
      }
    }
  })
}
