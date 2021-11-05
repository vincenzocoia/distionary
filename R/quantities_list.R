#' Specific Formulas for Quantities
#'
#' Formulas for quantities (such as mean, variance, skewness, EVI, etc.)
#' of select parametric distributions.
#'
#' @details A list, where each distribution gets a (named) entry, with name
#' given by the suffix of `dst_` (such as "norm", "unif", etc.). Each
#' distribution's entry is itself a named list of expressions, where the
#' name is the name of the quantity matching the distionary function name:
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
    variance = sd ^ 2,
    stdev = sd,
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
  ),
  binom = rlang::exprs(
    mean = size * p,
    variance = size * p * (1-p),
    skewness = ((1-p) - p)/sqrt( size * p * (1-p)),
    kurtosis_exc = (1 - 6 * p * (1-p))/(size * p * (1-p)),
    range = c(0, size)
  ),
  nbinom = rlang::exprs(
    mean = prob * size / (1 - prob),
    #median = FILL_THIS_IN,
    variance = prob * size/((1- prob)^2),
    skewness = (1 + prob)/sqrt(prob * size),
    kurtosis_exc = 6/size + ((1-prob)^2)/(prob*size),
    range = c(0, 1), # need to double check
    #evi = FILL_THIS_IN not sure
  ),
  geom = rlang::exprs(
    mean = 1/p,
    #median = ifelse((-1)/log2(1 - p)%%1 != 0, (-1)/log2(1 - p), 'No unique integer'), # not sure
    variance = (1 - p)/p^2,
    skewness = (2 - p)/sqrt(1 - p),
    kurtosis_exc = 6 + p^2/(1 - p),
    range = c(0, 1)
    #evi = FILL_THIS_IN not sure
  ),
  exp = rlang::exprs(
    mean = 1/rate,
    median = log(2)/rate,
    variance = 1/rate^2,
    skewness = 2,
    kurtosis_exc = 6,
    range = c(0, Inf),
    evi = 0
  ),
  weibull = rlang::exprs(
    mean = lambda*gamma(1 + 1/k),
    median = lambda*(log10(2)^(1/k)),
    variance = lambda^2 * (gamma(1 + 2/k) - (gamma(1 + 1/k))^2),
    skewness = (gamma(1 + 3/k)*(lambda^3) - 3*lambda*gamma(1 + 1/k)*(sqrt(lambda^2 * (gamma(1 + 2/k) - (gamma(1 + 1/k))^2))^2)  - (lambda*gamma(1 + 1/k))^3)/sqrt(lambda^2 * (gamma(1 + 2/k) - (gamma(1 + 1/k))^2))^3,
    kurtosis_exc = ((lambda^4) * gamma(1 + 4/k) - 4*((gamma(1 + 3/k)*(lambda^3) - 3*lambda*gamma(1 + 1/k)*(sqrt(lambda^2 * (gamma(1 + 2/k) - (gamma(1 + 1/k))^2))^2)  - (lambda*gamma(1 + 1/k))^3)/sqrt(lambda^2 * (gamma(1 + 2/k) - (gamma(1 + 1/k))^2))^3)*(sigma^3)*(lambda*gamma(1 + 1/k)) - 6*(mu^2)*(lambda^2 * (gamma(1 + 2/k) - (gamma(1 + 1/k))^2)) - (lambda*gamma(1 + 1/k))^4)/(sqrt(lambda^2 * (gamma(1 + 2/k) - (gamma(1 + 1/k))^2))^4) - 3,
    range = c(0, Inf)
    #evi = FILL_THIS_IN
  ),
  gamma = rlang::exprs(
    mean = alpha/beta,
    variance = alpha/beta^2,
    skewness = 2/sqrt(alpha),
    kurtosis_exc = 6/alpha,
    range = c(0, Inf)
    #evi = FILL_THIS_IN
  ),
  chisq = rlang::exprs(
      mean = k,
      median = k*((1 - 2/9*k)^3),
      variance = 2*k,
      skewness = sqrt(8/k),
      kurtosis_exc = 12/k,
      range = c(0, 1)
    ),
  cauchy = rlang::exprs(
      mean = NaN,
      median = location,
      variance = NaN,
      skewness = NaN,
      kurtosis_exc = NaN,
      evi = 1,
      range = c(-Inf, Inf)
  ),
  hyper = rlang::exprs(
       mean = n * K / N,
       #median = FILL_THIS_IN,
       variance = n* K/N * (N-K)/N * (N-n)/(N-1),
       skewness = (N - 2*K)((N-1)^(1/2))(N - 2*n)/
                    (((n*K*(N-K)(N - n))^(1/2))(N - 2)),
       kurtosis_exc = 1/(n*K*(N - K)*(N-n)*(N-2)*(N-3)) *
                        ((N-1)*(N^2) *(N*(N+1)-6*K(N-K) - 6*n*(N-n)) +
                           6*n*K*(N-K)*(N-n)*(5*N-6)),
       range = c(0, 1)
       #evi = FILL_THIS_IN
  ),
  t = rlang::exprs(
       mean = ifelse(df > 1, 0, NaN),
       median = 0,
       variance = ifelse(df > 2, df/(df-2), ifelse((df > 1 & df <= 2), Inf, NaN)),
       skewness = ifelse(df > 3, 0, NaN),
       kurtosis_exc = ifelse(df > 4, 6/(df - 4), ifelse((df > 2 & df <= 4), Inf, NaN)),
       range = c(0, 1),
       evi = 0 # not sure
  ),
  f = rlang::exprs(
    mean = ifelse(df2 > 4, df2 / (df2 - 2), NaN),
    #median = FILL_THIS_IN,
    variance = ifelse(df2 > 4, (2*(df2^2)(df1 + df2 -2))/(df1*((df2-2)^2)(df2-4)), NaN),
    skewness = ifelse(df2 > 6, ((2*df1 + df2 -2)*sqrt(8*(df2-4)))/
                        ((df2-6)*sqrt(df1 * (df1+df2-2))), NaN),
    kurtosis_exc = ifelse(df2 > 8, 12*(df1*(5*df2 - 22)*(df1+df2-2) + (df2-4)*((df2-2)^2)/
                            (df1*(df2-6)*(df2-8)(df1+df2-2))), NaN),
    range = c(0, 1),
    #evi = FILL_THIS_IN # not sure
  ),
  gev = rlang::exprs(
    mean = ifelse(shape >=1, Inf,
                  ifelse(shape == 0, location - scale * digamma(1),
                         location + (scale*(gamma(1-shape) -1))/shape)),
    median = ifelse(shape != 0, location + scale*(log10(2)^(-shape)-1)/shape, location - scale*log10(log10(2))),
    variance = ifelse(shape >= 1/2, Inf,
                      ifelse(shape == 0, (scale^2)*(pi^2)/6,
                             (scale^2)*(gamma(1-2*shape)-(gamma(1-shape))^2)/shape^2)),
    skewness = ifelse(shape == 0, 12*sqrt(6)*zeta(3)/(pi^3),
                      ifelse(shape < 1/3,
                             sign(shape)*(gamma(1-3*shape)-3*(gamma(1-2*shape))*(gamma(1-shape))+2*gamma(1-shape)^3)/((gamma(1-2*shape)-gamma(1-shape)^2)^(3/2)), NaN)),
    kurtosis_exc = ifelse(shape == 0, 12/5,
                          ifelse(shape < 1/4,
                                 (gamma(1-4*shape)-4*gamma(1-4*shape)*gamma(1-shape) - 3*gamma(1-2*shape)^2 + 12*gamma(1-2*shape)*(gamma(1-shape)^2) - 6*gamma(1-shape)^4)/(gamma(1-2*shape)-gamma(1-shape)^2)^2, NaN)),
    range = {
      if (shape > 0) {
        c(location - scale / shape, Inf)
      } else if (shape == 0) {
        c(-Inf, Inf)
      } else if (shape < 0) {
        c(-Inf, location - scale / shape)
      }
    },
    evi = shape
  )




# rlang::exprs(
#   mean = FILL_THIS_IN,
#   median = FILL_THIS_IN,
#   variance = FILL_THIS_IN,
#   skewness = FILL_THIS_IN,
#   kurtosis_exc = FILL_THIS_IN,
#   range = c(FILL_THIS_IN, FILL_THIS_IN),
#   evi = FILL_THIS_IN
)
