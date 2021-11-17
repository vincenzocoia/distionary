
<!-- README.md is generated from README.Rmd. Please edit that file -->

# distionary <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/distionary)](https://CRAN.R-project.org/package=distionary)
[![Codecov test
coverage](https://codecov.io/gh/vincenzocoia/distionary/branch/main/graph/badge.svg)](https://codecov.io/gh/vincenzocoia/distionary?branch=main)
[![R-CMD-check](https://github.com/vincenzocoia/distionary/workflows/R-CMD-check/badge.svg)](https://github.com/vincenzocoia/distionary/actions)
<!-- badges: end -->

The goal of distionary is to allow you to make and evaluate standard
families of univariate probability distributions.

## Installation

distionary is not on CRAN yet. You can download the development version
from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("vincenzocoia/distionary")
```

## Example

``` r
library(distionary)
```

We can make distributions from standard families, like beta and Poisson:

``` r
(d_beta <- dst_beta(2, 4))
#> beta parametric dst
#> 
#>  name :
#> [1] "beta"
(d_pois <- dst_pois(1.2))
#> pois parametric dst
#> 
#>  name :
#> [1] "pois"
```

You can also make your own family of distributions (see the vignette).

We can also make empirical distributions from data:

``` r
x <- c(4.1, 2.3, 3.4, 5.5, 1.0, 6.8)
(d_emp <- dst_empirical(x))
#> finite dst
#> 
#>  probabilities :
#> # A tibble: 6 × 2
#>   location  size
#>      <dbl> <dbl>
#> 1      1   0.167
#> 2      2.3 0.167
#> 3      3.4 0.167
#> 4      4.1 0.167
#> 5      5.5 0.167
#> 6      6.8 0.167
```

We can evaluate different distributional forms, such as the density or
pmf:

``` r
eval_density(d_beta, at = c(0.1, 0.2))
#> [1] 1.458 2.048
eval_pmf(d_pois, at = c(1, 1.5, 3))
#> [1] 0.36143305 0.00000000 0.08674393
```

Or, we can enframe the results in a tibble:

``` r
enframe_cdf(d_beta, d_pois, d_emp, at = c(0.1, 0.6, 1.5, 3))
#> # A tibble: 4 × 4
#>    .arg cdf_d_beta cdf_d_pois cdf_d_emp
#>   <dbl>      <dbl>      <dbl>     <dbl>
#> 1   0.1     0.0815      0.301     0    
#> 2   0.6     0.913       0.301     0    
#> 3   1.5     1           0.663     0.167
#> 4   3       1           0.966     0.333
```

Evaluate properties of the distributions:

``` r
mean(d_beta)
#> [1] 0.3333333
skewness(d_pois)
#> [1] 0.9128709
range(d_emp)
#> [1] 1.0 6.8
```

## Code of Conduct

Please note that the distionary project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
