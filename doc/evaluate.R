## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(distionary)

## -----------------------------------------------------------------------------
a <- -1
b <- 1
# Look up formula for variance:
(b - a) ^ 2 / 12
# Get quantiles:
qunif(c(0.25, 0.75), min = a, max = b)
# Get sample of size 10:
runif(10, min = a, max = b)

## -----------------------------------------------------------------------------
d <- dst_unif(-1, 1)
variance(d)
eval_quantile(d, at = c(0.25, 0.75))
realise(d, 10)

## -----------------------------------------------------------------------------
eval_hazard(d, at = 0:10)
enframe_density(d, at = 0:10)
set.seed(10)

## -----------------------------------------------------------------------------
# half_marathon <- tribble(
# 	~ person, ~ race_time_min,
# 	"Vincenzo", dst_norm(130, 25),
# 	"Colleen", dst_norm(110, 13),
# 	"Regina", dst_norm(115, 20)
# ) 
# half_marathon %>% 
# 	mutate(quartiles = map(race_time_min, enframe_quantile, at = 1:3 / 4)) %>% 
# 	unnest(quartiles)

## -----------------------------------------------------------------------------
realise(d, n = 5)

## -----------------------------------------------------------------------------
realise(d)

## -----------------------------------------------------------------------------
# half_marathon %>% 
# 	mutate(actual_time_min = map_dbl(race_time_min, realise))

## -----------------------------------------------------------------------------
mean(d)
stdev(d)
evi(d)

