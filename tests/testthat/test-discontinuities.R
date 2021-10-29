# car <- data.frame(
#   hp = c(
#     110, 110, 93, 110, 175, 105, 245, 62, 95, 123, 123,
#     180, 180, 180, 205, 215, 230, 66, 52, 65, 97, 150,
#     150, 245, 175, 66, 91, 113, 264, 175, 335, 109
#   )
# )
#
# test_that("Discontinuites Norm works", {
#   expect_equal(
#     discontinuities(dst_norm(1, 2), -Inf, Inf),
#     make_empty_discontinuities_df()
#   )
#   expect_equal(
#     discontinuities(dst_norm(-1, 0.343), -100023, 1232),
#     make_empty_discontinuities_df()
#   )
#   expect_equal(
#     discontinuities(dst_norm(-1, 0.343)),
#     make_empty_discontinuities_df()
#   )
# })
#
# test_that("Discontinuites Uniform works", {
#   expect_equal(
#     discontinuities(dst_unif(1, 2), -Inf, Inf),
#     make_empty_discontinuities_df()
#   )
#   expect_equal(
#     discontinuities(dst_norm(-1, 0.343), -1, 100),
#     make_empty_discontinuities_df()
#   )
#   expect_equal(
#     discontinuities(dst_norm(-1, 0.343)),
#     make_empty_discontinuities_df()
#   )
# })
#
# test_that("Discontinuites GPD works", {
#   expect_equal(
#     discontinuities(dst_gpd(1, 2, -1), -Inf, Inf),
#     make_empty_discontinuities_df()
#   )
#   expect_equal(
#     discontinuities(dst_gpd(-1, 0.343, 12), 1, 1000),
#     make_empty_discontinuities_df()
#   )
#   expect_equal(
#     discontinuities(dst_gpd(-1, 0.343, 12)),
#     make_empty_discontinuities_df()
#   )
# })
#
# test_that("Discontinuites Poisson works", {
#   expect_equal(
#     discontinuities(dst_pois(3), 0, 1),
#     tibble::tibble(
#       location = c(0, 1),
#       size = c(dpois(0, 3), dpois(1, 3))
#     )
#   )
#
#   results <- dpois(0:10, 18)
#   expect_equal(
#     discontinuities(dst_pois(18), 0, 10),
#     tibble::tibble(
#       location = 0:10,
#       size = results
#     )
#   )
#   expect_error(
#     discontinuities(dst_pois(1456))
#   )
#   expect_error(
#     discontinuities(dst_pois(1), 1, 0)
#   )
# })
#
# test_that("Discontinuites Finite works", {
#   expect_equal(
#     discontinuities(dst_finite(1:5, rep(0.2, 5)), 1, 4),
#     tibble::tibble(location = c(1, 2, 3, 4), size = rep(0.2, 4))
#   )
#   expect_equal(
#     discontinuities(dst_empirical(hp, data = car), 58, 80),
#     tibble::tibble(
#       location = c(62, 65, 66),
#       size = c(1 / 32, 1 / 32, 1 / 16)
#     )
#   )
#   expect_equal(
#     discontinuities(dst_empirical(hp, data = car), 52, 500),
#     tibble::tibble(location = sort(unique(car$hp)), size = c(
#       1 / 32, 1 / 32, 1 / 32, 2 / 32, 1 / 32, 1 / 32, 1 / 32,
#       1 / 32, 1 / 32, 1 / 32, 3 / 32, 1 / 32, 2 / 32, 2 / 32,
#       3 / 32, 3 / 32, 1 / 32, 1 / 32, 1 / 32, 2 / 32, 1 / 32,
#       1 / 32
#     ))
#   )
#   expect_equal(
#     discontinuities(dst_finite(1:5, rep(0.2, 5)), 58, 80),
#     make_empty_discontinuities_df()
#   )
#   expect_equal(
#     discontinuities(dst_finite(1:5, rep(0.2, 5))),
#     tibble::tibble(
#       location = 1:5,
#       size = rep(0.2, 5)
#     )
#   )
# })
#
# test_that("Discontinuites Degenerate works", {
#   expect_equal(
#     discontinuities(dst_degenerate(1), -Inf, Inf),
#     tibble::tibble(location = 1, size = 1)
#   )
#   expect_equal(
#     discontinuities(dst_degenerate(5), 0, 456),
#     tibble::tibble(location = 5, size = 1)
#   )
#   expect_equal(
#     discontinuities(dst_degenerate(5), 5, 54),
#     tibble::tibble(location = 5, size = 1)
#   )
#   expect_equal(
#     discontinuities(dst_degenerate(45), 56, 75654),
#     make_empty_discontinuities_df()
#   )
#   expect_equal(
#     discontinuities(dst_degenerate(4546455)),
#     tibble::tibble(location = 4546455, size = 1)
#   )
# })
#
# test_that("Discontinuites Mix works", {
#   expect_equal(
#     discontinuities(mix(
#       dst_norm(1, 4),
#       dst_norm(1, 3)
#     ), 1, 3),
#     tibble::tibble(location = numeric(), size = numeric())
#   )
#   expect_equal(
#     discontinuities(mix(
#       dst_norm(1, 4),
#       dst_unif(1, 3)
#     ), 4, 5),
#     tibble::tibble(location = numeric(), size = numeric())
#   )
#   expect_equal(
#     discontinuities(mix(
#       dst_norm(1, 4),
#       dst_gpd(1, 3, 34)
#     ), 7, 2342342),
#     tibble::tibble(location = numeric(), size = numeric())
#   )
#   expect_equal(
#     discontinuities(mix(
#       dst_norm(1, 5),
#       dst_degenerate(2)
#     )),
#     tibble::tibble(location = c(2), size = c(0.5))
#   )
#   expect_equal(
#     discontinuities(mix(
#       dst_finite(1:5, rep(0.2, 5)),
#       dst_degenerate(2)
#     ), 1, 6),
#     tibble::tibble(location = 1:5, size = c(0.1, 0.6, 0.1, 0.1, 0.1))
#   )
#   expect_equal(
#     discontinuities(mix(
#       dst_finite(1:5, rep(0.2, 5)),
#       dst_degenerate(2),
#       dst_norm(5, 76)
#     )),
#     tibble::tibble(location = 1:5, size = c(
#       0.2 / 3, (1 + 0.2) / 3,
#       0.2 / 3, 0.2 / 3, 0.2 / 3
#     ))
#   )
#   expect_equal(
#     discontinuities(mix(
#       dst_finite(1:5, rep(0.2, 5)),
#       dst_degenerate(2),
#       dst_empirical(hp, data = car)
#     ), 1, 6),
#     tibble::tibble(location = 1:5, size = c(
#       0.2 / 3, (1 + 0.2) / 3,
#       0.2 / 3, 0.2 / 3, 0.2 / 3
#     ))
#   )
#   expect_equal(
#     discontinuities(mix(
#       dst_finite(1:5, rep(0.2, 5)),
#       dst_finite(1:5, rep(0.2, 5)),
#     ), 2, 6),
#     tibble::tibble(location = 2:5, size = rep(0.2, 4))
#   )
#   expect_equal(
#     discontinuities(mix(
#       dst_finite(1:5, rep(0.2, 5)),
#       dst_empirical(hp, data = car)
#     )),
#     tibble::tibble(
#       location = c(1:5, sort(unique(car$hp))),
#       size = c(
#         rep(0.1, 5),
#         1 / 64, 1 / 64, 1 / 64, 2 / 64, 1 / 64, 1 / 64, 1 / 64,
#         1 / 64, 1 / 64, 1 / 64, 3 / 64, 1 / 64, 2 / 64, 2 / 64,
#         3 / 64, 3 / 64, 1 / 64, 1 / 64, 1 / 64, 2 / 64, 1 / 64,
#         1 / 64
#       )
#     )
#   )
#   expect_equal(
#     discontinuities(
#       mix(
#         mix(
#           dst_finite(1:5, rep(0.2, 5)),
#           dst_degenerate(6)
#         ),
#         dst_norm(1, 5)
#       ), -94, 465
#     ),
#     tibble::tibble(
#       location = c(1:5, 6), size = c(rep(0.2 / 4, 5), 1 / 4)
#     )
#   )
#   expect_equal(
#     discontinuities(
#       mix(
#         mix(
#           dst_finite(1:5, rep(0.2, 5)),
#           dst_degenerate(6)
#         ),
#         dst_norm(1, 5)
#       ), -94, 5
#     ),
#     tibble::tibble(
#       location = c(1:5), size = c(rep(0.2 / 4, 5))
#     )
#   )
#   expect_equal(
#     discontinuities(
#       mix(
#         mix(
#           dst_unif(3, 7),
#           dst_gpd(6, 4, 5)
#         ),
#         dst_norm(1, 5)
#       ), -94, 5
#     ),
#     tibble::tibble(
#       location = numeric(), size = numeric()
#     )
#   )
#   expect_equal(
#     discontinuities(
#       mix(
#         dst_pois(1),
#         dst_norm(1, 5)
#       ), 0, 2
#     ),
#     tibble::tibble(
#       location = 0:2,
#       size = c(dpois(0, 1) / 2, dpois(1, 1) / 2, dpois(2, 1) / 2)
#     )
#   )
#   expect_error(
#     discontinuities(
#       mix(
#         dst_pois(14),
#         dst_norm(56, 8)
#       )
#     )
#   )
#   expect_error(
#     discontinuities(
#       mix(
#         dst_pois(14),
#         dst_norm(56, 8)
#       ), -1, -6
#     )
#   )
# })
#
# rm("car")
