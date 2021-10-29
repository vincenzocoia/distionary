# Note that the "right" argument of `stepfun` is the opposite
# of the desired continuity: specify `right = FALSE` if you
# *do* want right-continuity, and TRUE if you want *left*
# continuity.
f_left <- stats::stepfun(0, 1:2, right = TRUE)
f_right <- stats::stepfun(0, 1:2, right = FALSE)

test_that("sanity check that f_left and f_right are labelled correctly", {
  expect_equal(f_left(c(-0.1, 0, 0.1)), c(1, 1, 2))
  expect_equal(f_right(c(-0.1, 0, 0.1)), c(1, 2, 2))
})

test_that("checks for left and right continuity work", {
  expect_true(check_right_continuous(f_right))
  expect_true(check_left_continuous(f_left))
  expect_false(check_right_continuous(f_left))
  expect_false(check_left_continuous(f_right))
})
