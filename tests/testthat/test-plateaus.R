x <- 1:5
y <- c(1, 3, 2, Inf, 4, -Inf)
fr <- stepfun(x, y, right = TRUE)
fl <- stepfun(x, y, right = FALSE)

test_that("plateaus works", {
  expect_equal(plateaus(fr), y)
  expect_equal(plateaus(fl), y)
})
