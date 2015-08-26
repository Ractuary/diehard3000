library(insuree)
context("expected")

l <- LifeTable()

test_that("p_x function", {
  expect_equal(p_x(l, x_ = 2, t_ = 1), 0.93)
})