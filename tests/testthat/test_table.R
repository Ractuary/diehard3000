library(insuree)
context("expected")

test_that("expected returns correct value", {
  expect_equal(round(expected(LifeTable(), x_ = 2), 6), 5.264875)
  expect_equal(expected(LifeTable(), x_ = 0, t_ = 1), 0.975)
  expect_equal(expected(LifeTable(), x_ = 0, t_ = 1, m_ = 1), 0.97)
})