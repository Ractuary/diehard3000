context("p_x")

test_that("p_x() function returns correct values for integer LifeTable", {
  l <- LifeTable(x = 0:3,
                 q_x = c(0.06, 0.04, 0.05, NA)
       )
  expect_equal(p_x(l, x_ = 0, t_ = 1), 0.94)
  expect_equal(p_x(l, x_ = 0, t_ = 2), 0.94 * 0.96)
  expect_equal(p_x(l, x_ = 0, t_ = 3), 0.94 * 0.96 * 0.95)
  expect_equal(p_x(l, x_ = 0.5, t_ = 0.5), 0.9690722, tolerance = 0.000001)
  expect_equal(p_x(l, x_ = 0.8, t_ = 1), 0.95579831, tolerance = 0.000001)
  expect_equal(p_x(l, x_ = 0.8, t_ = 2), 0.909983193, tolerance = 0.000001)
})

test_that("p_x() function returns correct values for noninteger LifeTable", {
  l <- LifeTable(x = c(0.2, 0.9, 1.6, 2.5),
                 q_x = c(0.06, 0.04, 0.05, NA)
  )
  expect_equal(p_x(l, x_ = 0.2, t_ = 0.7), 0.94)
  expect_equal(p_x(l, x_ = 0.2, t_ = 1.4), 0.94 * 0.96)
  expect_equal(p_x(l, x_ = 0.2, t_ = 2.3), 0.94 * 0.96 * 0.95)
  expect_equal(p_x(l, x_ = 0.4, t_ = 0.5), 0.95639534, tolerance = 0.000001)
  expect_equal(p_x(l, x_ = 0.8, t_ = 0.5), 0.968313253, tolerance = 0.000001)
  expect_equal(p_x(l, x_ = 0.8, t_ = 0.9), 0.94604016, tolerance = 0.000001)
})