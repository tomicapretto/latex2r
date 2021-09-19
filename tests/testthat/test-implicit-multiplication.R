test_that("implicit multiplication constants", {
  expect_equal(latex2r("ab"), "a * b")
  expect_equal(latex2r("abc"), "a * b * c")
  expect_equal(latex2r("a(b)c"), "a * (b) * c")
})

test_that("implicit multiplication number/constant", {
  expect_equal(latex2r("2b"), "2 * b")
  expect_equal(latex2r("2b3"), "2 * b * 3")
  expect_equal(latex2r("(2b)3"), "(2 * b) * 3")
})

test_that("implicit multiplication special numbers", {
  expect_equal(latex2r("\\pi 3"), "pi * 3")
  expect_equal(latex2r("3\\pi"), "3 * pi")
  expect_equal(latex2r("e3"), "exp(1) * 3")
  expect_equal(latex2r("3e"), "3 * exp(1)")
})


test_that("implicit multiplication functions", {
  expect_equal(latex2r("\\log{5}a"), "log(5) * a")
  expect_equal(latex2r("a\\log{5}"), "a * log(5)")
  expect_equal(latex2r("\\sin(x)\\cos(y)"), "sin(x) * cos(y)")
})


test_that("implicit multiplication sup-subscript", {
  expect_equal(latex2r("x^{22}y"), "x^(22) * y")
  expect_equal(latex2r("yx^{22}"), "y * x^(22)")
  expect_equal(latex2r("\\beta_1^2x"), "beta_1^2 * x")
})


