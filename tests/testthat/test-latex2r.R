test_that("sub-supscript reordering works", {
  expect_equal(latex2r("x_1^n"), latex2r("x^n_1"))
  expect_equal(latex2r("x^n_1"), "x_1^n")
})

