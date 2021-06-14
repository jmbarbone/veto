test_that("veto works", {
  foo <- function(a, n = NULL) {
    veto_integer(a, n)
    a
  }

  expect_equal(foo(1L), 1L)
  expect_error(foo("b"), "<veto a: must be integer>")
  expect_error(foo(1L, 2L), "<veto a: must be integer length 2L>")
})
