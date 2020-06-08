
test_that("FitParams instance works", {

  f <- new_fit_params(list(alpha = 0.0, use_min = 1L))
  f
  self <- f
  private <- f$.__enclos_env__$private
  super <- f$.__enclos_env__$super

  expect_is(f, "FitParams")
  expect_is(f[1L], "list")
  expect_equal(dim(f$items), c(1L, 2L))

  expect_is(f$add(list(alpha = 1.0, use_min = 0L)), "FitParams")
  expect_equal(dim(f$items), c(2L, 2L))

  ## Default FitParams
  f_default <- new_fit_params()
  expect_is(f_default, "FitParams")
  expect_null(f_default[1L], "list")

  ## Error
  expect_error(f$add(c(1L, 2L)),
               "A value for key = \"par_03\" must be a list object.")

})
