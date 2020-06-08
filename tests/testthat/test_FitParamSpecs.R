
test_that("FitParamSpecs instance works", {

  f <- new_fit_param_specs(
    new_fit_param_spec(name = "alpha",
                       values = seq(-10.0, -1.0, 1.0),
                       default = -1.0,
                       trans = scales::log10_trans()),
    new_fit_param_spec(name = "use_min",
                       values = c(0L, 1L),
                       default = 1L)
  )
  f
  self <- f
  private <- f$.__enclos_env__$private

  expect_is(f, "FitParamSpecs")
  expect_is(f[1L], "FitParamSpec")
  expect_is(f["alpha"], "FitParamSpec")
  expect_equal(dim(f$items), c(2L, 2L))
  expect_equal(f$defaults, list(alpha = 0.1, use_min = 1L))

  expect_equal(dim(f$get_grid()), c(20L, 2L))
  expect_equal(dim(f$get_random(size = 10L)), c(10L, 2L))
  expect_is(f$get_bayes_info(), "list")

})
