
test_that("FitParamSpec instance works", {

  f <- new_fit_param_spec(
    name = "alpha",
    values = seq(-10.0, -1.0, 1.0),
    default = -1.0,
    trans = scales::log10_trans()
  )
  f
  self <- f
  private <- f$.__enclos_env__$private

  expect_is(f, "FitParamSpec")
  expect_equal(f$name, "alpha")
  expect_equal(f$values, seq(-10.0, -1.0, 1.0))
  expect_equal(f$default, -1.0)
  expect_equal(f$trans, scales::log10_trans())

  expect_equal(f$nlevels, 10L)
  expect_equal(f$type, "double")
  expect_equal(f$range, list(lower = -10.0, upper = -1.0))
  expect_true(f$has_default)
  expect_equal(f$trans_values, purrr::map_dbl(-10L:-1L, ~ 10.0^.))
  expect_equal(f$trans_default, 0.1)

  expect_is(f$dials_param, "quant_param")
  expect_is(f$get_dials_param(), "quant_param")

  ## Error
  expect_error(
    new_fit_param_spec(
      name = "alpha",
      ## factor is not allowed
      values = factor("a", "b", "c"),
      default = factor("a")
    )
  )

  expect_error(
    new_fit_param_spec(
      name = "alpha",
      values = c("a", "b", "c"),
      default = 1L
    ),
    "Default must be length 1 and the same type of the values."
  )

})
