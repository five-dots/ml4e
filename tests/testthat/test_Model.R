
test_that("Model instance works", {

  m <- new_model("glmnet_cv")
  m
  self <- m
  private <- m$.__enclos_env__$private
  super <- m$.__enclos_env__$super

  expect_is(m, "Model")
  expect_equal(m$engine, "glmnet_cv")
  expect_is(m$fit_params, "FitParams")
  expect_is(m$preproc_calls, "PreprocCalls")

  ## FitParamSpecs
  expect_null(m$fit_param_specs)
  expect_false(m$has_fit_param_specs)

  fps <- new_fit_param_specs_glmnet_cv()
  m$fit_param_specs <- fps
  expect_is(m$fit_param_specs, "FitParamSpecs")
  expect_true(m$has_fit_param_specs)

  ## Calls
  expect_is(m$spec, "list")
  expect_is(m$data_call, "call")
  expect_is(m$fit_call, "call")
  expect_is(m$pred_call, "call")

  expect_is(m$get_fit_call_by_key(1L), "call")
  expect_is(m$get_fit_call(list(alpha = 0.0)), "call")
  expect_error(m$get_fit_call(1L),
               "fit_param must be a named list.")

})
