
test_that("glmnet_cv for multiclass classification works", {

  keys_cv["model"] <- "glmnet_cv"
  cv <- test_model(options_mc, keys_cv, "glmnet", pred_mc_col_names)

  ## normal prediction
  test <- cv$convert_data(cv$test)$x
  fitted <- cv$result$fits[[1L]]
  pred <- predict(fitted, newx = test, type = "response")
  pred <- cv$.__enclos_env__$private$.to_prob_nd(pred)

})

test_that("glmnet_cv for binary classification works", {

  keys_cv["model"] <- "glmnet_cv"
  cv <- test_model(options_bin, keys_cv, "glmnet", pred_bin_col_names)

  ## normal prediction
  test <- cv$convert_data(cv$test)$x
  fitted <- cv$result$fits[[1L]]
  pred <- predict(fitted, newx = test, type = "response")
  pred <- cv$.__enclos_env__$private$.to_prob_1d(pred)

})

test_that("glmnet_cv for regression works", {

  keys_cv["model"] <- "glmnet_cv"
  cv <- test_model(options_reg, keys_cv, "glmnet", pred_reg_col_names)

  ## normal prediction
  test <- cv$convert_data(cv$test)$x
  fitted <- cv$result$fits[[1L]]
  pred <- predict(fitted, newx = test, type = "response")
  pred <- cv$.__enclos_env__$private$.to_num_1d(pred)

})

test_that("glmnet_cv for poisson works", {

  keys_cv["model"] <- "glmnet_cv"
  cv <- test_model(options_pois, keys_cv, "glmnet", pred_reg_col_names)

  ## normal prediction
  test <- cv$convert_data(cv$test)$x
  fitted <- cv$result$fits[[1L]]
  pred <- predict(fitted, newx = test, type = "response")
  pred <- cv$.__enclos_env__$private$.to_num_1d(pred)

})

test_that("FitParamSpecs for glmnet_cv works", {

  fps <- new_fit_param_specs_glmnet_cv()

  expect_is(fps, "FitParamSpecs")
  expect_equal(fps$keys, c("alpha", "use_min"))

})
