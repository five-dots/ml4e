
test_that("glm for binary classification works", {

  keys_cv["model"] <- "glm"
  cv <- test_model(options_bin, keys_cv, "glm", pred_bin_col_names)

  ## normal prediction
  test <- cv$convert_data(cv$test)$data
  fitted <- cv$result$fits[[1L]]
  pred <- predict(fitted, newdata = test, type = "response")
  pred <- cv$.__enclos_env__$private$.to_prob_1d(pred)

})

test_that("glm for regression works", {

  keys_cv["model"] <- "glm"
  cv <- test_model(options_reg, keys_cv, "glm", pred_reg_col_names)

  ## normal prediction
  test <- cv$convert_data(cv$test)$data
  fitted <- cv$result$fits[[1L]]
  pred <- predict(fitted, newdata = test, type = "response")
  pred <- cv$.__enclos_env__$private$.to_num_1d(pred)

})

test_that("glm for poisson works", {

  keys_cv["model"] <- "glm"
  cv <- test_model(options_pois, keys_cv, "glm", pred_reg_col_names)

  ## normal prediction
  test <- cv$convert_data(cv$test)$data
  fitted <- cv$result$fits[[1L]]
  pred <- predict(fitted, newdata = test, type = "response")
  pred <- cv$.__enclos_env__$private$.to_num_1d(pred)

})
