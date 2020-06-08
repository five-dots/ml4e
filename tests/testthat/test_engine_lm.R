
test_that("lm for regression works", {

  keys_cv["model"] <- "lm"
  cv <- test_model(options_reg, keys_cv, "lm", pred_reg_col_names)

  ## normal prediction
  test <- cv$convert_data(cv$test)$data
  fitted <- cv$result$fits[[1L]]
  pred <- predict(fitted, newdata = test, type = "response")
  pred <- cv$.__enclos_env__$private$.to_num_1d(pred)

})
