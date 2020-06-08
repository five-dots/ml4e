
keras_model <- function() {
  keras_model <- keras::keras_model_sequential()
  initializer <- keras::initializer_glorot_uniform(seed = 1983L)
  keras_model %>%
    keras::layer_dense(units = 16L, activation = "relu", input_shape = 6L,
                       kernel_initializer = initializer) %>%
    keras::layer_dropout(rate = 0.2, seed = 1983L)
  keras_model
}

test_model <- function(options, keys, fitted_class, pred_col_names) {

  ## options <- options_mc
  ## model_key <- "glmnet_cv"
  ## keys <- c(formula = "all", dataset = "base", resample = "cv4",
  ##           seed = "sed_01", model = model_key, preproc = "ppc_01",
  ##           fit_param = "default")
  ## fitted_class <- "glmnet"
  ## pred_col_names <- pred_mc_col_names

  cv <- new_cv(options, keys)
  expect_is(cv, "CV")
  expect_is(cv$do()$result, "data.frame")

  ## data
  resamples <- cv$get_resample(cv$cv_seed)
  rsplit <- resamples$splits[[1L]]
  test <- cv$test
  ids <- cv$datasets$test_ids

  ## fit
  fitted <- cv$fit(rsplit)
  expect_is(fitted, fitted_class)

  ## task prediction
  pred <- cv$predict(fitted, test, ids)
  expect_named(pred, pred_col_names)
  expect_equal(nrow(pred), nrow(test))

  cv

}

test_prob <- function(pred1, pred2) {
  pred1 <- dplyr::select(pred1, dplyr::starts_with(".prob_"))
  pred2 <- dplyr::select(pred2, dplyr::starts_with(".prob_"))
  if (ncol(pred2) == 2L) pred2 <- pred2[, 2L]
  expect_true(dplyr::setequal(pred1, pred2))
}
