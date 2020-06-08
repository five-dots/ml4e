
test_that("CV instance works", {

  c <- cv_mc$clone(deep = TRUE)
  c
  self <- c
  private <- c$.__enclos_env__$private
  super <- c$.__enclos_env__$super

  ## fields
  expect_is(c, "CV")
  expect_null(c$fit_param)
  expect_is(c$fit_call, "call")
  expect_equal(c$error_count, list(error = 0L, folds = 0L))
  expect_equal(c$error_count_str, "0/0")

  ## public methods
  expect_is(c$get_resample(seed = 123L), "rset")

  train <- c$train
  data <- c$convert_data(train)
  expect_named(data, c("x", "y"))

  fitted <- c$fit(train)
  expect_is(fitted, "glmnet")

  test <- c$test
  pred <- c$predict(fitted, test)
  expect_is(pred, "data.frame")

  fitted <- c$get_fitted()
  expect_is(fitted, "glmnet")

  expect_false(c$done)
  expect_is(c$do(), "CV")
  expect_is(c$result, "data.frame")
  expect_true(c$done)
  expect_equal(c$error_count, list(error = 0L, folds = 4L))

  ## fields (after do())
  expect_equal(dim(c$cv_pred), c(300L, 6L))
  expect_equal(dim(c$test_pred), c(100L, 6L))
  expect_equal(dim(c$pred), c(400L, 6L))
  expect_is(c$cv_score, "list")
  expect_is(c$test_score, "list")

})

test_that("CV error instance works", {

  ## c <- cv_err$clone(deep = TRUE)
  ## c
  ## self <- c
  ## private <- c$.__enclos_env__$private
  ## super <- c$.__enclos_env__$super

  ## ## fields
  ## expect_is(c, "CV")
  ## ## expect_is(c$fit_param, "list")
  ## expect_null(c$fit_param)
  ## expect_is(c$fit_call, "call")

  ## ## public methods
  ## train <- c$train
  ## data <- c$convert_data(train)
  ## expect_named(data, c("x", "y"))

  ## fitted <- c$fit(train)
  ## expect_is(fitted, "simpleError")

  ## test <- c$test
  ## pred <- c$predict(fitted, test)
  ## expect_null(pred)

  ## expect_false(c$done)
  ## expect_is(c$do(), "CV")
  ## expect_is(c$result, "data.frame")
  ## expect_true(c$done)

  ## ## fields (after do())
  ## expect_equal(dim(c$cv_pred), c(300, 6))
  ## expect_equal(dim(c$test_pred), c(100, 6))
  ## expect_equal(dim(c$pred), c(400, 6))
  ## expect_is(c$cv_score, "list")
  ## expect_is(c$test_score, "list")

})
