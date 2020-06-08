
test_that("data_identity() works", {

  rsplit <- rsplit_reg
  train <- train_reg
  test <- test_reg

  ## rsplit
  data <- data_identity(rsplit)
  expect_is(data, "list")
  expect_named(data, c("data", "eval"))
  expect_is(data$data, "data.frame")
  expect_is(data$eval, "data.frame")
  expect_equal(dim(data$data), c(300, 6))
  expect_equal(dim(data$eval), c(100, 6))

  ## train (same as rsplit)
  data <- data_identity(train)
  expect_is(data, "list")
  expect_is(data$data, "data.frame")
  expect_equal(dim(data$data), c(300, 6))

  ## test
  data <- data_identity(test)
  expect_is(data, "list")
  expect_is(data$data, "data.frame")
  expect_equal(dim(data$data), c(100, 6))

})

test_that("data_xy() works", {

  rsplit <- rsplit_mc
  train <- train_mc
  test <- test_mc

  ## rsplit
  xy <- data_xy(rsplit, formula)
  expect_is(xy, "list")
  expect_named(xy, c("x", "y", "eval_x", "eval_y"))
  expect_is(xy$x, "matrix")
  expect_is(xy$y, "factor")
  expect_is(xy$eval_x, "matrix")
  expect_is(xy$eval_y, "factor")
  expect_equal(dim(xy$x), c(300, 6))
  expect_length(xy$y, 300)
  expect_equal(dim(xy$eval_x), c(100, 6))
  expect_length(xy$eval_y, 100)

  ## train (same as rsplit)
  xy <- data_xy(train, formula)
  expect_is(xy, "list")
  expect_is(xy$x, "matrix")
  expect_is(xy$y, "factor")
  expect_equal(dim(xy$x), c(300, 6))
  ## test
  xy <- data_xy(test, formula)
  expect_is(xy, "list")
  expect_is(xy$x, "matrix")
  expect_is(xy$y, "factor")
  expect_equal(dim(xy$x), c(100, 6))

})

test_that("data_xgboost() works", {

  rsplit <- rsplit_reg
  train <- train_reg
  test <- test_reg

  ## rsplit
  data <- data_xgboost(rsplit, formula)
  expect_is(data, "list")
  expect_named(data, c("x", "y", "data", "eval"))
  expect_is(data$x, "dgCMatrix")
  expect_is(data$y, "numeric")
  expect_is(data$data, "xgb.DMatrix")
  expect_is(data$eval, "xgb.DMatrix")
  expect_equal(dim(data$data), c(300, 6))
  expect_equal(dim(data$eval), c(100, 6))

  ## train (same as rsplit)
  data <- data_xgboost(train, formula)
  expect_is(data, "list")
  expect_named(data, c("x", "y", "data"))
  expect_is(data$x, "dgCMatrix")
  expect_is(data$y, "numeric")
  expect_is(data$data, "xgb.DMatrix")
  expect_equal(dim(data$data), c(300, 6))

  ## test
  data <- data_xgboost(test, formula)
  expect_is(data, "list")
  expect_named(data, c("x", "y", "data"))
  expect_is(data$x, "dgCMatrix")
  expect_is(data$y, "numeric")
  expect_is(data$data, "xgb.DMatrix")
  expect_equal(dim(data$data), c(100, 6))

})

test_that("data_lightgbm() works", {

  rsplit <- rsplit_reg
  train <- train_reg
  test <- test_reg

  ## rsplit
  data <- data_lightgbm(rsplit, formula)
  expect_is(data, "list")
  expect_named(data, c("x", "y", "data", "eval"))
  expect_is(data$x, "dgCMatrix")
  expect_is(data$y, "numeric")
  expect_is(data$data, "lgb.Dataset")
  expect_is(data$eval, "lgb.Dataset")
  expect_equal(dim(data$data), c(300, 6))
  expect_equal(dim(data$eval), c(100, 6))

  ## train (same as rsplit)
  data <- data_lightgbm(train, formula)
  expect_is(data, "list")
  expect_named(data, c("x", "y", "data"))
  expect_is(data$x, "dgCMatrix")
  expect_is(data$y, "numeric")
  expect_is(data$data, "lgb.Dataset")
  expect_equal(dim(data$data), c(300, 6))

  ## test
  data <- data_lightgbm(test, formula)
  expect_is(data, "list")
  expect_named(data, c("x", "y", "data"))
  expect_is(data$x, "dgCMatrix")
  expect_is(data$y, "numeric")
  expect_is(data$data, "lgb.Dataset")
  expect_equal(dim(data$data), c(100, 6))

})

test_that("data_catboost() works", {

  rsplit <- rsplit_mc
  train <- train_mc
  test <- test_mc

  ## rsplit
  data <- data_catboost(rsplit, formula)
  expect_is(data, "list")
  expect_named(data, c("x", "y", "data", "eval"))
  expect_is(data$x, "matrix")
  expect_is(data$y, "integer")
  expect_is(data$data, "catboost.Pool")
  expect_is(data$eval, "catboost.Pool")
  expect_equal(dim(data$data), c(300, 6))
  expect_equal(dim(data$eval), c(100, 6))

  ## train (same as rsplit)
  data <- data_catboost(train, formula)
  expect_is(data, "list")
  expect_named(data, c("x", "y", "data"))
  expect_is(data$x, "matrix")
  expect_is(data$y, "integer")
  expect_is(data$data, "catboost.Pool")
  expect_equal(dim(data$data), c(300, 6))

  ## test
  data <- data_catboost(test, formula)
  expect_is(data, "list")
  expect_named(data, c("x", "y", "data"))
  expect_is(data$x, "matrix")
  expect_is(data$y, "integer")
  expect_is(data$data, "catboost.Pool")
  expect_equal(dim(data$data), c(100, 6))

})
