
#' Identity data function
#'
#' Data function for models that consumes `data.frame` as is.
#'
#' @param object A `data.frame` or `rset` object.
#' @param ... Not currently used.
#'
#' @export
data_identity <- function(object, ...) {
  UseMethod("data_identity", object)
}

#' @rdname data_identity
#' @export
data_identity.data.frame <- function(object, ...) {
  list(data = object)
}

#' @rdname data_identity
#' @export
data_identity.rsplit <- function(object, ...) {
  data <- rsample::analysis(object)
  eval <- rsample::assessment(object)
  list(data = data, eval = eval)
}

#' XY data function
#'
#' Data function for models that consumes x (features) matrix and y (labels)
#' vector.
#'
#' @param object A `data.frame` or `rset` object.
#' @param formula A `formula` object.
#' @param ... Not currently used.
#'
#' @export
data_xy <- function(object, ...) {
  UseMethod("data_xy", object)
}

#' @rdname data_xy
#' @export
data_xy.data.frame <- function(object, formula, ...) {
  mf <- stats::model.frame(formula, object, na.action = stats::na.pass)
  x <- stats::model.matrix(formula, mf)
  y <- stats::model.response(mf)
  list(x = x, y = y)
}

#' @rdname data_xy
#' @export
data_xy.rsplit <- function(object, formula, ...) {
  data <- rsample::analysis(object)
  data <- data_xy(data, formula)
  eval <- rsample::assessment(object)
  eval <- data_xy(eval, formula)
  list(x = data$x, y = data$y, eval_x = eval$x, eval_y = eval$y)
}

#' Xgboost data function
#'
#' Data function for xgboost engine.
#'
#' @param object A `data.frame` or `rset` object.
#' @param formula A `formula` object.
#' @param ... Additinal arguments passed to the other S3 methods.
#'
#' @export
data_xgboost <- function(object, ...) {
  UseMethod("data_xgboost", object)
}

#' @rdname data_xgboost
#' @export
data_xgboost.data.frame <- function(object, formula, ...) {
  d <- .data_xgboost(object, formula)
  list(x = d$x, y = d$y, data = d$data)
}

#' @rdname data_xgboost
#' @export
data_xgboost.rsplit <- function(object, formula, ...) {
  train <- rsample::analysis(object)
  d <- .data_xgboost(train, formula)
  test <- rsample::assessment(object)
  eval <- .data_xgboost(test, formula)$data
  list(x = d$x, y = d$y, data = d$data, eval = eval)
}

.data_xgboost <- function(df, formula) {
  x <- .sparse_model_matrix(formula, df)
  y <- .labels_from_formula(formula, df)
  if (is.factor(y)) y <- as.integer(y) - 1L
  args <- list(data = quote(x), label = quote(y), missing = NA)
  call <- rlang::call2("xgb.DMatrix", !!!args, .ns = "xgboost")
  data <- rlang::eval_tidy(call, env = rlang::current_env())
  list(x = x, y = y, data = data)
}

#' Lightgbm data function
#'
#' Data function for lightgbm engine.
#'
#' @param object A `data.frame` or `rset` object.
#' @param formula A `formula` object.
#' @param ... Additinal arguments passed to the other S3 methods.
#'
#' @export
data_lightgbm <- function(object, ...) {
  UseMethod("data_lightgbm", object)
}

#' @rdname data_lightgbm
#' @export
data_lightgbm.data.frame <- function(object, formula, ...) {
  d <- .data_lightgbm(object, formula)
  list(x = d$x, y = d$y, data = d$data)
}

#' @rdname data_lightgbm
#' @export
data_lightgbm.rsplit <- function(object, formula, ...) {
  train <- rsample::analysis(object)
  d <- .data_lightgbm(train, formula)

  test <- rsample::assessment(object)
  eval <- .data_lightgbm(test, formula, d$data)$data
  list(x = d$x, y = d$y, data = d$data, eval = eval)
}

.data_lightgbm <- function(df, formula, dtrain = NULL) {
  ## Change NA action
  previous_na_action <- options()$na.action
  options(na.action = "na.pass")
  on.exit(options(na.action = previous_na_action))

  x <- .sparse_model_matrix(formula, df)
  y <- .labels_from_formula(formula, df)
  if (is.factor(y)) y <- as.integer(y) - 1L
  if (is.null(dtrain)) {
    ## train data
    args <- list(data = quote(x), label = quote(y))
    call <- rlang::call2("lgb.Dataset", !!!args, .ns = "lightgbm")
  } else {
    ## validation data
    args <- list(dataset = quote(dtrain), data = quote(x), label = quote(y))
    call <- rlang::call2("lgb.Dataset.create.valid", !!!args, .ns = "lightgbm")
  }
  data <- rlang::eval_tidy(call, env = rlang::current_env())
  list(x = x, y = y, data = data)
}

#' Catboost data function
#'
#' Data function for catboost engine.
#'
#' @param object A `data.frame` or `rset` object.
#' @param formula A `formula` object.
#' @param ... Additinal arguments passed to the other S3 methods.
#'
#' @export
data_catboost <- function(object, ...) {
  UseMethod("data_catboost", object)
}

#' @rdname data_catboost
#' @export
data_catboost.data.frame <- function(object, formula, ...) {
  d <- .data_catboost(object, formula)
  list(x = d$x, y = d$y, data = d$data)
}

#' @rdname data_catboost
#' @export
data_catboost.rsplit <- function(object, formula, ...) {
  train <- rsample::analysis(object)
  d <- .data_catboost(train, formula)

  test <- rsample::assessment(object)
  eval <- .data_catboost(test, formula)$data
  list(x = d$x, y = d$y, data = d$data, eval = eval)
}

.data_catboost <- function(df, formula) {
  mf <- stats::model.frame(formula, df, na.action = stats::na.pass)
  x <- stats::model.matrix(formula, mf)
  y <- stats::model.response(mf)
  if (is.factor(y)) y <- as.integer(y) - 1L

  args <- list(data = quote(x), label = quote(y))
  call <- rlang::call2("catboost.load_pool", !!!args, .ns = "catboost")
  data <- rlang::eval_tidy(call, env = rlang::current_env())
  list(x = x, y = y, data = data)
}

.sparse_model_matrix <- function(formula, df) {
  ## Change NA action
  previous_na_action <- options()$na.action
  options(na.action = "na.pass")
  on.exit(options(na.action = previous_na_action))

  args <- list(object = formula, data = df)
  call <- rlang::call2("sparse.model.matrix", !!!args, .ns = "Matrix")
  rlang::eval_tidy(call, env = rlang::current_env())
}
