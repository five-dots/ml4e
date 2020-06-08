
## FastRGF_Classifier$new(
##   n_estimators = 500,
##   max_depth = 6,
##   max_leaf = 50,
##   tree_gain_ratio = 1.0,
##   min_samples_leaf = 5,
##   loss = "LS",
##   l1 = 1.0,
##   l2 = 1000.0,
##   opt_algorithm = "rgf",
##   learning_rate = 0.001,
##   max_bin = NULL,
##   min_child_weight = 5.0,
##   data_l2 = 2.0,
##   sparse_max_features = 80000,
##   sparse_min_occurences = 5,
##   calc_prob="sigmoid",
##   n_jobs = 1,
##   verbose = 0
## )

## FastRGF_Regressor$new(
##   n_estimators = 500,
##   max_depth = 6,
##   max_leaf = 50,
##   tree_gain_ratio = 1.0,
##   min_samples_leaf = 5,
##   l1 = 1.0,
##   l2 = 1000.0,
##   opt_algorithm = "rgf",
##   learning_rate = 0.001,
##   max_bin = NULL,
##   min_child_weight = 5.0,
##   data_l2 = 2.0,
##   sparse_max_features = 80000,
##   sparse_min_occurences = 5,
##   n_jobs = 1,
##   verbose = 0
## )

## Internal_class$fit(
##   x,
##   y,
##   sample_weight = NULL
## )

.RGF_fit_fixed <- list(
  x = quote(x),
  y = quote(y)
)

.RGF_fit_default <- list(
  ## TODO FastRGF support (predict fails in case of FastRGF)
  ## fast = quote(parallel)
)

## Internal_class$predict(x)
## Internal_class$predict_proba(x)

.RGF_pred_fixed <- list(
  object = quote(fitted),
  x = quote(x)
)

.assert_RGF <- function() {
  if (!rlang::is_installed("reticulate"))
    stop("RGF engine requres reticulate package.", call. = FALSE)

  py_avail_call <- rlang::call2("py_available", initialize = TRUE,
                               .ns = "reticulate")
  if (!rlang::eval_tidy(py_avail_call, env = rlang::current_env()))
    stop("Python is not available.", call. = FALSE)

  mod_avail_call <- rlang::call2("py_module_available", module = "rgf.sklearn",
                                 .ns = "reticulate")
  if (!rlang::eval_tidy(mod_avail_call, env = rlang::current_env()))
    stop("Python package \"rgf.sklearn\" is not available. ",
         "You can install by `pip install rgf-python`",
         call. = FALSE)

  invisible(NULL)
}

#' Fit rgf by RGF package
#'
#' @param x A matrix of features.
#' @param y A vector of labels.
#' @param fast A logical scalar wheather to use `RGF::FastRGF`.
#' @param ... Additional arguments passed to [RGF::RGF_Classifier] or
#' [RGF::RGF_Regressor].
#'
#' @return A fitted object.
#'
#' @export
fit_RGF <- function(x, y, fast = FALSE, ...) {
  .assert_RGF()

  ## Switch R6Generator class
  if (is.factor(y)) {
    r6gen_str <- if (fast) "RGF::FastRGF_Classifier" else "RGF::RGF_Classifier"
  } else {
    r6gen_str <- if (fast) "RGF::FastRGF_Regressor" else "RGF::RGF_Regressor"
  }
  r6gen_call <- rlang::parse_expr(r6gen_str)

  ## Build R6 generator
  r6gen <- rlang::eval_tidy(r6gen_call, env = rlang::current_env())
  args <- list(...)
  obj <- rlang::exec(r6gen$new, !!!args)
  obj$fit(x, y)
  obj
}

#' Predict rgf by RGF package
#'
#' @param object A fitted object.
#' @param x A matrix of features.
#'
#' @return Predicted values.
#'
#' @export
predict_RGF <- function(object, x) {
  if (inherits(object, "RGF_Classifier") ||
      inherits(object, "FastRGF_Classifier")) {
    object$predict_proba(x)
  } else if (inherits(object, "RGF_Regressor") ||
             inherits(object, "FastRGF_Regressor")) {
    object$predict(x)
  } else {
    stop("object is not supported RGF instance.", call. = FALSE)
  }
}
