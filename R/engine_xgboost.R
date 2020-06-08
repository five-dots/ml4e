
## xgb.train(
##   params = list(),
##   data,
##   nrounds,
##   watchlist = list(),
##   obj = NULL,
##   feval = NULL,
##   verbose = 1,
##   print_every_n = 1L,
##   early_stopping_rounds = NULL,
##   maximize = NULL,
##   save_period = NULL,
##   save_name = "xgboost.model",
##   xgb_model = NULL,
##   callbacks = list(),
##   ...
## )

.xgboost_es_fit_fixed <- list(
  data = quote(data),
  eval = quote(eval),
  task_type = quote(task_type)
)

.xgboost_es_fit_default <- list()

## predict.xgb.Booster(
##   object,
##   newdata,
##   missing = NA,
##   outputmargin = FALSE,
##   ntreelimit = NULL,
##   predleaf = FALSE,
##   predcontrib = FALSE,
##   approxcontrib = FALSE,
##   predinteraction = FALSE,
##   reshape = FALSE,
##   training = FALSE,
##   ...
## )

.xgboost_es_pred_fixed <- list(
  object = quote(fitted),
  newdata = quote(data),
  reshape = TRUE
)

## .xgboost_fit_param_specs <- new_fit_param_specs(
##   new_fit_param_spec(name = "max_depth", values = seq(3L, 8L, 1L),
##                      default = 6L),
##   new_fit_param_spec(name = "min_child_weight", values = seq(1L, 5L, 1L),
##                      default = 1L),
##   new_fit_param_spec(name = "gamma", values = c(0.0), default = 0.0),
##   new_fit_param_spec(name = "colsample_bytree", values = seq(0.6, 1.0, 0.1),
##                      default = 1.0),
##   new_fit_param_spec(name = "subsample", values = seq(0.6, 1.0, 0.1),
##                      default = 1.0),
##   new_fit_param_spec(name = "eta", values = seq(0.01, 0.05, 0.01),
##                      default = 0.3)
## )

#' FitParamSpecs Class Constructor for xgboost Engine
#'
#' @return A FitParamSpecs class object.
#'
## new_fit_param_specs_xgboost <- function() {
##   max_depth <- new_fit_param_spec(name = "max_depth", values = seq(3L, 8L, 1L),
##                                   default = 6L)
##   min_child_weight <- new_fit_param_spec(name = "min_child_weight",
##                                          values = seq(1L, 5L, 1L),
##                                          default = 1L)
##   gamma <- new_fit_param_spec(name = "gamma", values = seq(0.0, 1.0, 0.1),
##                               default = 0.0)
##   colsample_bytree <- new_fit_param_spec(name = "colsample_bytree",
##                                          values = seq(0.6, 1.0, 0.1),
##                                          default = 1.0)
##   subsample <- new_fit_param_spec(name = "subsample",
##                                   values = seq(0.6, 1.0, 0.1),
##                                   default = 1.0)
##   eta <- new_fit_param_spec(name = "eta", values = seq(0.01, 0.05, 0.01),
##                             default = 0.3)
##   new_fit_param_specs(max_depth, min_child_weight, gamma, colsample_bytree,
##                       subsample, eta)
## }

#' Fit GBDT by xgboost package with early stopping
#'
#' @param data A training data of \code{\link[xgboost]{xgb.DMatrix}} class
#' object.
#' @param eval A evaluation data of \code{\link[xgboost]{xgb.DMatrix}} class
#' object. Used for early stopping.
#' @param task_type A character scalar of task_type.
#' @param ... Additional arguments passed to \code{\link[xgboost]{xgb.train}}.
#'
#' @return A fitted object.
#'
#' @export
fit_xgboost_es <- function(data, eval, task_type, ...) {

  ## set a default objective/eval_metric and num_class from the task and labels
  ## objective and eval_metric can be overridden by dots
  num_class <- NULL
  switch(task_type,
    "regression" = {
      objective <- "reg:squarederror"
      eval_metric <- "rmse"
    },
    "poisson" = {
      objective <- "count:poisson"
      eval_metric <- "poisson-nloglik"
    },
    "binary" = {
      objective <- "binary:logistic"
      eval_metric = "logloss"
    },
    "multiclass" = {
      objective <- "multi:softprob"
      eval_metric = "mlogloss"
      labels <- .labels_from_dmatrix(data)
      num_class <- length(unique(labels))
    },
    stop(task_type, " is not supported task type.", call. = FALSE)
  )

  ## default params
  eta <- 0.3
  verbose <- 0L
  nrounds <- 10000L
  watchlist <- quote(list(train = data, eval = eval))
  early_stopping_rounds <- round(10.0 / eta)

  ## call by default params
  args <- list(
    data = quote(data),
    objective = objective,
    eval_metric = eval_metric,
    nrounds = nrounds,
    watchlist = watchlist,
    early_stopping_rounds = early_stopping_rounds,
    verbose = verbose
  )
  if (!is.null(num_class)) args$num_class <- num_class
  call <- rlang::call2("xgb.train", !!!args, .ns = "xgboost")

  ## args not overridden
  fixed_names <- c("data", "params", "num_class", "watchlist")

  ## override or add some other args
  dots <- list(...)
  dots <- dots[!(names(dots) %in% fixed_names)]

  if (length(dots) > 0L) {
    ## translate to (0, 1)
    if (rlang::has_name(dots, "subsample"))
      dots$subsample <- .as_ratio(dots$subsample, nrow(data))
    if (rlang::has_name(dots, "colsample_bytree"))
      dots$colsample_bytree <- .as_ratio(dots$colsample_bytree, ncol(data))

    call <- rlang::call_modify(call, !!!dots)
  }
  rlang::eval_tidy(call, env = rlang::current_env())
}
