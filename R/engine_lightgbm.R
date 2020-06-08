
## lgb.train(
##   params = list(),
##   data,
##   nrounds = 10L,
##   valids = list(),
##   obj = NULL,
##   eval = NULL,
##   verbose = 1L,
##   record = TRUE,
##   eval_freq = 1L,
##   init_model = NULL,
##   colnames = NULL,
##   categorical_feature = NULL,
##   early_stopping_rounds = NULL,
##   callbacks = list(),
##   reset_data = FALSE,
##   ...
## )

.lightgbm_es_fit_fixed <- list(
  data = quote(data),
  eval = quote(eval),
  task_type = quote(task_type)
)

.lightgbm_es_fit_default <- list()

## predict.lgb.Booster(
##   object,
##   data,
##   num_iteration = NULL,
##   rawscore = FALSE,
##   predleaf = FALSE,
##   predcontrib = FALSE,
##   header = FALSE,
##   reshape = FALSE,
##   ...
## )

.lightgbm_es_pred_fixed <- list(
  object = quote(fitted),
  data = quote(x),
  reshape = TRUE
)

#' FitParamSpecs Class Constructor for lightgbm Engine
#'
#' @return A FitParamSpecs class object.
#'
new_fit_param_specs_lightgbm_es <- function() {
  max_depth <- new_fit_param_spec(
    name = "max_depth",
    values = -1L ,
    default = -1L
  )
  min_sum_hessian_in_leaf <- new_fit_param_spec(
    name = "min_sum_hessian_in_leaf",
    values = 0.001,
    default = 0.001
  )
  min_gain_to_split <- new_fit_param_spec(
    name = "min_gain_to_split",
    values = -1.0,
    default = -1.0,
    trans = scales::log10_trans()
  )

  feature_fraction <- new_fit_param_spec(name = "feature_fraction",
                                         values = seq(0.6, 1.0, 0.1),
                                         default = 1.0)
  bagging_fraction <- new_fit_param_spec(name = "bagging_fraction",
                                         values = seq(0.6, 1.0, 0.1),
                                         default = 1.0)
  learning_rate <- new_fit_param_spec(name = "learning_rate",
                                      values = seq(0.01, 0.05, 0.01),
                                      default = 0.1)
  min_data_in_leaf <- new_fit_param_spec(name = "min_data_in_leaf",
                                         values = 1L:20L,
                                         default = 20L)
  min_data_in_bin <- new_fit_param_spec(name = "min_data_in_bin",
                                        values = 1L:3L,
                                        default = 5L)
  new_fit_param_specs(max_depth, min_sum_hessian_in_leaf, min_gain_to_split,
                      feature_fraction, bagging_fraction, learning_rate,
                      min_data_in_leaf, min_data_in_bin)
}

#' Fit GBDT by lightgbm package with early stopping
#'
#' @param data A training data of [lightgbm::lgb.Dataset] class object.
#' @param eval A evaluation data of [lightgbm::lgb.Dataset] class object. Used
#' for early stopping.
#' @inheritParams fit_xgboost_es
#' @param ... Additional arguments passed to [lightgbm::lgb.train]].
#'
#' @return A fitted object.
#'
#' @export
fit_lightgbm_es <- function(data, eval, task_type, ...) {

  ## set default objective/metric and num_class from task and labels
  ## objective and metric can be overridden
  num_class <- NULL
  switch(task_type,
    "regression" = {
      objective <- task_type
      metric <- "mse" # = "l2"
    },
    "poisson" = {
      objective <- task_type
      metric <- "poisson"
    },
    "binary" = {
      objective <- task_type
      metric = "binary_logloss"
    },
    "multiclass" = {
      objective <- task_type
      metric <- "multi_logloss"
      labels <- .labels_from_dmatrix(data)
      num_class <- length(unique(labels))
    },
    stop(task_type, " is not supported task type.", call. = FALSE)
  )

  ## default params
  learning_rate <- 0.1
  verbose <- -1L
  num_iterations <- 10000L
  valids <- quote(list(test = eval))
  early_stopping_rounds <- round(10.0 / learning_rate)

  ## call from default params
  args <- list(
    data = quote(data),
    objective = objective,
    metric = metric,
    num_iterations = num_iterations,
    valids = valids,
    early_stopping_rounds = early_stopping_rounds,
    verbose = verbose
  )
  if (!is.null(num_class)) args$num_class <- num_class
  call <- rlang::call2("lgb.train", !!!args, .ns = "lightgbm")

  ## args not overridden
  fixed_names <- c("data", "params", "num_class", "valids")

  ## override or add some other args
  dots <- list(...)
  dots <- dots[!(names(dots) %in% fixed_names)]

  if (length(dots) > 0L) {
    ## translate to (0, 1)
    if (rlang::has_name(dots, "bagging_fraction"))
      dots$bagging_fraction <- .as_ratio(dots$bagging_fraction, nrow(data))
    if (rlang::has_name(dots, "feature_fraction"))
      dots$feature_fraction <- .as_ratio(dots$feature_fraction, ncol(data))

    call <- rlang::call_modify(call, !!!dots)
  }
  rlang::eval_tidy(call, env = rlang::current_env())
}
