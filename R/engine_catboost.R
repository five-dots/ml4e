
## catboost.train(
##   learn_pool,
##   test_pool = NULL,
##   params = list()
## )

.catboost_es_fit_fixed <- list(
  data = quote(data),
  eval = quote(eval),
  task_type = quote(task_type)
)

.catboost_es_fit_default <- list(
  allow_writing_files = FALSE
)

## catboost.predict(
##   model,
##   pool,
##   verbose = FALSE,
##   prediction_type = "RawFormulaVal",
##   ntree_start = 0,
##   ntree_end = 0,
##   thread_count = -1
## )

.catboost_es_pred_fixed <- list(
  model = quote(fitted),
  pool = quote(data)
)

#' Fit GBDT by catboost package with early stopping
#'
#' @param data A training data of `catboost.Pool` class object.
#' @param eval A evaluation data of `catboost.Pool` class object.
#' Used for early stopping.
#' @inheritParams fit_xgboost_es
#' @param ... Additional arguments passed to [catboost::catboost.train].
#'
#' @return A fitted object.
#'
#' @export
fit_catboost_es <- function(data, eval, task_type, ...) {

  ## https://catboost.ai/docs/concepts/loss-functions.html
  switch(task_type,
    "regression" = {
      loss_function <- "RMSE"
      eval_metric <- loss_function
    },
    "poisson" = {
      loss_function <- "Poisson"
      eval_metric <- loss_function
    },
    "binary" = {
      loss_function <- "Logloss"
      eval_metric <- loss_function
    },
    "multiclass" = {
      loss_function <- "MultiClass"
      eval_metric <- loss_function
    }
  )

  ## default params
  learning_rate <- 0.1 # default 0.03
  iterations <- 10000L
  od_type <- "Iter" # "IncToDec" or "Iter"
  od_wait <- round(10.0 / learning_rate)
  logging_level <- "Silent" # "Silent", "Verbose", "Info" or "Debug"

  default_params <- list(
    loss_function = loss_function,
    eval_metric = eval_metric,
    learning_rate = learning_rate,
    iterations = iterations,
    od_type = od_type,
    od_wait = od_wait,
    logging_level = logging_level
  )

  ## user arguments
  dots <- list(...)
  if (length(dots) > 0L) {
    ## translate to (0, 1)
    if (rlang::has_name(dots, "subsample"))
      dots$subsample <- .as_ratio(dots$subsample, nrow(data))
    if (rlang::has_name(dots, "rsm"))
      dots$rsm <- .as_ratio(dots$rsm, ncol(data))

    ## override default params
    default_params <- default_params[!names(default_params) %in% names(dots)]
  }

  ## combine all params
  params <- c(dots, default_params)

  ## call from default params
  args <- list(
    learn_pool = quote(data),
    test_pool = quote(eval),
    params = params
  )
  call <- rlang::call2("catboost.train", !!!args, .ns = "catboost")
  rlang::eval_tidy(call, env = rlang::current_env())
}
