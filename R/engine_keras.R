
## fit.keras.engine.training.Model(
##   object,
##   x = NULL,
##   y = NULL,
##   batch_size = NULL,
##   epochs = 10,
##   verbose = getOption("keras.fit_verbose", default = 1),
##   callbacks = NULL,
##   view_metrics = getOption("keras.view_metrics", default = "auto"),
##   validation_split = 0.0,
##   validation_data = NULL,
##   shuffle = TRUE,
##   class_weight = NULL,
##   sample_weight = NULL,
##   initial_epoch = 0,
##   steps_per_epoch = NULL,
##   validation_steps = NULL,
##   ...
## )

.keras_es_fit_fixed <- list(
  x = quote(x),
  y = quote(y),
  eval_x = quote(eval_x),
  eval_y = quote(eval_y),
  task_type = quote(task_type),
  seed = quote(model_seed)
)

.keras_es_fit_default <- list()

## predict.keras.engine.training.Model(
##   object,
##   x,
##   batch_size = NULL,
##   verbose = 0,
##   steps = NULL,
##   callbacks = NULL,
##   ...
## )

.keras_es_pred_fixed <- list(
  object = quote(fitted),
  x = quote(x)
)

.parse_optimizer_args <- function(...) {
  dots <- list(...)
  arg_names <- c(
    "lr",
    "momentum",
    "beta_1",
    "beta_2",
    "rho",
    "epsilon",
    "decay",
    "nesterov",
    "schedule_decay",
    "amsgrad",
    "clipnorm",
    "clipvalue"
  )
  dots[names(dots) %in% arg_names]
}

.parse_compile_args <- function(...) {
  dots <- list(...)
  arg_names <- c(
    ## "object",
    "optimizer",
    "loss",
    "metrics",
    "loss_weights",
    "sample_weight_mode",
    "weighted_metrics",
    "target_tensors"
  )
  dots[names(dots) %in% arg_names]
}

.parse_fit_args <- function(...) {
  dots <- list(...)
  arg_names <- c(
    ## "object",
    ## "x",
    ## "y",
    "batch_size",
    ## "epoch",
    "verbose",
    ## "callbacks",
    "view_metrics",
    ## "validation_split",
    ## "validation_data",
    "shuffle",
    "class_weight",
    "sample_weight",
    "initial_epoch",
    "steps_per_epoch",
    "validation_steps"
  )
  dots[names(dots) %in% arg_names]
}

.parse_callback_early_stopping_args <- function(...) {
  dots <- list(...)
  arg_names <- c(
    "monitor",
    "min_delta",
    ## "patience",
    "verbose",
    "mode",
    "baseline",
    "restore_best_weights"
  )
  dots[names(dots) %in% arg_names]
}

.set_compile_args <- function(task_type, ...) {

  compile_args <- .parse_compile_args(...)
  optimizer_args <- .parse_optimizer_args(...)

  ## optimizer
  if (!rlang::has_name(compile_args, "optimizer")) {
    ## default optimizer: "adam"
    optimizer_fun <- "optimizer_adam"
  } else {
    valid_optimizers <- c("adadelta", "adagrad", "adam", "adamax", "nadam",
                          "rmsprop", "sgd")
    if (!compile_args$optimizer %in% valid_optimizers)
      stop("optimizer must be one of ",
           paste(valid_optimizers, collapse = ", "),
           ".", call. = FALSE)
    optimizer_fun <- paste0("optimizer_", compile_args$optimizer)
  }
  optimizer_call <- rlang::call2(optimizer_fun, !!!optimizer_args,
                                 .ns = "keras")
  compile_args$optimizer <- optimizer_call

  ## loss
  if (!rlang::has_name(compile_args, "loss")) {
    compile_args$loss <- switch(
      task_type,
      "multiclass" = "categorical_crossentropy",
      "binary" = "binary_crossentropy",
      "regression" = "mean_squared_error",
      "poisson" = "poisson",
      stop(task_type, " is not a supported task_type for keras engine.",
           call. = FALSE)
    )
  }

  ## metrics
  if (!rlang::has_name(compile_args, "metrics")) {
    compile_args$metrics <- switch(
      task_type,
      "multiclass" = "accuracy",
      "binary" = "accuracy",
      "regression" = "mean_squared_error",
      "poisson" = "poisson",
      stop(task_type, " is not a supported task_type for keras engine.",
           call. = FALSE)
    )
  }
  compile_args
}

.as_y_matrix <- function(y) {
  if (is.factor(y)) {
    label_levels <- levels(y)
    y <- stats::model.matrix(~ y + 0L, data = data.frame(y = y))
    colnames(y) <- label_levels
    attributes(y)$assign <- NULL
    attributes(y)$contrasts <- NULL
    if (ncol(y) == 2L) y <- y[, 2L, drop = FALSE]
  } else {
    y <- matrix(y, ncol = 1L)
  }
  y
}

#' Fit neural network by keras package with early stopping
#'
#' @param x A matrix of features.
#' @param y A vector of labels.
#' @param eval_x A matrix of features for evaluation.
#' @param eval_y A vector of labels for evaluation.
#' @param task_type A character scalar of task_type.
#' @param keras_model A keras model object.
#' @param epoch A integer scalar of epoch.
#' @param patience A integer scalar of patience.
#' @param seed A integer scalar of random seed.
#' @param ... Additional arguments passed to [keras::fit].
#'
#' @return A fitted object.
#'
#' @export
fit_keras_es <- function(x, y, eval_x, eval_y, task_type, keras_model,
                         epoch = 10000L, patience = 20L, seed, ...) {

  .assert_class(keras_model, "keras.engine.training.Model")

  y <- .as_y_matrix(y)
  eval_y <- .as_y_matrix(eval_y)

  ## add output layer
  activation <- switch(
    task_type,
    "multiclass" = "softmax",
    "binary" = "sigmoid",
    "regression" = "linear",
    "poisson" = "linear"
  )
  layer_args <- list(
    object = quote(keras_model),
    units = ncol(y),
    activation = activation
  )
  layer_call <- rlang::call2("layer_dense", !!!layer_args, .ns = "keras")
  rlang::eval_tidy(layer_call, env = rlang::current_env())

  ## compile call (optimizer, loss, metrics)
  compile_args <- .set_compile_args(task_type, ...)
  compile_args$object <- quote(keras_model)

  compile_call <- rlang::call2("compile", !!!compile_args, .ns = "keras")
  keras_model <- rlang::eval_tidy(compile_call, env = rlang::current_env())

  ## fit call
  fit_args <- .parse_fit_args(...)
  fit_args$object <- quote(keras_model)
  fit_args$x <- quote(x)
  fit_args$y <- quote(y)
  fit_args$epoch <- epoch
  fit_args$validation_data <- quote(list(eval_x, eval_y))

  ## early_stopping callback
  es_args <- .parse_callback_early_stopping_args(...)
  es_args$patience <- patience
  es_call <- rlang::call2("callback_early_stopping", !!!es_args, .ns = "keras")
  es_fun <- rlang::eval_tidy(es_call, env = rlang::current_env())
  fit_args$callbacks <- list(es_fun)

  fit_call <- rlang::call2("fit", !!!fit_args, .ns = "keras")
  history <- rlang::eval_tidy(fit_call, env = rlang::current_env())
  keras_model
}
