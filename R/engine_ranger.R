
## ranger(
##   formula = NULL,
##   data = NULL,
##   num.trees = 500,
##   mtry = NULL,
##   importance = "none",
##   write.forest = TRUE,
##   probability = FALSE,
##   min.node.size = NULL,
##   max.depth = NULL,
##   replace = TRUE,
##   sample.fraction = ifelse(replace, 1, 0.632),
##   case.weights = NULL,
##   class.weights = NULL,
##   splitrule = NULL,
##   num.random.splits = 1,
##   alpha = 0.5,
##   minprop = 0.1,
##   split.select.weights = NULL,
##   always.split.variables = NULL,
##   respect.unordered.factors = NULL,
##   scale.permutation.importance = FALSE,
##   local.importance = FALSE,
##   regularization.factor = 1,
##   regularization.usedepth = FALSE,
##   keep.inbag = FALSE,
##   inbag = NULL,
##   holdout = FALSE,
##   quantreg = FALSE,
##   oob.error = TRUE,
##   num.threads = NULL,
##   save.memory = FALSE,
##   verbose = TRUE,
##   seed = NULL,
##   dependent.variable.name = NULL,
##   status.variable.name = NULL,
##   classification = NULL,
##   x = NULL,
##   y = NULL
## )
.ranger_fit_fixed <- list(
  formula = quote(formula),
  data = quote(data),
  write.forest = TRUE,
  probability = quote(!is.null(label_levels)),
  verbose = quote(show_progress),
  seed = quote(model_seed)
)

.ranger_fit_default <- list(
  num.threads = quote(if (parallel) NULL else 1)
)

## S3 method for ranger
## predict(
##   object,
##   data = NULL,
##   predict.all = FALSE,
##   num.trees = object$num.trees,
##   type = "response",
##   se.method = "infjack",
##   quantiles = c(0.1, 0.5, 0.9),
##   what = NULL,
##   seed = NULL,
##   num.threads = NULL,
##   verbose = TRUE,
##   ...
## )
.ranger_pred_fixed <- list(
  object = quote(fitted),
  data = quote(data)
)

#' FitParamSpecs Class Constructor for ranger Engine
#'
#' @param task_type A character scalar of task type.
#' @param splitrule A character scalar of splitrule.
#'
#' @return A FitParamSpecs class object.
#'
#' @export
new_fit_param_specs_ranger <- function(task_type = NULL, splitrule = NULL) {
  .assert_task_type(task_type)

  splitrule <- switch(
    task_type,
    "multiclass" = , "binary" = {
      match.arg(splitrule, c("gini", "extratrees"))
    },
    "regression" = {
      match.arg(splitrule, c("variance", "extratrees", "maxstat"))
    },
    "survival" = {
      match.arg(splitrule, c("logrank", "extratrees", "C", "maxstat"))
    },
    stop(task_type, " is not a supported task_type for ranger engine.",
         call. = FALSE)
  )

  splitrule_spec <- new_fit_param_spec(
    name = "splitrule",
    values = splitrule,
    default = splitrule
  )
  num.trees <- new_fit_param_spec(
    name = "num.trees",
    values = 500L,
    default = 500L
  )
  mtry <- new_fit_param_spec(
    name = "mtry",
    values = 0.2,
    default = 0.2
  )
  min.node.size_default <- switch(
    task_type,
    "multiclass" = , "binary" = 10L, # default for probability
    "regression" = 5L,
    "survival" = 3L
  )
  min.node.size <- new_fit_param_spec(
    name = "min.node.size",
    values = min.node.size_default,
    default = min.node.size_default
  )

  ## return fit_param_specs
  switch(
    splitrule,
    "gini" = , "variance" = , "logrank" = , "C" = {
      new_fit_param_specs(splitrule_spec, num.trees, mtry, min.node.size)
    },
    "extratrees" = {
      num.random.splits <- new_fit_param_spec(
        name = "num.random.splits",
        values = c(1L, 5L, 10L, 20L, 40L),
        default = 1L
      )
      new_fit_param_specs(splitrule_spec, num.trees, mtry, min.node.size,
                          num.random.splits)
    },
    "maxstat" = {
      alpha <- new_fit_param_spec(
        name = "alpha",
        values = seq(0.0, 1.0, 0.1),
        default = 0.5
      )
      minprop <- new_fit_param_spec(
        name = "minprop",
        values = seq(0.0, 0.5, 0.1),
        default = 0.1
      )
      new_fit_param_specs(splitrule_spec, num.trees, mtry, min.node.size, alpha,
                          minprop)
    },
  )
}

#' Fit ranger package with fraction mtry
#'
#' @param formula A `formula`.
#' @param data A `data.frame`.
#' @param mtry A double scalar of `mtry` fraction.
#' @param ... Additional arguments passed to [ranger::ranger].
#'
#' @return A fitted object.
#'
#' @export
fit_ranger <- function(formula, data, mtry = 0.2, ...) {
  if (!(rlang::is_scalar_double(mtry) && !is.na(mtry) &&
          0.0 <= mtry && mtry <= 1.0))
    stop("mtry must be a double scalar between 0 and 1.", call. = FALSE)

  ## number of features * mtry
  terms <- terms(formula, data = data)
  vars <- length(attr(terms, "term.labels"))
  mtry <- as.integer(floor(vars * mtry))
  if (mtry < 1.0) mtry <- 1L

  ## build call
  dots <- list(...)
  args <- c(list(formula = quote(formula), data = quote(data), mtry = mtry),
            dots)
  call <- rlang::call2("ranger", !!!args, .ns = "ranger")
  rlang::eval_tidy(call, env = rlang::current_env())
}
