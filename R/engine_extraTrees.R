
## extraTrees(
##   x,
##   y,
##   ntree=500,
##   mtry =
##     if (!is.null(y) && !is.factor(y))
##       max(floor(ncol(x)/3), 1)
##     else
##       floor(sqrt(ncol(x))),
##   nodesize = if (!is.null(y) && !is.factor(y)) 5 else 1,
##   numRandomCuts = 1,
##   evenCuts = FALSE,
##   numThreads = 1,
##   quantile = F,
##   weights = NULL,
##   subsetSizes = NULL,
##   subsetGroups = NULL,
##   tasks = NULL,
##   probOfTaskCuts = mtry / ncol(x),
##   numRandomTaskCuts = 1,
##   na.action = "stop",
##   ...
## )

.extraTrees_fit_fixed <- list(
  x = quote(x),
  y = quote(y)
)

.extraTrees_fit_default <- list()

## S3 method for class 'extraTrees'
## predict(
##   object,
##   newdata,
##   quantile = NULL,
##   allValues = F,
##   probability = F,
##   newtasks = NULL,
##   ...
## )
.extraTrees_pred_fixed <- list(
  object = quote(fitted),
  newdata = quote(x),
  probability = quote(!is.null(label_levels))
)

.extraTrees_fit_param_specs <- new_fit_param_specs(
  new_fit_param_spec("ntree", values = c(500L), default = 500L),
  new_fit_param_spec("mtry", values = seq(0.3, 0.6, 0.1), default = 0.3),
  new_fit_param_spec("nodesize", values = c(5L, 10L, 20L, 40L), default = 5L),
  new_fit_param_spec("numRandomCuts", values = 1L:5L, default = 1L)
)

#' FitParamSpecs Class Constructor for extraTrees Engine
#'
#' @param task_type A character scalar of task type.
#'
#' @return A FitParamSpecs class object.
#'
#' @export
new_fit_param_specs_extraTrees <- function(task_type = NULL) {
  .assert_task_type(task_type)

  ntree <- new_fit_param_spec(
    name = "ntree",
    values = 500L,
    default = 500L
  )
  mtry <- new_fit_param_spec(
    name = "mtry",
    values = 0.2,
    default = 0.2
  )
  nodesize_default <- switch(
    task_type,
    "multiclass" = , "binary" = 1L,
    "regression" = 5L,
    stop(task_type, " is not a supported task_type for extraTrees engine.",
         call. = FALSE)
  )
  nodesize <- new_fit_param_spec(
    name = "nodesize",
    values = nodesize_default,
    default = nodesize_default
  )
  numRandomCuts <- new_fit_param_spec(
    name = "numRandomCuts",
    values = 1L,
    default = 1L
  )

  new_fit_param_specs(ntree, mtry, nodesize, numRandomCuts)
}

#' Fit extraTrees package with fraction mtry
#'
#' @param x A matrix of features.
#' @param y A vector of labels.
#' @param mtry A double scalar of `mtry` fraction.
#' @param ... Additional arguments passed to [extraTrees::extraTrees].
#'
#' @return A fitted object.
#'
#' @export
fit_extraTrees <- function(x, y, mtry = 0.2, ...) {
  ## mtry
  if (!(rlang::is_scalar_double(mtry) && !is.na(mtry) &&
          0.0 <= mtry && mtry <= 1.0))
    stop("mtry must be a double scalar between 0 and 1.", call. = FALSE)
  mtry <- as.integer(floor(ncol(x) * mtry))
  if (mtry < 1.0) mtry <- 1L

  ## increase java heap size
  prev_opt <- options()$java.parameters
  options(java.parameters = "-Xmx2g")
  on.exit(options(java.parameters = prev_opt))

  ## build call
  dots <- list(...)
  args <- c(list(x = quote(x), y = quote(y), mtry = mtry), dots)
  call <- rlang::call2("extraTrees", !!!args, .ns = "extraTrees")
  rlang::eval_tidy(call, env = rlang::current_env())
}
