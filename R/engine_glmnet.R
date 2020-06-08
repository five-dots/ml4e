
## cv.glmnet(
##   x,
##   y,
##   weights = NULL,
##   offset = NULL,
##   lambda = NULL,
##   type.measure = c("default", "mse", "deviance", "class", "auc", "mae", "C"),
##   nfolds = 10,
##   foldid = NULL,
##   alignment = c("lambda", "fraction"),
##   grouped = TRUE,
##   keep = FALSE,
##   parallel = FALSE,
##   gamma = c(0, 0.25, 0.5, 0.75, 1),
##   relax = FALSE,
##   trace.it = 0,
##   ...
## )
.glmnet_cv_fit_fixed <- list(
  x = quote(x),
  y = quote(y)
)

.glmnet_cv_fit_default <- list(
  family = quote(family_from_task(task_type))
)

## S3 method for class 'glmnet'
## predict(
##   object,
##   newx,
##   s = NULL,
##   type = c("link", "response", "coefficients", "nonzero", "class"),
##   exact = FALSE,
##   newoffset,
##   ...
## )
.glmnet_cv_pred_fixed <- list(
  object = quote(fitted),
  newx = quote(x)
)

#' FitParamSpecs Class Constructor for glmnet_cv Engine
#'
#' @return A FitParamSpecs class object.
#'
#' @export
new_fit_param_specs_glmnet_cv <- function() {
  alpha <- new_fit_param_spec(name = "alpha", values = 1.0, default = 1.0)
  use_min <- new_fit_param_spec(name = "use_min", values = 1L, default = 1L)
  new_fit_param_specs(alpha, use_min)
}

#' Fit glmnet package with cross-validation
#'
#' @param x A matrix of features.
#' @param y A vector of labels.
#' @param family A character scalar of family.
#' @param use_min A logical scalar or binary integer wheather to use
#' `lambda.min`. If `FALSE`, `lambda.1se` is used.
#' @param ... Additional arguments passed to [glmnet::cv.glmnet] and
#' [glmnet::glmnet].
#'
#' @return A fitted object.
#'
#' @export
fit_glmnet_cv <- function(x, y, family, use_min = TRUE, ...) {
  ## convert if passed by 0/1
  use_min <- as.logical(use_min)
  dots <- list(...)
  args <- c(list(x = quote(x), y = quote(y), family = family), dots)
  cv_call <- rlang::call2("cv.glmnet", !!!args, .ns = "glmnet")
  cv <- rlang::eval_tidy(cv_call, env = rlang::current_env())

  ## Re-fit by glment::glment() with the lambda value
  args$lambda <- ifelse(use_min, cv$lambda.min, cv$lambda.1se)
  call <- rlang::call2("glmnet", !!!args, .ns = "glmnet")
  rlang::eval_tidy(call, env = rlang::current_env())
}

fit_glmnet_cv_err <- function(x, y, family, use_min = TRUE, ...) {
  if (as.integer(lubridate::second(Sys.time())) %% 2L == 0L)
    stop("error")
  ## stop("error")
  fit_glmnet_cv(x, y, family, use_min = TRUE, ...)
}
