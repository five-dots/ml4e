
## S4 method for signature 'formula'
## ksvm(
##   x,
##   data = NULL,
##   ...,
##   subset,
##   na.action = na.omit,
##   scaled = TRUE
## )

## S4 method for signature 'matrix'
## ksvm(
##   x,
##   y = NULL,
##   scaled = TRUE,
##   type = NULL,
##   kernel ="rbfdot",
##   kpar = "automatic",
##   C = 1,
##   nu = 0.2,
##   epsilon = 0.1,
##   prob.model = FALSE,
##   class.weights = NULL,
##   cross = 0,
##   fit = TRUE,
##   cache = 40,
##   tol = 0.001,
##   shrinking = TRUE,
##   ...,
##   subset,
##   na.action = na.omit
## )
.kernlab_fit_fixed <- list(
  x = quote(formula),
  data = quote(data),
  prob_model = quote(!is.null(label_levels))
)

.kernlab_fit_default <- list()

# S4 method for ksvm
## predict(
##   object,
##   newdata,
##   type = "response",
##   coupler = "minpair"
## )
.kernlab_pred_fixed <- list(
  object = quote(fitted),
  newdata = quote(data)
)

#' FitParamSpecs Class Constructor for kernlab Engine
#'
#' @param kernel A character scalar of `kernel`.
#' @param type A character scalar of `type`.
#'
#' @return A FitParamSpecs class object.
#'
#' @export
new_fit_param_specs_kernlab <- function(kernel = NULL, type = NULL) {
  ## kernal parameters (kpar)
  sigma <- new_fit_param_spec("sigma", values = 0.0, default = 0.0,
                              trans = scales::log2_trans())
  degree <- new_fit_param_spec("degree", values = 1L, default = 1L)
  scale <- new_fit_param_spec("scale", values = 0.0, default = 0.0,
                              trans = scales::log10_trans())
  offset <- new_fit_param_spec("offset", values = 1L, default = 1L)
  order <- new_fit_param_spec("order", values = 1L, default = 1L)
  length <- new_fit_param_spec("length", values = 4L, default = 4L)
  lambda <- new_fit_param_spec("lambda", values = 1.1, default = 1.1)
  type_ <- new_fit_param_spec("type", values = "spectrum", default = "spectrum")
  normalized <- new_fit_param_spec("normalized", values = TRUE, default = TRUE)

  kernel_params <- switch(
    kernel,
    "rbfdot"     = list(sigma),
    "laplacedot" = list(sigma),
    "polydot"    = list(degree, scale, offset),
    "tanhdot"    = list(scale, offset),
    "basseldot"  = list(sigma, degree, order),
    "anovadot"   = list(sigma, degree),
    "stringdot"  = list(length, lambda, type_, normalized),
    "vanilladot" = NULL,
    "splinedot"  = NULL,
    stop(.str_quote(kernel), " is not supported kernel.", call. = FALSE)
  )

  ## Type parameter (C-svc or eps-svr)
  C <- new_fit_param_spec("C", values = 0.0, default = 0.0,
                          trans = scales::log10_trans())
  nu <- new_fit_param_spec("nu", values = 0.2, default = 0.2)
  epsilon <- new_fit_param_spec("epsilon", values = 0.1, default = 0.1)

  type_params <- switch(
    type,
    "C-svc"    = list(C),
    "C-bsvc"   = list(C),
    "spoc-svc" = list(C),
    "kbb-svc"  = list(C),
    "nu-svc"   = list(nu),
    "one-svc"  = list(nu),
    "eps-svr"  = list(C, epsilon),
    "nu-svr"   = list(nu, epsilon),
    "eps-bsvr" = NULL,
    stop(.str_quote(type), " is not supported type.", call. = FALSE)
  )

  params <- c(kernel_params, type_params)
  rlang::exec(new_fit_param_specs, !!!params)
}

#' FitParamSpecs Class Constructor for kernlab Engine with RBF Kernel
#'
#' @return A FitParamSpecs class object.
#'
#' @export
new_fit_param_specs_kernlab_rbf <- function() {
  ## kernal parameters (kpar)
  ## TODO Add kernal and type as fixed param

  sigma <- new_fit_param_spec("sigma", values = 0.0, default = 0.0,
                              trans = scales::log2_trans())

  ## Type parameter (C-svc or eps-svr)
  C <- new_fit_param_spec("C", values = 0.0, default = 0.0,
                          trans = scales::log10_trans())
  epsilon <- new_fit_param_spec("epsilon", values = 0.1, default = 0.1)

  new_fit_param_specs(sigma, C, epsilon)
}

#' FitParamSpecs Class Constructor for kernlab Engine with Poly Kernel
#'
#' @return A FitParamSpecs class object.
#'
#' @export
new_fit_param_specs_kernlab_poly <- function() {
  ## kernal parameters (kpar)
  ## TODO Add kernal and type as fixed param

  degree <- new_fit_param_spec("degree", values = 1L, default = 1L)
  scale <- new_fit_param_spec("scale", values = 0.0, default = 0.0,
                              trans = scales::log10_trans())
  offset <- new_fit_param_spec("offset", values = 1L, default = 1L)

  ## Type parameter (C-svc or eps-svr)
  C <- new_fit_param_spec("C", values = 0.0, default = 0.0,
                          trans = scales::log10_trans())
  epsilon <- new_fit_param_spec("epsilon", values = 0.1, default = 0.1)

  new_fit_param_specs(degree, scale, offset, C, epsilon)
}

#' Fit kernlab package with kernel parameters
#'
#' @param x A formula.
#' @param data A data.frame.
#' @param kernel A character scalar of `kernel`.
#' @param prob_model A logical scalar.
#' @param ... Additional arguments passed to [kernlab::ksvm].
#'
#' @return A fitted object.
#'
#' @export
fit_kernlab <- function(x, data, kernel = "rbfdot", prob_model = FALSE, ...) {
  dots <- list(...)

  ## Extract kpars from dots by kernel
  kpar_names <- switch(
    kernel,
    "rbfdot"     = "sigma",
    "polydot"    = c("degree", "scale", "offset"),
    "vanilladot" = character(),
    "tanhdot"    = c("scale", "offset"),
    "laplacedot" = "sigma",
    "basseldot"  = c("sigma", "order", "degree"),
    "anovadot"   = c("sigma", "degree"),
    "splinedot"  = character(),
    "stringdot"  = c("length", "lambda", "type", "normalized"),
    stop(.str_quote(kernel), " is not a supported kernel.", call. = FALSE)
  )

  kpar <- dots[names(dots) %in% kpar_names]
  if (length(kpar) == 0L) kpar <- "automatic" # default

  ## call by all args
  args <- dots[!names(dots) %in% c("x", "data", "kernel", "prob_model",
                                   kpar_names)]
  all_args <- c(list(x = quote(x), data = quote(data), prob.model = prob_model,
                     kernel = kernel, kpar = kpar), args)

  call <- rlang::call2("ksvm", !!!all_args, .ns = "kernlab")
  rlang::eval_tidy(call, env = rlang::current_env())
}
