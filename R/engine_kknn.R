
## train.kknn(
##   formula,
##   data,
##   kmax = 11,
##   ks = NULL,
##   distance = 2,
##   kernel = "optimal",
##   ykernel = NULL,
##   scale = TRUE,
##   contrasts = c('unordered' = "contr.dummy", ordered = "contr.ordinal"),
##   ...
## )
.kknn_fit_fixed <- list(
  formula = quote(formula),
  data = quote(data)
)

.kknn_fit_default <- list(
  kernel = c("rectangular", "triangular", "epanechnikov", "biweight",
             "triweight", "cos", "inv", "gaussian", "optimal")
)

## S3 method for class 'train.kknn'
## predict(
##   object,
##   newdata,
##   ...
## )
.kknn_pred_fixed <- list(
  object = quote(fitted),
  newdata = quote(data)
)

#' FitParamSpecs Class Constructor for kknn Engine
#'
#' @return A FitParamSpecs class object.
#'
#' @export
new_fit_param_specs_kknn <- function() {
  kmax <- new_fit_param_spec(name = "kmax", values = 11L, default = 11L)
  distance <- new_fit_param_spec(name = "distance", values = 2.0, default = 2.0)
  new_fit_param_specs(kmax, distance)
}
