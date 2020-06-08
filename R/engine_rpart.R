
## rpart(
##   formula,
##   data,
##   weights,
##   subset,
##   na.action = na.rpart,
##   method,
##   model = FALSE,
##   x = FALSE,
##   y = TRUE,
##   parms,
##   control,
##   cost,
##   ...
## )
.rpart_fit_fixed <- list(
  formula = quote(formula),
  data = quote(data)
)

.rpart_fit_default <- list()

## S3 method for class 'rpart'
## predict(
##   object,
##   newdata,
##   type = c("vector", "prob", "class", "matrix"),
##   na.action = na.pass,
##   ...
## )
.rpart_pred_fixed <- list(
  object = quote(fitted),
  newdata = quote(data)
)

#' FitParamSpecs Class Constructor for rpart Engine
#'
#' @return A FitParamSpecs class object.
#'
#' @export
new_fit_param_specs_rpart <- function() {
  cp <- new_fit_param_spec(name = "cp", values = -2.0, default = -2.0,
                           trans = scales::log10_trans())
  maxdepth <- new_fit_param_spec(name = "maxdepth", values = 30L, default = 30L)
  minsplit <- new_fit_param_spec(name = "minsplit", values = 20L, default = 20L)
  new_fit_param_specs(cp, maxdepth, minsplit)
}
