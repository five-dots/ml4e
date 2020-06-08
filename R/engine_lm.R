
## lm(
##   formula,
##   data,
##   subset,
##   weights,
##   na.action,
##   method = "qr",
##   model = TRUE,
##   x = FALSE,
##   y = FALSE,
##   qr = TRUE,
##   singular.ok = TRUE,
##   contrasts = NULL,
##   offset,
##   ...
## )
.lm_fit_fixed <- list(
  formula = quote(formula),
  data = quote(data)
)

.lm_fit_default <- list()

## S3 method for class 'lm'
## predict(
##   object,
##   newdata,
##   se.fit = FALSE,
##   scale = NULL,
##   df = Inf,
##   interval = c("none", "confidence", "prediction"),
##   level = 0.95, type = c("response", "terms"),
##   terms = NULL,
##   na.action = na.pass,
##   pred.var = res.var/weights,
##   weights = 1,
##   ...
## )
.lm_pred_fixed <- list(
  object = quote(fitted),
  newdata = quote(data)
)
