
## glm(
##   formula,
##   family = gaussian,
##   data,
##   weights,
##   subset,
##   na.action,
##   start = NULL,
##   etastart,
##   mustart,
##   offset,
##   control = list(...),
##   model = TRUE,
##   method = "glm.fit",
##   x = FALSE,
##   y = TRUE,
##   singular.ok = TRUE,
##   contrasts = NULL,
##   ...
## )
.glm_fit_fixed <- list(
  formula = quote(formula),
  data = quote(data)
)

.glm_fit_default <- list(
  family = quote(family_from_task(task_type))
)

## S3 method for class 'glm'
## predict(
##   object,
##   newdata = NULL,
##   type = c("link", "response", "terms"),
##   se.fit = FALSE,
##   dispersion = NULL,
##   terms = NULL,
##   na.action = na.pass,
##   ...
## )
predict.glm
.glm_pred_fixed <- list(
  object = quote(fitted),
  newdata = quote(data)
)
