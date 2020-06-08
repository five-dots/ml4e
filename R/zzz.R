
model_specs <- tibble::tribble(
  ~engine,       ~tasks,
  "lm",          c("regression"),
  "glm",         c("binary", "regression", "poisson"),
  "glmnet_cv",   c("binary", "multiclass", "regression", "poisson", "survival"),
  "glmnet_err",  c("binary", "multiclass", "regression", "poisson", "survival"),
  "kernlab",     c("binary", "multiclass", "regression"),
  "kknn",        c("binary", "multiclass", "regression"),
  "rpart",       c("binary", "multiclass", "regression", "poisson", "survival"),
  "ranger",      c("binary", "multiclass", "regression", "survival"),
  "extraTrees",  c("binary", "multiclass", "regression"),
  "RGF",         c("binary", "multiclass", "regression"),
  "xgboost_es",  c("binary", "multiclass", "regression", "poisson", "survival"),
  "lightgbm_es", c("binary", "multiclass", "regression", "poisson"),
  "catboost_es", c("binary", "multiclass", "regression", "poisson"),
  "keras_es",    c("binary", "multiclass", "regression", "poisson"),
)

## Add data_fun
model_specs <- dplyr::left_join(
  model_specs,
  tibble::tribble(
    ~engine,       ~data_fun,
    "lm",          "data_identity",
    "glm",         "data_identity",
    "glmnet_cv",   "data_xy", # TODO sparse matrix (except for cox model)
    "glmnet_err",  "data_xy", # TODO sparse matrix (except for cox model)
    "kernlab",     "data_identity",
    "kknn",        "data_identity",
    "rpart",       "data_identity",
    "ranger",      "data_identity",
    "extraTrees",  "data_xy",
    "RGF",         "data_xy",
    "xgboost_es",  "data_xgboost",
    "lightgbm_es", "data_lightgbm",
    "catboost_es", "data_catboost",
    "keras_es",    "data_xy"
  ), by = "engine"
)

## Add fit_fun
model_specs <- dplyr::left_join(
  model_specs,
  tibble::tribble(
    ~engine,       ~fit_ns, ~fit_fun,
    "lm",          "stats", "lm",
    "glm",         "stats", "glm",
    "glmnet_cv",   "ml4e",  "fit_glmnet_cv",
    "glmnet_err",  "ml4e",  "fit_glmnet_cv_err",
    "kernlab",     "ml4e",  "fit_kernlab",
    "kknn",        "kknn",  "train.kknn",
    "rpart",       "rpart", "rpart",
    "ranger",      "ml4e",  "fit_ranger",
    "extraTrees",  "ml4e",  "fit_extraTrees",
    "RGF",         "ml4e",  "fit_RGF",
    "xgboost_es",  "ml4e",  "fit_xgboost_es",
    "lightgbm_es", "ml4e",  "fit_lightgbm_es",
    "catboost_es", "ml4e",  "fit_catboost_es",
    "keras_es",    "ml4e",  "fit_keras_es"
  ), by = "engine"
)

## Add fit args
model_specs <- dplyr::left_join(
  model_specs,
  tibble::tribble(
    ~engine,       ~fit_fixed,             ~fit_default,
    "lm",          .lm_fit_fixed,          .lm_fit_default,
    "glm",         .glm_fit_fixed,         .glm_fit_default,
    "glmnet_cv",   .glmnet_cv_fit_fixed,   .glmnet_cv_fit_default,
    "glmnet_err",  .glmnet_cv_fit_fixed,   .glmnet_cv_fit_default,
    "kernlab",     .kernlab_fit_fixed,     .kernlab_fit_default,
    "kknn",        .kknn_fit_fixed,        .kknn_fit_default,
    "rpart",       .rpart_fit_fixed,       .rpart_fit_default,
    "ranger",      .ranger_fit_fixed,      .ranger_fit_default,
    "extraTrees",  .extraTrees_fit_fixed,  .extraTrees_fit_default,
    "RGF",         .RGF_fit_fixed,         .RGF_fit_default,
    "xgboost_es",  .xgboost_es_fit_fixed,  .xgboost_es_fit_default,
    "lightgbm_es", .lightgbm_es_fit_fixed, .lightgbm_es_fit_default,
    "catboost_es", .catboost_es_fit_fixed, .catboost_es_fit_default,
    "keras_es",    .keras_es_fit_fixed,    .keras_es_fit_default
  ), by = "engine"
)

## Add pred_fun
model_specs <- dplyr::left_join(
  model_specs,
  tibble::tribble(
    ~engine,       ~pred_ns,    ~pred_fun,          ~pred_fixed,
    "lm",          "stats",     "predict",          .lm_pred_fixed,
    "glm",         "stats",     "predict",          .glm_pred_fixed,
    "glmnet_cv",   "stats",     "predict",          .glmnet_cv_pred_fixed,
    "glmnet_err",  "stats",     "predict",          .glmnet_cv_pred_fixed,
    "kernlab",     "kernlab",   "predict",          .kernlab_pred_fixed,
    "kknn",        "stats",     "predict",          .kknn_pred_fixed,
    "rpart",       "stats",     "predict",          .rpart_pred_fixed,
    "ranger",      "stats",     "predict",          .ranger_pred_fixed,
    "extraTrees",  "stats",     "predict",          .extraTrees_pred_fixed,
    "RGF",         "ml4e",      "predict_RGF",      .RGF_pred_fixed,
    "xgboost_es",  "stats",     "predict",          .xgboost_es_pred_fixed,
    "lightgbm_es", "stats",     "predict",          .lightgbm_es_pred_fixed,
    "catboost_es", "catboost",  "catboost.predict", .catboost_es_pred_fixed,
    "keras_es",    "stats",     "predict",          .keras_es_pred_fixed
  ), by = "engine"
)

model_specs <- dplyr::left_join(
  model_specs,
  tibble::tribble(
    ~engine,       ~pred_type,
    "lm",          c(regression = "response"),
    "glm",         c(binary = "response", regression = "response",
                     poisson = "response"),
    "glmnet_cv",   c(binary = "response", multiclass = "response",
                     regression = "response", poisson = "response"),
    "glmnet_err",  c(binary = "response", multiclass = "response",
                     regression = "response", poisson = "response"),
    "kernlab",     c(binary = "probabilities", multiclass = "probabilities",
                     regression = "response"),
    "kknn",        c(binary = "prob", multiclass = "prob", regression = "raw"),
    "rpart",       c(binary = "prob", multiclass = "prob",
                     regression = "vector", poisson = "vector"),
    "ranger",      c(binary = "response", multiclass = "response",
                     regression = "response"),
    "extraTrees",  c(binary = NA_character_, multiclass = NA_character_,
                     regression = NA_character_),
    "RGF",         c(binary = NA_character_, multiclass = NA_character_,
                     regression = NA_character_),
    "xgboost_es",  c(binary = "response", multiclass = "response",
                     regression = "response", poisson = "response"),
    "lightgbm_es", c(binary = NA_character_, multiclass = NA_character_,
                     regression = NA_character_, poisson = NA_character_),
    "catboost_es", c(binary = "Probability", multiclass = "Probability",
                     regression = "RawFormulaVal", poisson = "RawFormulaVal"),
    "keras_es",    c(binary = NA_character_, multiclass = NA_character_,
                     regression = NA_character_, poisson = NA_character_),
  ), by = "engine"
)
