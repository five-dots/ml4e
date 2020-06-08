
## Data vars and funcs ---------------------------------------------------------

n <- 400L
p <- 5L
data_seed <- 1983L

data_regression <- function(n = 400L, p = 5L, seed = 1983L) {
  set.seed(seed)
  x <- matrix(rnorm(n * p), n * p / p)
  coef <- rnorm(p)
  y <- x %*% coef
  mat <- cbind(y, x)
  colnames(mat) <- c("y", paste0("x", 1L:p))
  tibble::as_tibble(mat)
}

data_binary <- function(n = 400L, p = 5L, seed = 1983L) {
  set.seed(seed)
  x <- matrix(rnorm(n * p), n * p / p)
  coef <- rnorm(p)
  z <- x %*% coef
  prob <- 1.0 / (1.0 + exp(-z)) # logistic fun
  y <- rbinom(n, 1L, prob) # prob can be vector
  mat <- cbind(y, x)
  colnames(mat) <- c("y", paste0("x", 1L:p))
  dat <- tibble::as_tibble(mat)
  dplyr::mutate(dat, y = factor(y, labels = c("no", "yes")))
}

data_multiclass <- function(n = 400L, p = 5L, seed = 1983L) {
  set.seed(seed)
  x <- matrix(rnorm(n * p), n * p / p)
  coef1 <- rnorm(p)
  coef2 <- rnorm(p)
  coef3 <- rnorm(p)

  probs <- cbind(exp(x %*% coef1), exp(x %*% coef2), exp(x %*% coef3))
  choices <- t(apply(probs, 1L, rmultinom, n = 1L, size = 1L))
  y <- apply(choices, 1L, function(x) which(x == 1L))
  mat <- cbind(y, x)
  colnames(mat) <- c("y", paste0("x", 1L:p))
  dat <- tibble::as_tibble(mat)
  dplyr::mutate(dat, y = factor(y, labels = c("a", "b", "c")))
}

data_poisson <- function(n = 400L, p = 5L, seed = 1983L) {
  set.seed(seed)
  x <- matrix(rnorm(n * p), n * p / p)
  coef <- rnorm(p)
  lambda <- exp(x %*% coef)
  y <- rpois(n, lambda) # prob can be vector
  mat <- cbind(y, x)
  colnames(mat) <- c("y", paste0("x", 1L:p))
  dat <- tibble::as_tibble(mat)
  dplyr::mutate(dat, y = as.integer(y))
}


## Regression data -------------------------------------------------------------

data_reg <- data_regression(n, p, data_seed)
rsplit_reg <- rsample::initial_split(data_reg)
train_reg <- rsample::training(rsplit_reg)
test_reg <- rsample::testing(rsplit_reg)


## Binary data -----------------------------------------------------------------

data_bin <- data_binary(n, p, data_seed)
rsplit_bin <- rsample::initial_split(data_bin)
train_bin <- rsample::training(rsplit_bin)
test_bin <- rsample::testing(rsplit_bin)


## Multiclass data -------------------------------------------------------------

data_mc <- data_multiclass(n, p, data_seed)
rsplit_mc <- rsample::initial_split(data_mc)
train_mc <- rsample::training(rsplit_mc)
test_mc <- rsample::testing(rsplit_mc)


## Poisson data ----------------------------------------------------------------

data_pois <- data_poisson(n, p, data_seed)
rsplit_pois <- rsample::initial_split(data_pois)
train_pois <- rsample::training(rsplit_pois)
test_pois <- rsample::testing(rsplit_pois)


## Test data -------------------------------------------------------------------

search_seed <- 1983L
cv_seed <- 2014L
model_seed <- 2020L

id_col <- "id"
test_ids <- 301L:400L

formula <- y ~ .
bin_label_levels <- c("no", "yes")
mc_label_levels <- c("a", "b", "c")

pred_reg_col_names <- c("id", "y", ".pred")
pred_bin_col_names <- c("id", "y", ".pred", ".prob_yes")
pred_mc_col_names <- c("id", "y", ".pred", ".prob_a", ".prob_b", ".prob_c")


## Formulas --------------------------------------------------------------------
formulas <- new_formulas(all = y ~ .)


## ResampleCalls ---------------------------------------------------------------
resample_calls <- new_resample_calls(
  cv4 = quote(rsample::vfold_cv(data, v = 4L))
)

resample_calls_strata <- new_resample_calls(
  cv4 = quote(rsample::vfold_cv(data, v = 4L, strata = "y"))
)


## Datasets --------------------------------------------------------------------

datasets_mc <- new_datasets(
  base = tibble::rowid_to_column(data_mc, var = "id"),
  .id_col = id_col, .test_ids = test_ids,
  .resample_calls = resample_calls_strata
)

datasets_bin <- new_datasets(
  base = tibble::rowid_to_column(data_bin, var = "id"),
  .id_col = id_col, .test_ids = test_ids,
  .resample_calls = resample_calls_strata
)

datasets_reg <- new_datasets(
  base = tibble::rowid_to_column(data_reg, var = "id"),
  .id_col = id_col, .test_ids = test_ids,
  .resample_calls = resample_calls
)

datasets_pois <- new_datasets(
  base = tibble::rowid_to_column(data_pois, var = "id"),
  .id_col = id_col, .test_ids = test_ids,
  .resample_calls = resample_calls
)


## PreprocCalls ----------------------------------------------------------------
preproc_calls <- new_preproc_calls(quote(preproc_identity(data)))


## FitParams -------------------------------------------------------------------
fit_params <- new_fit_params(lasso_min = list(alpha = 0.0, use_min = 1L))


## Model -----------------------------------------------------------------------

model_lm          <- new_model("lm")
model_glm         <- new_model("glm")
model_glmnet_cv   <- new_model("glmnet_cv")
model_glmnet_err  <- new_model("glmnet_err")
model_kernlab     <- new_model("kernlab")
model_kknn        <- new_model("kknn")
model_rpart       <- new_model("rpart")
model_ranger      <- new_model("ranger")
model_extraTrees  <- new_model("extraTrees")
model_RGF         <- new_model("RGF")
model_xgboost_es  <- new_model("xgboost_es")
model_lightgbm_es <- new_model("lightgbm_es")
model_catboost_es <- new_model("catboost_es")
model_keras_es    <- new_model("keras_es")


## Models ----------------------------------------------------------------------

models <- new_models(
  model_glmnet_cv,
  model_xgboost_es
)

models_err <- new_models(
  model_glmnet_cv,
  model_glmnet_err
)

models_mc <- new_models(
  model_glmnet_cv,
  model_kernlab,
  model_kknn,
  model_rpart,
  model_ranger,
  model_extraTrees,
  model_RGF,
  model_xgboost_es,
  model_lightgbm_es,
  model_catboost_es,
  model_keras_es
)

models_bin <- new_models(
  model_glm,
  model_glmnet_cv,
  model_kernlab,
  model_kknn,
  model_rpart,
  model_ranger,
  model_extraTrees,
  model_RGF,
  model_xgboost_es,
  model_lightgbm_es,
  model_catboost_es,
  model_keras_es
)

models_reg <- new_models(
  model_lm,
  model_glm,
  model_glmnet_cv,
  model_kernlab,
  model_kknn,
  model_rpart,
  model_ranger,
  model_extraTrees,
  model_RGF,
  model_xgboost_es,
  model_lightgbm_es,
  model_catboost_es,
  model_keras_es
)

models_pois <- new_models(
  model_glm,
  model_glmnet_cv,
  model_rpart,
  model_xgboost_es,
  model_lightgbm_es,
  model_catboost_es,
  model_keras_es
)


## Seeds -----------------------------------------------------------------------
seeds <- new_seeds()


## Measure ---------------------------------------------------------------------

measure_mc   <- new_measure("multiclass")
measure_bin  <- new_measure("binary")
measure_reg  <- new_measure("regression")
measure_pois <- new_measure("poisson")


## Options ---------------------------------------------------------------------

options <- new_options(formulas, datasets_mc, seeds, models,
                       measure_mc, show_progress = FALSE,
                       keep_data = FALSE, parallel = FALSE)

options_err <- new_options(formulas, datasets_mc, seeds, models_err,
                           measure_mc, show_progress = FALSE,
                           keep_data = FALSE, parallel = FALSE)

options_mc <- new_options(formulas, datasets_mc, seeds, models_mc,
                          measure_mc, show_progress = FALSE,
                          keep_data = FALSE, parallel = FALSE)

options_bin <- new_options(formulas, datasets_bin, seeds, models_bin,
                           measure_bin, show_progress = FALSE,
                           keep_data = FALSE, parallel = FALSE)

options_reg <- new_options(formulas, datasets_reg, seeds, models_reg,
                           measure_reg, show_progress = FALSE,
                           keep_data = FALSE, parallel = FALSE)

options_pois <- new_options(formulas, datasets_pois, seeds, models_pois,
                            measure_pois, show_progress = FALSE,
                            keep_data = FALSE, parallel = FALSE)


## Project ---------------------------------------------------------------------

project <- new_project(formulas, datasets_mc, seeds, models,
                       measure_mc, show_progress = FALSE,
                       keep_data = FALSE, parallel = FALSE)

project_err <- new_project(formulas, datasets_mc, seeds, models_err,
                           measure_mc, show_progress = FALSE,
                           keep_data = FALSE, parallel = FALSE)

project_mc <- new_project(formulas, datasets_mc, seeds, models_mc,
                          measure_mc, show_progress = FALSE,
                          keep_data = FALSE, parallel = FALSE)

project_bin <- new_project(formulas, datasets_bin, seeds, models_bin,
                           measure_bin, show_progress = FALSE,
                           keep_data = FALSE, parallel = FALSE)

project_reg <- new_project(formulas, datasets_reg, seeds, models_reg,
                           measure_reg, show_progress = FALSE,
                           keep_data = FALSE, parallel = FALSE)

project_pois <- new_project(formulas, datasets_pois, seeds, models_pois,
                            measure_pois, show_progress = FALSE,
                            keep_data = FALSE, parallel = FALSE)


## Task  -----------------------------------------------------------------------

keys <- c(formula = "all", dataset = "base", resample = "cv4", seed = "sed_01",
          model = "glmnet_cv", preproc = "ppc_01")

task_mc <- Task$new(options_mc, keys)

keys_cv <- c(keys, fit_param = "default")
cv_mc   <- new_cv(options_mc, keys_cv)

grid_mc   <- new_search_grid(options_mc, keys)
random_mc <- new_search_random(options_mc, keys)
bayes_mc  <- new_search_bayes(options_mc, keys, metric_name = "acc")

## keys_err <- keys_cv
## keys_err["model"] <- "glmnet_err"

## cv_err <- new_cv(options_err, keys_err)
## bayes_err <- new_bayes_search(options_err, keys_err, metric_name = "acc")
