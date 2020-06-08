- [ml4e: Machine Learning for Everyone](#sec-1)
  - [Installation](#sec-1-1)
  - [Quick Introduction](#sec-1-2)


# ml4e: Machine Learning for Everyone<a id="sec-1"></a>

Utilities make machine learning projects easier. Note this project is highly development state.

## Installation<a id="sec-1-1"></a>

`ml4e` is not available through CRAN. To install from github, use:

```R
devtools::install_github("five-dots/ml4e")
```

## Quick Introduction<a id="sec-1-2"></a>

```R
library(ml4e)

# Formulas defines label variable (reponse variable) and features
formulas <- new_formulas(Species ~ .)

# Seeds for hyper parameter optimizaion and cross validation
seeds <- new_seeds()$add_random()

# Resample method for cross validation. Currenlty supports functions from rsample package
resample_calls <- new_resample_calls(
  cv4 = quote(rsample::vfold_cv(data, v = 4)),
  cv4_strata = quote(rsample::vfold_cv(data, v = 4, strata = "Species"))
)

# Datasets must have a resample_calls (defined above) and can have test_ids to calculate scores.
datasets <- new_datasets(
  iris,
  .test_ids = sample(nrow(iris), 30),
  .resample_calls = resample_calls
)

# Models, in this case include 5 models
models <- new_models(
  new_model("glmnet_cv"),
  new_model("kknn"),
  new_model("kernlab"),
  new_model("ranger"),
  new_model("xgboost_es")
)

# Gather all options into one project
project <- new_project(formulas, datasets, seeds, models,
                       show_progress = FALSE)

# Run cross validations of all options combinations
project$run_cv()

# Confirm CV and test set scores of all combinations
project$get_scores() %>% dplyr::mutate_if(is.numeric, round, 4)
```

| resample             | seed             | model                | cv<sub>acc</sub> | cv<sub>logloss</sub> | test<sub>acc</sub> | test<sub>logloss</sub> |
|-------------------- |---------------- |-------------------- |---------------- |-------------------- |------------------ |---------------------- |
| cv4                  | sed<sub>01</sub> | glmnet<sub>cv</sub>  | 0.975            | 0.1009               | 0.9667             | 0.1029                 |
| cv4                  | sed<sub>01</sub> | kernlab              | 0.9667           | 0.1384               | 0.9333             | 0.1613                 |
| cv4                  | sed<sub>01</sub> | kknn                 | 0.95             | 0.1116               | 0.9333             | 0.1451                 |
| cv4                  | sed<sub>01</sub> | ranger               | 0.9583           | 0.1424               | 0.9333             | 0.1485                 |
| cv4                  | sed<sub>01</sub> | xgboost<sub>es</sub> | 0.9583           | 0.1563               | 0.9333             | 0.1599                 |
| cv4                  | sed<sub>02</sub> | glmnet<sub>cv</sub>  | 0.9417           | 0.1174               | 0.9333             | 0.1101                 |
| cv4                  | sed<sub>02</sub> | kernlab              | 0.95             | 0.1346               | 0.9333             | 0.1479                 |
| cv4                  | sed<sub>02</sub> | kknn                 | 0.9417           | 0.1145               | 0.9333             | 0.1438                 |
| cv4                  | sed<sub>02</sub> | ranger               | 0.9417           | 0.1777               | 0.9333             | 0.1407                 |
| cv4                  | sed<sub>02</sub> | xgboost<sub>es</sub> | 0.9417           | 0.2066               | 0.9333             | 0.1525                 |
| cv4<sub>strata</sub> | sed<sub>01</sub> | glmnet<sub>cv</sub>  | 0.9667           | 0.0829               | 0.9667             | 0.0987                 |
| cv4<sub>strata</sub> | sed<sub>01</sub> | kernlab              | 0.9417           | 0.1603               | 0.9333             | 0.1697                 |
| cv4<sub>strata</sub> | sed<sub>01</sub> | kknn                 | 0.9417           | 0.1084               | 0.9333             | 0.156                  |
| cv4<sub>strata</sub> | sed<sub>01</sub> | ranger               | 0.9583           | 0.1557               | 0.9333             | 0.1452                 |
| cv4<sub>strata</sub> | sed<sub>01</sub> | xgboost<sub>es</sub> | 0.95             | 0.1613               | 0.9333             | 0.1457                 |
| cv4<sub>strata</sub> | sed<sub>02</sub> | glmnet<sub>cv</sub>  | 0.9583           | 0.0805               | 0.9667             | 0.105                  |
| cv4<sub>strata</sub> | sed<sub>02</sub> | kernlab              | 0.9583           | 0.1341               | 0.9333             | 0.1387                 |
| cv4<sub>strata</sub> | sed<sub>02</sub> | kknn                 | 0.9583           | 0.679                | 0.9333             | 0.1869                 |
| cv4<sub>strata</sub> | sed<sub>02</sub> | ranger               | 0.95             | 0.1614               | 0.9333             | 0.1426                 |
| cv4<sub>strata</sub> | sed<sub>02</sub> | xgboost<sub>es</sub> | 0.9583           | 0.1823               | 0.9333             | 0.1261                 |
