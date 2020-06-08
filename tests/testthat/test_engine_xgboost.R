
test_that("xgboost_es for multiclass classification works", {

  keys_cv["model"] <- "xgboost_es"
  cv <- test_model(options_mc, keys_cv, "xgb.Booster", pred_mc_col_names)

})

test_that("xgboost_es for binary classification works", {

  keys_cv["model"] <- "xgboost_es"
  cv <- test_model(options_bin, keys_cv, "xgb.Booster", pred_bin_col_names)

})

test_that("xgboost_es for regression works", {

  keys_cv["model"] <- "xgboost_es"
  cv <- test_model(options_reg, keys_cv, "xgb.Booster", pred_reg_col_names)

})

test_that("xgboost_es for poisson works", {

  keys_cv["model"] <- "xgboost_es"
  cv <- test_model(options_pois, keys_cv, "xgb.Booster", pred_reg_col_names)

})
