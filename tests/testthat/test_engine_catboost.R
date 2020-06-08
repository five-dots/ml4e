
test_that("catboost_es for multiclass classification works", {

  keys_cv["model"] <- "catboost_es"
  cv <- test_model(options_mc, keys_cv, "catboost.Model", pred_mc_col_names)

})

test_that("catboost_es for binary classification works", {

  keys_cv["model"] <- "catboost_es"
  cv <- test_model(options_bin, keys_cv, "catboost.Model", pred_bin_col_names)

})

test_that("catboost_es regression works", {

  keys_cv["model"] <- "catboost_es"
  cv <- test_model(options_reg, keys_cv, "catboost.Model", pred_reg_col_names)

})

test_that("catboost_es poisson works", {

  keys_cv["model"] <- "catboost_es"
  cv <- test_model(options_pois, keys_cv, "catboost.Model", pred_reg_col_names)

})
