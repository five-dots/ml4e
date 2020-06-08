
test_that("RGF for multiclass classification works", {

  keys_cv["model"] <- "RGF"
  cv <- test_model(options_mc, keys_cv, "RGF_Classifier", pred_mc_col_names)

})

test_that("RGF for binary classification works", {

  keys_cv["model"] <- "RGF"
  cv <- test_model(options_bin, keys_cv, "RGF_Classifier", pred_bin_col_names)

})

test_that("RGF for regression works", {

  keys_cv["model"] <- "RGF"
  cv <- test_model(options_reg, keys_cv, "RGF_Regressor", pred_reg_col_names)

})
