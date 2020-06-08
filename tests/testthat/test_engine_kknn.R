
test_that("kknn for multiclass classification works", {

  keys_cv["model"] <- "kknn"
  cv <- test_model(options_mc, keys_cv, "train.kknn", pred_mc_col_names)

})

test_that("kknn for binary classification works", {

  keys_cv["model"] <- "kknn"
  cv <- test_model(options_bin, keys_cv, "train.kknn", pred_bin_col_names)

})

test_that("kknn for regression works", {

  keys_cv["model"] <- "kknn"
  cv <- test_model(options_reg, keys_cv, "train.kknn", pred_reg_col_names)

})
