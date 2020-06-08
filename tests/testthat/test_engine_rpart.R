
test_that("rpart for multiclass classification works", {

  keys_cv["model"] <- "rpart"
  cv <- test_model(options_mc, keys_cv, "rpart", pred_mc_col_names)

})

test_that("rpart for binary classification works", {

  keys_cv["model"] <- "rpart"
  cv <- test_model(options_bin, keys_cv, "rpart", pred_bin_col_names)

})

test_that("rpart for regression works", {

  keys_cv["model"] <- "rpart"
  cv <- test_model(options_reg, keys_cv, "rpart", pred_reg_col_names)

})

test_that("rpart for poisson works", {

  keys_cv["model"] <- "rpart"
  cv <- test_model(options_pois, keys_cv, "rpart", pred_reg_col_names)

})
