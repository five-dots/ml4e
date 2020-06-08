
test_that("kernlab_rbf for multiclass classification works", {

  keys_cv["model"] <- "kernlab"
  cv <- test_model(options_mc, keys_cv, "ksvm", pred_mc_col_names)

})

test_that("kernlab_rbf for binary classification works", {

  keys_cv["model"] <- "kernlab"
  cv <- test_model(options_bin, keys_cv, "ksvm", pred_bin_col_names)

})

test_that("kernlab_rbf for regression works", {

  keys_cv["model"] <- "kernlab"
  cv <- test_model(options_reg, keys_cv, "ksvm", pred_reg_col_names)

})

## -----------------------------------------------------------------------------
test_that("fps fun for kernlab works", {

  ## kernle
  fps <- new_fit_param_specs_kernlab("rbfdot", "C-svc")
  expect_equal(fps$keys, c("sigma", "C"))

  fps <- new_fit_param_specs_kernlab("laplacedot", "C-svc")
  expect_equal(fps$keys, c("sigma", "C"))

  fps <- new_fit_param_specs_kernlab("polydot", "C-svc")
  expect_equal(fps$keys, c("degree", "scale", "offset", "C"))

  fps <- new_fit_param_specs_kernlab("tanhdot", "C-svc")
  expect_equal(fps$keys, c("scale", "offset", "C"))

  fps <- new_fit_param_specs_kernlab("basseldot", "C-svc")
  expect_equal(fps$keys, c("sigma", "degree", "order", "C"))

  fps <- new_fit_param_specs_kernlab("anovadot", "C-svc")
  expect_equal(fps$keys, c("sigma", "degree", "C"))

  fps <- new_fit_param_specs_kernlab("stringdot", "C-svc")
  expect_equal(fps$keys, c("length", "lambda", "type", "normalized", "C"))

  fps <- new_fit_param_specs_kernlab("vanilladot", "C-svc")
  expect_equal(fps$keys, c("C"))

  fps <- new_fit_param_specs_kernlab("splinedot", "C-svc")
  expect_equal(fps$keys, c("C"))

  ## type
  fps <- new_fit_param_specs_kernlab("rbfdot", "C-bsvc")
  expect_equal(fps$keys, c("sigma", "C"))

  fps <- new_fit_param_specs_kernlab("rbfdot", "spoc-svc")
  expect_equal(fps$keys, c("sigma", "C"))

  fps <- new_fit_param_specs_kernlab("rbfdot", "kbb-svc")
  expect_equal(fps$keys, c("sigma", "C"))

  fps <- new_fit_param_specs_kernlab("rbfdot", "nu-svc")
  expect_equal(fps$keys, c("sigma", "nu"))

  fps <- new_fit_param_specs_kernlab("rbfdot", "one-svc")
  expect_equal(fps$keys, c("sigma", "nu"))

  fps <- new_fit_param_specs_kernlab("rbfdot", "eps-svr")
  expect_equal(fps$keys, c("sigma", "C", "epsilon"))

  fps <- new_fit_param_specs_kernlab("rbfdot", "nu-svr")
  expect_equal(fps$keys, c("sigma", "nu", "epsilon"))

  fps <- new_fit_param_specs_kernlab("rbfdot", "eps-svr")
  expect_equal(fps$keys, c("sigma", "C", "epsilon"))

})
