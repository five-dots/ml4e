
test_that("extraTrees for multiclass classification works", {

  keys_cv["model"] <- "extraTrees"
  cv <- test_model(options_mc, keys_cv, "extraTrees", pred_mc_col_names)

})

test_that("extraTrees for binary classification works", {

  keys_cv["model"] <- "extraTrees"
  cv <- test_model(options_bin, keys_cv, "extraTrees", pred_bin_col_names)

})

test_that("extraTrees for regression works", {

  keys_cv["model"] <- "extraTrees"
  cv <- test_model(options_reg, keys_cv, "extraTrees", pred_reg_col_names)

})

## -----------------------------------------------------------------------------
test_that("fps fun for extraTrees works", {

  task_types <- supported_tasks("extraTrees")

  purrr::walk(task_types, function(task_type) {
    fps <- new_fit_param_specs_extraTrees(task_type)
    expect_equal(fps$keys, c("ntree", "mtry", "nodesize", "numRandomCuts"))
  })

})
