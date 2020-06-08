
test_that("ranger for multiclass classification works", {

  keys_cv["model"] <- "ranger"
  cv <- test_model(options_mc, keys_cv, "ranger", pred_mc_col_names)

})

test_that("ranger for binary classification works", {

  keys_cv["model"] <- "ranger"
  cv <- test_model(options_bin, keys_cv, "ranger", pred_bin_col_names)

})

test_that("ranger for regression works", {

  keys_cv["model"] <- "ranger"
  cv <- test_model(options_reg, keys_cv, "ranger", pred_reg_col_names)

})

## -----------------------------------------------------------------------------
test_that("fps fun for ranger works", {

  args <- list(
    multiclass = c("gini", "extratrees"),
    binary     = c("gini", "extratrees"),
    regression = c("variance", "extratrees", "maxstat"),
    survival   = c("logrank", "extratrees", "C", "maxstat")
  )

  purrr::iwalk(args, function(splitrules, task_type) {
    purrr::walk(splitrules, function(splitrule) {
      fps <- new_fit_param_specs_ranger(task_type, splitrule)

      if (splitrule %in% c("gini", "variance", "logrank", "C")) {
        expect_equal(fps$keys, c("splitrule", "num.trees", "mtry",
                                 "min.node.size"))

      } else if (splitrule == "extraTrees") {
        expect_equal(fps$keys, c("splitrule", "num.trees", "mtry",
                                 "min.node.size", "num.random.splits"))

      } else if (splitrule == "maxstat") {
        expect_equal(fps$keys, c("splitrule", "num.trees", "mtry",
                                 "min.node.size", "alpha", "minprop"))
      }
    })
  })

})
