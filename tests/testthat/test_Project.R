
test_that("Project instance works", {

  p <- project$clone(deep = TRUE)
  p
  self <- p
  private <- p$.__enclos_env__$private
  super <- p$.__enclos_env__$super

  ## fields
  expect_is(p, "Project")
  expect_is(p$cv_table, "tbl_df")
  expect_is(p$grid_table, "tbl_df")
  expect_is(p$random_table, "tbl_df")
  expect_is(p$bayes_tables, "list")
  expect_is(p$bayes_tables[[1L]], "tbl_df")
  expect_is(p$bayes_tables[[2L]], "tbl_df")

  ## cv
  expect_null(p$get_scores())
  expect_is(p$run_cv(), "Project")
  expect_is(p$get_scores(), "data.frame")
  expect_is(p$get_preds(), "list")
  expect_is(p$get_stacking_data(), "data.frame")
  expect_is(p$get_stacking_data(prob = TRUE), "data.frame")

  ## grid
  fps <- new_fit_param_specs_glmnet_cv()
  m <- p$models["glmnet_cv"]
  m$fit_param_specs <- fps

  expect_is(p$grid_table, "data.frame")
  expect_is(p$run_grid(model == "glmnet_cv"), "Project")
  expect_is(p$get_search_result(), "data.frame")
  expect_is(p$get_ranks(), "list")
  expect_is(p$get_best_params(), "list")

  ## random
  expect_is(p$random_table, "data.frame")
  expect_is(p$run_random(model == "glmnet_cv", num = 5L), "Project")

  ## bayes
  expect_is(p$bayes_tables, "list")
  ## expect_is(p$run_bayes(1, n_iter = 1, init_points = 3), "Project")

})

test_that("Project error instance works", {

  ## p <- project_err$clone(deep = TRUE)
  ## p
  ## self <- p
  ## private <- p$.__enclos_env__$private
  ## super <- p$.__enclos_env__$super

  ## ## fields
  ## expect_is(p, "Project")

  ## ## cv
  ## expect_is(p$cv_table, "data.frame")
  ## expect_null(p$get_scores())
  ## expect_is(p$run_cv(), "Project")
  ## expect_is(p$get_scores(), "data.frame")
  ## expect_is(p$get_preds(), "list")

  ## ## grid
  ## expect_is(p$grid_table, "data.frame")
  ## expect_is(p$run_grid(), "Project")
  ## expect_is(p$get_ranks(), "list")
  ## expect_is(p$get_best_params(), "list")

  ## ## bayes
  ## expect_is(p$bayes_tables, "list")
  ## expect_is(p$run_bayes(1L, n_iter = 1L, init_points = 3L), "Project")

})
