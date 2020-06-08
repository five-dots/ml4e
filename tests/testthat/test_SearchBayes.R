
test_that("SearchBayes instance works", {

  skip("heavy test")

  fps <- new_fit_param_specs_glmnet_cv()
  fps["alpha"]$values <- seq(0.0, 1.0, 0.1)
  fps["use_min"]$values <- c(0L, 1L)

  m <- new_model("glmnet_cv", preproc_calls, NULL, fps)
  ms <- new_models(m)
  o <- new_options(formulas, datasets_mc, seeds, ms, measure_mc,
                   show_progress = FALSE, keep_data = FALSE,
                   parallel = FALSE)
  b <- new_search_bayes(o, keys)
  b
  self <- b
  private <- b$.__enclos_env__$private

  expect_is(b, "SearchBayes")
  expect_equal(b$metric_name, "acc")
  expect_true(b$is_new_grid)

  expect_false(b$done)
  expect_is(b$do(n_iter = 5L, init_points = 4L), "SearchBayes")
  expect_is(b$result, "data.frame")
  expect_false(b$is_new_grid)
  expect_true(b$done)
  expect_false(b$error)

  ## change fit_param_set
  fps["alpha"]$values <- seq(0.0, 0.5, 0.1)
  expect_true(b$is_new_grid)
  expect_false(b$done)

})

test_that("Bayes error instance works", {

  ## b <- bayes_err$clone(deep = TRUE)
  ## b
  ## self <- b
  ## private <- b$.__enclos_env__$private
  ## super <- b$.__enclos_env__$super

  ## expect_is(b, "Bayes")
  ## expect_equal(b$metric_name, "acc")

  ## expect_false(b$done)
  ## expect_is(b$do(n_iter = 2, init_points = 3), "Bayes")
  ## expect_is(b$result, "data.frame")
  ## expect_true(b$done)

  ## use init_grid_dt
  ## g <- grid_mc$clone(deep = TRUE)
  ## g$do()
  ## b2 <- bayes_mc$clone(deep = TRUE)
  ## expect_is(b2$do(n_iter = 2, init_points = 3, grid = g), "Bayes")
  ## expect_is(b2$result, "data.frame")

})
