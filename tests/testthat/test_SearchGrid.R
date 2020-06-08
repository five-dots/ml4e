
test_that("SearchGrid for glmnet_cv instance works", {

  fps <- new_fit_param_specs_glmnet_cv()
  fps["alpha"]$values <- c(0.0, 0.5, 1.0)
  m <- new_model("glmnet_cv")
  m$fit_param_specs <- fps

  ms <- new_models(m)
  o <- new_options(formulas, datasets_mc, seeds, ms, measure_mc,
                   show_progress = FALSE, keep_data = FALSE,
                   parallel = FALSE)
  g <- new_search_grid(o, keys)
  g
  self <- g
  private <- g$.__enclos_env__$private

  expect_is(g$progress, "list")
  expect_equal(g$progress_str, "0/3 (ttl: 0)")
  expect_false(g$done)
  expect_is(g$do(), "SearchGrid")
  expect_equal(g$progress_str, "3/3 (ttl: 3)")
  expect_true(g$done)

  expect_is(g$get_rank(), "data.frame")
  expect_is(g$get_best_param(), "list")
  expect_is(g$get_count_by_param(), "data.frame")

  ## reset new fps
  fps["alpha"]$values <- seq(2.0, 3.0, 0.1)
  expect_null(g$result)
  expect_equal(g$progress_str, "0/11 (ttl: 3)")
  expect_error(g$get_rank())
  expect_error(g$get_best_param())
  expect_error(g$get_count_by_param())

  ## reset new fps
  fps["alpha"]$values <- seq(0.0, 0.5, 0.1)
  expect_equal(g$progress_str, "2/6 (ttl: 3)")
  expect_is(g$do(), "SearchGrid")
  expect_equal(g$progress_str, "6/6 (ttl: 7)")
  expect_true(g$done)

  ## add new fps
  fps["use_min"]$values <- c(0L, 1L)
  expect_equal(g$progress_str, "6/12 (ttl: 7)")
  expect_is(g$do(), "SearchGrid")
  expect_equal(g$progress_str, "12/12 (ttl: 13)")
  expect_true(g$done)

  ## remove fps
  fps$remove("use_min")
  expect_equal(g$progress_str, "6/6 (ttl: 13)")
  expect_true(g$done)

})

test_that("SearchGrid for kernlab_rbf instance works", {

  skip("Heavy test")

  fps <- new_fit_param_specs_kernlab_rbf()
  fps["sigma"]$values <- c(0.0, 1.0)
  fps["C"]$values <- c(0.0, 1.0)
  fps["epsilon"]$values <- c(0.1, 0.2)

  keys["model"] <- "kernlab_rbf"
  m <- new_model("kernlab_rbf", preproc_calls, NULL, fps)
  ms <- new_models(m)

  ## Multiclass
  o <- new_options(formulas, datasets_mc, seeds, ms, measure_mc,
                   show_progress = FALSE, keep_data = FALSE,
                   parallel = FALSE)
  g <- new_search_grid(o, keys)
  g$do()
  expect_is(g$result, "data.frame")

  ## Regression
  o <- new_options(formulas, datasets_reg, seeds, ms, measure_reg,
                   show_progress = FALSE, keep_data = FALSE,
                   parallel = FALSE)
  g <- new_search_grid(o, keys)
  g$do()
  expect_is(g$result, "data.frame")

})

test_that("SearchGrid for kknn instance works", {

  skip("Heavy test")

  fps <- new_fit_param_specs_kknn()
  fps["kmax"]$values <- c(11L, 15L)
  fps["distance"]$values <- c(1.0, 2.0)

  keys["model"] <- "kknn"
  m <- new_model("kknn", preproc_calls, NULL, fps)
  ms <- new_models(m)

  ## Multiclass
  o <- new_options(formulas, datasets_mc, seeds, ms, measure_mc,
                   show_progress = FALSE, keep_data = FALSE,
                   parallel = FALSE)
  g <- new_search_grid(o, keys)
  g$do()
  expect_is(g$result, "data.frame")

  ## Regression
  o <- new_options(formulas, datasets_reg, seeds, ms, measure_reg,
                   show_progress = FALSE, keep_data = FALSE,
                   parallel = FALSE)
  g <- new_search_grid(o, keys)
  g$do()
  expect_is(g$result, "data.frame")

})

test_that("SearchGrid for rpart instance works", {

  skip("Heavy test")

  fps <- new_fit_param_specs_rpart()
  fps["cp"]$values <- c(-1.0, -2.0, -3.0)
  fps["maxdepth"]$values <- c(10L, 20L, 30L)
  fps["minsplit"]$values <- c(2L, 4L, 7L)

  keys["model"] <- "rpart"
  m <- new_model("rpart", preproc_calls, NULL, fps)
  ms <- new_models(m)

  ## Multiclass
  o <- new_options(formulas, datasets_mc, seeds, ms, measure_mc,
                   show_progress = FALSE, keep_data = FALSE,
                   parallel = FALSE)
  g <- new_search_grid(o, keys)
  g$do()
  expect_is(g$result, "data.frame")

  ## Regression
  o <- new_options(formulas, datasets_reg, seeds, ms, measure_reg,
                   show_progress = FALSE, keep_data = FALSE,
                   parallel = FALSE)
  g <- new_search_grid(o, keys)
  g$do()
  expect_is(g$result, "data.frame")

})

test_that("SearchGrid for rangerinstance works", {

  skip("Heavy test")

  fps <- new_fit_param_specs_ranger("multiclass")
  fps["num.trees"]$values <- c(500L, 1000L)
  fps["mtry"]$values <- c(0.2, 0.3)

  keys["model"] <- "ranger"
  m <- new_model("ranger", preproc_calls, NULL, fps)
  ms <- new_models(m)

  ## Multiclass
  o <- new_options(formulas, datasets_mc, seeds, ms, measure_mc,
                   show_progress = FALSE, keep_data = FALSE,
                   parallel = FALSE)
  g <- new_search_grid(o, keys)
  g$do()
  expect_is(g$result, "data.frame")

  ## Regression
  fps <- new_fit_param_specs_ranger("regression")
  fps["num.trees"]$values <- c(500L, 1000L)
  fps["mtry"]$values <- c(0.2, 0.3)
  m$fit_param_specs <- fps
  o <- new_options(formulas, datasets_reg, seeds, ms, measure_reg,
                   show_progress = FALSE, keep_data = FALSE,
                   parallel = FALSE)
  g <- new_search_grid(o, keys)
  g$do()
  expect_is(g$result, "data.frame")

})
