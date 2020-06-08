
test_that("SearchRandom instance works", {

  fps <- new_fit_param_specs_glmnet_cv()
  fps["alpha"]$values <- seq(0.0, 1.0, 0.1)

  m <- new_model("glmnet_cv", preproc_calls, NULL, fps)
  ms <- new_models(m)
  o <- new_options(formulas, datasets_mc, seeds, ms, measure_mc,
                   show_progress = FALSE, keep_data = FALSE,
                   parallel = FALSE)
  r <- new_search_random(o, keys)
  r
  self <- r
  private <- r$.__enclos_env__$private

  expect_is(r$progress, "list")
  expect_equal(r$progress_str, "0 (ttl: 0)")
  expect_false(r$done)
  expect_is(r$do(5L), "SearchRandom")
  expect_true(r$done)
  expect_equal(r$progress_str, "5 (ttl: 5)")
  expect_equal(dim(r$result), c(5L, 4L))

  expect_is(r$get_rank(), "data.frame")
  expect_is(r$get_best_param(), "list")

  ## reset new fps
  fps["alpha"]$values <- seq(0.0, 0.5, 0.1)
  expect_lte(r$progress$done, 5L)
  expect_equal(r$progress$total, 5L)
  expect_is(r$do(5L), "SearchRandom")
  expect_lte(r$progress$done, 10L)
  expect_equal(r$progress$total, 10L)

  ## revert fps
  fps["alpha"]$values <- seq(0.0, 1.0, 0.1)
  expect_equal(r$progress$done, 10L)

  ## add new fps
  fps["use_min"]$values <- c(0L, 1L)
  expect_equal(dim(r$result), c(10L, 4L))

  ## remove fps
  fps$remove("alpha")
  expect_null(r$result)

})
