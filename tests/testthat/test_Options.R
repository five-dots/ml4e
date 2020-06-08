
test_that("Options instance works", {

  o <- options_mc$clone(deep = TRUE)
  o
  self <- o
  private <- o$.__enclos_env__$private
  super <- o$.__enclos_env__$super

  ## fields
  expect_is(o, "Options")
  expect_is(o$formulas, "Formulas")
  expect_is(o$datasets, "Datasets")
  expect_is(o$models, "Models")
  expect_is(o$seeds, "Seeds")
  expect_is(o$measure, "Measure")

  expect_false(o$show_progress)
  expect_false(o$keep_data)
  expect_false(o$parallel)

  expect_equal(o$labels, data_mc$y)
  expect_equal(o$label_var, "y")
  expect_equal(o$label_levels, c("a", "b", "c"))
  expect_equal(o$task_type, "multiclass")

  ## private methods
  m <- models_mc$clone(deep = TRUE)
  m$add(new_model("lm"))
  expect_error(Options$new(formulas, datasets_mc, seeds, m),
               "multiclass is not a valid task_type for \"lm\" engine.")

  d <- datasets_mc$clone(deep = TRUE)
  d$add(tibble::rowid_to_column(data_mc, var = "id") %>% dplyr::rename(y2 = y))
  expect_error(Options$new(formulas, d, seeds, models_mc),
               "datasets \"dat_01\" does not have \"y\" labels.")

  d <- new_datasets(
    tibble::rowid_to_column(data_mc, var = "id"),
    .id_col = id_col, .test_ids = test_ids, .resample_calls = NULL
  )
  expect_error(Options$new(formulas, d, seeds, models_mc),
               "datasets must have a resample_calls.")

})
