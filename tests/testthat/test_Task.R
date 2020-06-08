
test_that("Task instance works", {

  t <- task_mc$clone(deep = TRUE)
  t
  self <- t
  private <- t$.__enclos_env__$private

  ## fields
  expect_is(t, "Task")
  expect_is(t$options, "Options")
  expect_equal(t$keys, keys)

  ## Options object fields
  expect_is(t$formulas, "Formulas")
  expect_is(t$datasets, "Datasets")
  expect_is(t$models, "Models")
  expect_is(t$seeds, "Seeds")
  expect_is(t$measure, "Measure")
  expect_false(t$show_progress)
  expect_false(t$keep_data)
  expect_false(t$parallel)
  expect_equal(t$labels, data_mc$y)
  expect_equal(t$label_var, "y")
  expect_equal(t$label_levels, c("a", "b", "c"))
  expect_equal(t$task_type, "multiclass")

  expect_is(t$formula, "formula")
  expect_is(t$dataset, "data.frame")
  expect_is(t$train, "data.frame")
  expect_is(t$test, "data.frame")
  expect_is(t$seed, "integer")
  expect_is(t$model, "Model")

  expect_is(t$search_seed, "integer")
  expect_is(t$cv_seed, "integer")
  expect_is(t$model_seed, "integer")

  expect_equal(t$id_col, "id")
  expect_equal(t$test_ids, 301L:400L)

  expect_is(t$preproc_call, "call")
  expect_is(t$resample_call, "call")
  expect_is(t$data_call, "call")
  expect_is(t$fit_call, "call")
  expect_is(t$pred_call, "call")

  expect_true(t$has_test_labels)
  expect_null(t$result)
  expect_false(t$done)

  ## methods
  expect_is(t$eval_call_args(quote(paste0(formula))), "call")

  ## private
  expect_error(private$.check_result())

})
