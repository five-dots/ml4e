
test_that("Measure instance works", {

  m <- measure_mc$clone(deep = TRUE)
  m
  self <- m
  private <- m$.__enclos_env__$private

  ## fileds
  expect_is(m, "Measure")
  expect_equal(m$task_type, "multiclass")
  expect_equal(m$metrics, c(acc = "accuracy", logloss = "mn_log_loss"))
  expect_equal(m$metric_names, c("acc", "logloss"))

  ## methods
  expect_equal(m$assert_metric_name("acc"), "acc")
  expect_equal(m$assert_metric_name(NULL), "acc")
  expect_equal(m$assert_metric_name("accuracy"), "acc")
  expect_error(m$assert_metric_name("hoge"))

  expect_false(m$is_minimize_metric("acc"))
  expect_true(m$is_minimize_metric("logloss"))

  ## TODO measure pred
  ## c <- cv_mc$clone(deep = TRUE)
  ## c$do()
  ## scores <- m$do(c$pred, "y")
  ## expect_is(scores, "list")
  ## expect_named(scores, names(metrics))

  ## private
  expect_error(private$.assert_task_type("hoge"))
  expect_error(private$.assert_metrics("rmse"),
               "rmse is not supported metrics for multiclass task.")
  expect_error(private$.assert_pred(data_mc, "y"),
               "pred must have \"y\" and \".pred\" columns.")
  expect_is(private$.build_metric_set(), "metric_set")

})
