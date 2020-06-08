
test_that("helper functions work", {

  ## engine
  expect_is(supported_engines(), "character")

  ## tasks
  tasks <- supported_tasks("glmnet_cv")
  expect_equal(tasks,
               c("binary", "multiclass", "regression", "poisson", "survival"))
  expect_error(supported_tasks("foo"), "foo is not a supported engine.")

  ## metrics
  metrics <- supported_metrics()
  expect_is(metrics, "data.frame")
  expect_named(metrics, c("metric", "type", "minimize"))

  ## labels from formula
  ## TODO uncomment
  ## expect_equivalent(.labels_from_formula(y ~ ., data_mc), data_mc$y)

  ## labels from dmatrix
  ## TODO uncomment
  ## dmat <- data_xgboost(data_mc, formula)$data
  ## expect_equal(.labels_from_dmatrix(dmat), as.integer(data_mc$y) - 1L)

  ## family from task
  expect_equal(family_from_task("multiclass"), "multinomial")
  expect_equal(family_from_task("binary"), "binomial")
  expect_equal(family_from_task("regression"), "gaussian")
  expect_equal(family_from_task("poisson"), "poisson")
  expect_error(family_from_task("foo"))
  expect_error(family_from_task(NULL))
  expect_error(family_from_task(NA_character_))

  ## as_ratio
  expect_equal(.as_ratio(0.1, 1.0), 0.1)
  expect_equal(.as_ratio(10.0, 100.0), 0.1)

  ## .filter_or_slice
  expect_equal(nrow(.filter_or_slice(data_mc, y == "a")), 103L)
  expect_equal(nrow(.filter_or_slice(data_mc, 1L:50L)), 50L)
  expect_error(.filter_or_slice(1L), "df must be a data.frame class.")

  ## .get_matched_index
  data_mc2 <- data_mc[1L:50L, ]
  expect_equal(.get_matched_index(data_mc, data_mc2), 1L:50L)

})
