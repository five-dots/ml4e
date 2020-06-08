test_that("Datasets instance works", {

  d <- datasets_mc$clone(deep = TRUE)
  d
  self <- d
  private <- d$.__enclos_env__$private

  expect_is(d, "Datasets")
  expect_is(d[1L], "data.frame")
  expect_is(d$get_train(1L), "data.frame")
  expect_is(d$get_test(1L), "data.frame")
  expect_is(d$get_resample(1L, 1L, seed = 123L), "rset")
  expect_equal(dim(d$items), c(1L, 2L))
  expect_equal(d$id_col, id_col)
  expect_equal(d$test_ids, test_ids)
  expect_true(d$has_id_col)
  expect_true(d$has_test_ids)
  expect_is(d$resample_calls, "ResampleCalls")

  expect_is(d$add(d[1L] %>% dplyr::mutate(x6 = x5)),
            "Datasets")
  expect_equal(dim(d$items), c(2L, 2L))

  ## preproc_call
  preproc_call <- quote(preproc_identity(data = data))
  expect_equal(dim(d[1L, preproc_call]), c(400L, 7L))
  expect_equal(dim(d$get_train(1L, preproc_call)), c(300L, 6L))
  expect_equal(dim(d$get_test(1L, preproc_call)), c(100L, 6L))
  expect_is(d$get_resample(1L, 1L, 123L, preproc_call), "rset")

  ## No id_col/test_ids case
  d2 <- new_datasets(data_mc, .id_col = NULL, .test_ids = NULL,
                     .resample_calls = resample_calls)
  expect_null(d2$id_col)
  expect_null(d2$test_ids)
  expect_false(d2$has_id_col)
  expect_false(d2$has_test_ids)
  expect_error(d2$get_test(1L))

  ## Error on construction
  expect_error(new_datasets(1L),
               "A value for key = \"dat_01\" must be a data.frame object.")
  expect_error(new_datasets(data_mc, .id_col = 1L),
               ".id_col must be a character scalar.")
  expect_error(new_datasets(data_mc, .test_ids = "foo"),
               "test_ids must be positive integer vector.")
  expect_error(new_datasets(data_mc, .test_ids = 350L:450L),
               "test_ids must be between 1 to 400.")

  data_mc2 <- tibble::rowid_to_column(data_mc, var = "id")
  expect_error(new_datasets(data_mc2, data_mc, .id_col = "id"),
               "data.frame does not contains \"id\" columns.")

  data_mc3 <- data_mc %>% dplyr::mutate(`1x` = x1)
  expect_error(new_datasets(data_mc3))

})
