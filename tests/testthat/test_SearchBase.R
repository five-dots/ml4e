
test_that("SearchBase instance works", {

  s <- SearchBase$new(options_mc, keys)
  s
  self <- s
  private <- s$.__enclos_env__$private

  ## fields
  expect_is(s, "SearchBase")
  expect_is(s$options, "Options")
  expect_equal(s$keys, keys)
  expect_null(s$result)

  expect_error(s$get_rank())
  expect_error(s$get_best_param())
  expect_error(private$.check_if_no_result())
  expect_is(private$.cv_by_fit_param(1L, alpha = 0.5, use_min = 1L),
            "data.frame")

})
