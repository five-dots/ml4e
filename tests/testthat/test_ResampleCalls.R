
test_that("ResampleCalls instance works", {

  r <- resample_calls$clone(deep = TRUE)
  r
  self <- r
  private <- r$.__enclos_env__$private

  expect_is(r, "ResampleCalls")
  expect_is(r[1L], "call")
  expect_equal(dim(r$items), c(1L, 2L))
  expect_is(r$get_resample(data_mc, 1L, seed = 123L), "rset")

  r$add(quote(rsample::vfold_cv(data, v = 2L)))
  expect_equal(dim(r$items), c(2L, 2L))

  ## Error check
  expect_error(r$add(test01 = 1L),
               "A value for key = \"test01\" must be a call object.")

  expect_error(r$get_resample(1L), "data must be a data.frame class.")
  expect_error(r$get_resample(data_mc, 1L, 0.5), "seed must be a integer scalar.")

})
