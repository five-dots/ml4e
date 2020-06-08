
test_that("PreprocCalls instance works", {

  p <- preproc_calls$clone(deep = TRUE)
  p
  self <- p
  private <- p$.__enclos_env__$private
  super <- p$.__enclos_env__$super

  expect_is(p, "PreprocCalls")
  expect_is(p[1L], "call")
  expect_equal(dim(p$items), c(1L, 2L))

  expect_is(p$add(quote(preproc_identity(data, foo = "bar"))),
            "PreprocCalls")
  expect_equal(dim(p$items), c(2L, 2L))

  eval_data <- list(data = data_mc, formula = y ~ .)
  expect_is(rlang::eval_tidy(p[1L], eval_data), "data.frame")

  ## error check
  expect_error(p$add(test02 = 1L),
               "A value for key = \"test02\" must be a call object.")

})
