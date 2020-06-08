
test_that("print helper functions work", {

  ## .str_quote
  expect_equal(.str_quote("foo"), "\"foo\"")
  expect_equal(.str_quote(c("foo", "bar")), "\"foo\", \"bar\"")

  ## .str_df
  expect_equal(.str_df(data_mc), "400 x 6 (y, x1, x2, x3, x4, x5)")

  ## .str_list
  l <- list(a = "aaa", b = 123L, c = data_mc, d = sum, e = NULL)
  expect_equal(.str_list(l),
               "a = \"aaa\", b = 123, c = data.frame, d = function, e = NULL")

  ## .str_call
  cl <- call("round", 10.5)
  expect_equal(.str_call(cl), "round(10.5)")

  ## .str_task
  expect_equal(.str_task("multiclass", levels(data_mc$y)),
               "\"multiclass\" (Levels: a, b, c)")

  ## .str_keys
  expect_equal(.str_keys(c(aaa = "aaa", bbb = "bbb")),
               "aaa=\"aaa\", bbb=\"bbb\"")

  ## .str_model
  m <- new_model("glmnet_cv")
  expect_equal(.str_model(m), "Engine = \"glmnet_cv\"")

  ## .str_split
  expect_equal(.str_split("aaaaabbbbbccccc", 5L), c("aaaaa", "bbbbb", "ccccc"))

})
