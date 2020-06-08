
test_that(".get_first_arg_name function work", {

  f <- function(a) .get_first_arg_name(f)
  expect_equal(f(), "a")
  b <- 1
  expect_equal(f(b), "b")

  f <- function() .get_first_arg_name(f)
  expect_null(f())

})

test_that("str assertion function work", {

  expect_equal(.assert_str("foo"), "foo")
  expect_null(.assert_str(NULL, allow_null = TRUE))

  expect_error(.assert_str(c("foo", "bar")))
  expect_error(.assert_str(NULL))
  expect_error(.assert_str(Inf))
  expect_error(.assert_str(NaN))
  expect_error(.assert_str(NA))
  expect_error(.assert_str(NA_character_))
  expect_error(.assert_str(1), "str must be a character scalar.")
  dummy_string <- 1
  expect_error(.assert_str(dummy_string), "dummy_string must be a character scalar.")

})

test_that("count assertion function work", {

  expect_equal(.assert_count(1), 1)
  expect_equal(.assert_count(Inf), Inf)
  expect_null(.assert_count(NULL, allow_null = TRUE))

  expect_error(.assert_count(c(1, 2)))
  expect_error(.assert_count(NULL))
  expect_error(.assert_count(NaN))
  expect_error(.assert_count(NA_integer_))
  expect_error(.assert_count(NA), "count must be a positive integer scalar.")
  dummy_count <- 0
  expect_error(.assert_count(dummy_count),
               "dummy_count must be a positive integer scalar.")

})

test_that("flag assertion function work", {

  expect_true(.assert_flag(TRUE))

  expect_error(.assert_flag(c(TRUE, FALSE)))
  expect_error(.assert_flag(NULL))
  expect_error(.assert_flag(Inf))
  expect_error(.assert_flag(NaN))
  expect_error(.assert_flag(NA), "flag must be a logical scalar.")
  dummy_flag <- 1
  expect_error(.assert_flag(dummy_flag), "dummy_flag must be a logical scalar.")

})

test_that("class assertion function work", {

  expect_equal(.assert_class("hoge", "character"), "hoge")
  expect_equal(.assert_class(NA, "logical"), NA)
  expect_equal(.assert_class(Inf, "numeric"), Inf)
  expect_null(.assert_class(NULL, "character", allow_null = TRUE))

  expect_error(.assert_class(NULL, "character", allow_null = FALSE))
  expect_error(.assert_class("hoge", "list"))
  dummy_object <- 1
  expect_error(.assert_class(dummy_object, "character"),
               "dummy_object must be a character class.")
  expect_error(.assert_class(1, 1),
               "class must be a character scalar.")
  expect_error(.assert_class(1, "numerics", 1),
               "allow_null must be a logical scalar.")

})

test_that("model engine assertion function work", {

  expect_is(.assert_engine("lm"), "character")
  expect_error(.assert_engine("foo"), "foo is not a supported engine.")
  expect_error(.assert_engine(NULL))
  expect_error(.assert_engine(NA_character_))

})

test_that("task_type assertion function work", {

  expect_equal(.assert_task_type("multiclass"), "multiclass")
  expect_error(.assert_task_type("hoge"), "hoge is not valid task type.")
  expect_error(.assert_task_type(NULL))
  expect_error(.assert_task_type(NA_character_))

})

test_that("fit_param assertion function work", {

  expect_equal(.assert_fit_param(list(a = 1, b = 2)), list(a = 1, b = 2))
  expect_null(.assert_fit_param(NULL))
  expect_error(.assert_fit_param(1), "fit_param must be a named list.")
  expect_error(.assert_fit_param(list(a = 1, 2)))

})

test_that("seed assertion function work", {

  expect_equal(.assert_seed(1L), 1L)
  expect_equal(.assert_seed(1.0), 1.0)
  expect_error(.assert_seed(NULL), "seed must be a integer scalar.")
  expect_error(.assert_seed(NA_integer_))
  expect_error(.assert_seed(c(1L, 2L)))

})
