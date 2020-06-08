
test_that("Option instance works", {

  o <- Option$new(key01 = y ~ ., y ~ x1 + x2, .class = "formula")
  o
  self <- o
  private <- o$.__enclos_env__$private
  super <- o$.__enclos_env__$super

  ## Fields
  expect_is(o, "Option")
  expect_equal(o$value_width, Inf)
  expect_equal(private$.key_prefix, "opt")
  expect_equal(private$.key_width, 2L)
  expect_equal(private$.value_width, Inf)

  ## Print
  o$value_width <- 3L
  ## o$print_items("Formulas", .str_call)
  expect_equal(o$value_width, 3L)

  ## Add
  expect_is(o$add(y ~ x2), "Option")
  expect_equal(o$keys, c("key01", "opt01", "opt02"))

  o$add(y ~ x2) # Add duplicated value
  expect_equal(o$length, 3L)

  ## Removal check
  expect_is(o$remove("key01"), "Option")
  expect_equal(o$length, 2L)
  o$remove("opt01")
  expect_error(o$remove("opt02"), "Cannot remove the last item.")

  ## Error check: Constructor
  expect_error(Option$new(), "Options must have at least one row.")

  expect_error(Option$new(.key_prefix = 1L),
               ".key_prefix must be a character scalar.")

  expect_error(Option$new(.key_width = -1L),
               ".key_width must be a positive integer scalar.")

  expect_error(Option$new(.value_width = -1L),
               ".value_width must be a positive integer scalar.")

  ## Error check: Methods
  expect_error(o$clear())

  expect_error(o$print_items(items_name = 1L),
               "items_name must be a character scalar.")

  expect_error(o$print_items(format_fun = 1L),
               "format_fun must be a function class.")

})
