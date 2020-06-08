
test_that("Formulas instance works", {

  f <- formulas$clone(deep = TRUE)
  f
  self <- f
  private <- f$.__enclos_env__$private
  super <- f$.__enclos_env__$super

  expect_is(f, "Formulas")
  expect_equal(f[1L], y ~ .)
  expect_equal(dim(f$items), c(1L, 2L))

  expect_is(f$add(y ~ x3 - 0L), "Formulas")
  expect_is(f["key01"] <- y ~ x4, "formula")
  expect_equal(dim(f$items), c(3L, 2L))

  expect_equal(f$keys, c("all", "fml_01", "key01"))

  ## Check formula's env
  expect_identical(environment(f[1L]), globalenv())

  ## error check
  expect_error(f$add(1L),
               "A value for key = \"fml_02\" must be a formula object.")
  expect_error(f$add(x ~ .),
               "All formula response must be the same.")

})
