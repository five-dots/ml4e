
test_that("Models instance works", {

  m <- models$clone(deep = TRUE)
  m
  self <- m
  private <- m$.__enclos_env__$private
  super <- m$.__enclos_env__$super

  expect_is(m, "Models")
  expect_is(m[1L], "Model")
  expect_equal(dim(m$items), c(2L, 2L))

  mod_lm <- new_model("lm")
  expect_is(m$add(mod_lm), "Models")
  expect_equal(dim(m$items), c(3L, 2L))

  ## .is_equal() prevent not to add the same engine's model
  mod_lm2 <- new_model("lm")
  expect_is(m$add(mod_lm2), "Models")
  expect_equal(dim(m$items), c(3L, 2L))

  ## error check
  expect_error(m$add(test = 1L),
               "A value for key = \"test\" must be a Model object.")

})
