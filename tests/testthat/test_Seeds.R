
test_that("Seeds instance works", {

  s <- seeds$clone(deep = TRUE)
  s
  self <- s
  private <- s$.__enclos_env__$private
  super <- s$.__enclos_env__$super

  expect_is(s, "Seeds")
  expect_is(s[1L], "integer")
  expect_equal(dim(s$items), c(1L, 2L))

  expect_is(s$add(c(search = 111L, cv = 222L, model = 333L)), "Seeds")
  expect_equal(dim(s$items), c(2L, 2L))

  expect_is(s$add_random(), "Seeds")
  expect_equal(dim(s$items), c(3L, 2L))

  ## error check
  expect_error(s$add(c(search = 444L, cv = 555L, hoge = 666L)),
               "\"model\" is missing in the seed names.")

})
