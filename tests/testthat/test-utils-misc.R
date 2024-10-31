test_that("%notin% works for a single value to (not) match", {
  expect_true("a" %notin% c("b", "c"))
  expect_false("a" %notin% c("a", "b", "c"))
})

test_that("%notin% works for a vector value to (not) match", {
  expect_identical(c("a", "z") %notin% c("b", "c"), c(TRUE, TRUE))
  expect_identical(c("a", "b") %notin% c("b", "c"), c(TRUE, FALSE))
  expect_identical(c("a", "b", "a") %notin% c("b", "c"), c(TRUE, FALSE, TRUE))
  expect_identical(c("a", "b") %notin% c("b", "c", "d"), c(TRUE, FALSE))
})
