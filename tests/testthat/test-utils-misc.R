# SPDX-FileCopyrightText: [2024] University of Manchester
# SPDX-License-Identifier: apache-2.0

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

test_that("escape_txt works as expected", {
  expect_equal(escape_txt("."), "\\.")
  expect_equal(escape_txt("ab[c]"), "ab\\[c\\]")
  expect_equal(escape_txt("$30"), "\\$30")
})

test_that("is_strict_vec returns TRUE when null_ok is TRUE and x is NULL", {
  expect_true(is_strict_vec(NULL, null_ok = TRUE))
})

test_that("is_strict_vec shows expected msg when not(null_ok) and x is NULL", {
  expect_snapshot(is_strict_vec(NULL, null_ok = FALSE), error = TRUE)
})

test_that("is_strict_vec returns TRUE when x is an integer vector", {
  expect_true(is_strict_vec(c(1, 2, 3), null_ok = FALSE))
})

test_that("is_strict_vec returns TRUE when x is a character vector", {
  expect_true(is_strict_vec(letters[1:5], null_ok = FALSE))
})

test_that("is_strict_vec returns FALSE when x is a list", {
  expect_false(is_strict_vec(list(1, 2, 3), null_ok = FALSE))
})

test_that("is_strict_vec returns FALSE when x is a matrix", {
  expect_false(is_strict_vec(matrix(1:6, nrow = 2, ncol = 3), null_ok = FALSE))
})

test_that("is_strict_vec returns FALSE when x is a data frame", {
  df <- data.frame(name = c("Alice", "Bob"), score = c(98.5, 92.3))
  expect_false(is_strict_vec(df, null_ok = FALSE))
})
