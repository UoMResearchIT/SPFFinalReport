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

test_that("is_int_val works", {
  expect_false(is_int_val("5"))
  expect_true(is_int_val(10))
  expect_false(is_int_val(16.5))
  expect_false(is_int_val("apple"))
  expect_true(is_int_val(-1))
})

test_that("parse_nested_list works", {

  nested <- list(
    a = "1",
    b = "12.5",
    c = "NA",
    d = "alphabet",
    e = list(
      first = "NA",
      second = "notanumber",
      third = "100",
      fourth = "0.45",
      fifth = list(
        one = "teal",
        two = "NA",
        three = "999"
      )
    )
  )

  parsed <- recurse_nested(nested, parse_string)

  expect_equal(parsed$a, 1L)
  expect_equal(parsed$b, 12.5)
  expect_equal(parsed$c, NA)
  expect_equal(parsed$d, "alphabet")
  expect_equal(parsed$e$first, NA)
  expect_equal(parsed$e$second, "notanumber")
  expect_equal(parsed$e$third, 100L)
  expect_equal(parsed$e$fourth, 0.45)
  expect_equal(parsed$e$fifth$one, "teal")
  expect_equal(parsed$e$fifth$two, NA)
  expect_equal(parsed$e$fifth$three, 999L)
})
