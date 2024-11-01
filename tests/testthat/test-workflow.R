# SPDX-FileCopyrightText: [2024] University of Manchester
# SPDX-License-Identifier: apache-2.0

test_that("get_steps_to_run parses steps correctly", {
  expect_equal(c("1a"), parse_steps_to_run("1a"))
  expect_equal(c("1a", "1b"), parse_steps_to_run("1a,1b"))
  expect_equal(c("1a", "1b", "1c"), parse_steps_to_run("1a,  1b,1c,"))
  expect_equal(c("1a", "1b", "1c"), parse_steps_to_run("1a,  1b,1c,  "))
})
