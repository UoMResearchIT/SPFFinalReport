# SPDX-FileCopyrightText: [2024] University of Manchester
# SPDX-License-Identifier: apache-2.0

test_that("cfg_keys_all_valid fails if cfg is a character vecor not a list", {
  expect_snapshot(cfg_keys_all_valid(cfg = c("a", "b", "c")), error = TRUE)
})

test_that("get_invalid_cfg_keys correctly identifies invalid keys", {

  tst_cfg <- get_test_cfg()

  exp_keys <- get_test_cfg_exp_keys()

  tst_cfg <- modifyList(tst_cfg, list(top = list(something_else = "a-value")))

  exp_diff <- c("top.something_else")

  expect_false(cfg_keys_all_valid(exp_keys = exp_keys, cfg = tst_cfg))

  invalid_keys <- get_invalid_cfg_keys(exp_keys = exp_keys, cfg = tst_cfg)

  expect_identical(invalid_keys, as.list(exp_diff))
})
