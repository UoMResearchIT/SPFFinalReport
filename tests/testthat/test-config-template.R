# SPDX-FileCopyrightText: [2024] University of Manchester
# SPDX-License-Identifier: apache-2.0

test_that("config template has valid keys", {
  exp_keys <- get_cfg_template_keys()

  cfg_templ <- generate_cfg_template()
  expect_true(cfg_keys_all_valid(exp_keys = exp_keys, cfg = cfg_templ))
})

test_that("config template has valid keys", {
  exp_keys <- get_cfg_template_keys()

  cfg_templ <- generate_cfg_template()
  expect_true(cfg_keys_all_valid(exp_keys = exp_keys, cfg = cfg_templ))
})
