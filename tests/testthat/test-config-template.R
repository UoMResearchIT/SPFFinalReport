# SPDX-FileCopyrightText: [2024] University of Manchester
# SPDX-License-Identifier: apache-2.0

test_that("config template has expected keys", {
  exp_keys <- get_cfg_template_keys()
  expect_equal(get_cfg_keys(cfg = generate_cfg_template()), exp_keys)
})

test_that("config template has valid keys", {
  cfg_templ <- generate_cfg_template()
  exp_keys <- get_cfg_template_keys()
  expect_true(cfg_keys_all_valid(exp_keys = exp_keys, cfg = cfg_templ))
})
