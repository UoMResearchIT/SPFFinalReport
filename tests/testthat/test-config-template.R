# SPDX-FileCopyrightText: [2024] University of Manchester
# SPDX-License-Identifier: apache-2.0

test_that("cfg_keys_all_valid works", {
  cfg_templ <- generate_cfg_template()
  exp_keys <- get_cfg_keys(cfg = cfg_templ)

  expect_true(cfg_keys_all_valid(exp_keys = exp_keys, cfg = cfg_templ))
})
