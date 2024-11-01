# SPDX-FileCopyrightText: [2024] University of Manchester
# SPDX-License-Identifier: apache-2.0

test_that("loading nonexistent sys env var shows correct msg", {
  expect_snapshot(chk_sys_env_vars("DIMEX_TST_SHOULD_EXIST"), error = TRUE)
})

test_that("loading 2 sys env vars where 1 is nonexistent shows correct msg", {
  Sys.setenv(DIMEX_TST_TMP = "dimex testing")
  sys_env_vars <- c("DIMEX_TST_TMP", "DIMEX_TST_SHOULD_EXIST")
  expect_snapshot(chk_sys_env_vars(sys_env_vars), error = TRUE)
})

test_that("loading 3 sys env vars where 2 are nonexistent shows correct msg", {
  sys_env_vars <- c("DIMEX_TST_SHOULD_EXIST_1", "DIMEX_TST_SHOULD_EXIST_2")
  expect_snapshot(chk_sys_env_vars(sys_env_vars), error = TRUE)
})
