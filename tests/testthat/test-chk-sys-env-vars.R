# SPDX-FileCopyrightText: [2024] University of Manchester
# SPDX-License-Identifier: apache-2.0

test_that("loading nonexistent sys env var shows correct msg", {
  expect_snapshot(ensure_sys_env_vars("DIMEX_TST_VAR1"), error = TRUE)
})

test_that("loading 2 sys env vars where 1 is nonexistent shows correct msg", {
  Sys.setenv(DIMEX_TST_TMP = "dimex testing")
  sys_env_vars <- c("DIMEX_TST_VAR1", "DIMEX_TST_VAR2")
  expect_snapshot(ensure_sys_env_vars(sys_env_vars), error = TRUE)
})

test_that("loading 3 sys env vars where 2 are nonexistent shows correct msg", {
  sys_env_vars <- c("DIMEX_TST_VAR1", "DIMEX_TST_VAR2")
  expect_snapshot(ensure_sys_env_vars(sys_env_vars), error = TRUE)
})

test_that("ensure_sys_env_vars succeeds if vars are found", {

  vars <- c(DIMEX_TST_VAR1 = "dog", DIMEX_TST_VAR2 = "puppy")

  # Temporarily create the test variables in the environment (these will be
  # automatically cleaned up)
  local_sys_env_vars(vars)

  # Check that the sys env vars do not exist after running this test with:
  # 'Sys.getenv("DIMEX_TST_VAR1")' and 'Sys.getenv("DIMEX_TST_VAR2")'

  expect_success(expect_null(ensure_sys_env_vars(names(vars))))
})
