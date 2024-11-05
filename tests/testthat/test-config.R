# SPDX-FileCopyrightText: [2024] University of Manchester
# SPDX-License-Identifier: apache-2.0

test_that("get_pkg_cfg_dir fails with msg if supplied dir not found", {
  expect_error(get_pkg_cfg_dir("theconfigpath"))
})

test_that("write_user_cfg writes expected config to file", {
  tst_cfg <- get_test_cfg()

  exp_keys <- get_test_cfg_exp_keys()

  cfg_name <- "test_cfg.yml"

  # This dir should be cleaned up automatically afterwards
  tmp_dir <- local_write_cfg(cfg_name)

  # Check the config keys of the newly-created test config file
  res <- cfg_keys_all_valid(exp_keys = exp_keys, cfg_dir = tmp_dir,
                            cfg_name = cfg_name)

  expect_true(res)
})

test_that("sweep_key works with default key_sep", {
  expect_equal(sweep_key("one.two.three"), "one.two.three")
})

test_that("sweep_key works with non-default key_sep", {
  expect_equal(sweep_key("one.two.three", "$"), "one$two$three")
})

test_that("get_cfg_keys works", {
  exp_keys <- get_cfg_template_keys()
  expect_equal(get_cfg_keys(cfg = generate_cfg_template()), exp_keys)
})

test_that("get_cfg_val fails if keys not found", {
  tst_cfg <- get_test_cfg()
  expect_snapshot(get_cfg_val("non.existent", cfg = tst_cfg), error = TRUE)
})

test_that("get_cfg_val returns expected value", {
  tst_cfg <- get_test_cfg()

  val <- get_cfg_val("top.colours.sub.sub1", cfg = tst_cfg)
  expect_equal(val, "periwinkle")

  val <- get_cfg_val("top.locations.base_path", cfg = tst_cfg)
  expect_equal(val, file.path("basedir", "subdir", "level3dir"))
})

test_that("get_root gives expected msg if sys env var missing", {
  # Temporarily unset the required system env var
  dimex_store_val <- Sys.getenv("DIMEX_STORE")
  Sys.unsetenv("DIMEX_STORE")
  withr::defer(Sys.setenv("DIMEX_STORE" = dimex_store_val))

  expect_snapshot(get_root("main"), error = TRUE)
})

test_that("local_sys_env_vars temporarily alters vars", {

  local_sys_env_vars(c(DIMEX_STORE = "xxx", DIMEX_STORE_REF = "yyy"))

  # Check that the sys env vars have not been altered after running this test
  # with: 'Sys.getenv("DIMEX_STORE")' and 'Sys.getenv("DIMEX_STORE_REF")'

  expect_equal(Sys.getenv("DIMEX_STORE"), "xxx")
  expect_equal(Sys.getenv("DIMEX_STORE_REF"), "yyy")
})

test_that("get_dat_path gives error message for invalid env", {
  expect_snapshot(get_dat_path(env = "nonexistent"), error = TRUE)
})

test_that("get_dat_path gives expected path for default env ('main')", {

  tst_key <-"top.locations.other_path"

  tst_cfg <- get_test_cfg()

  # This path should be cleaned up afterwards
  cfg_dir <- local_mk_dir()

  # Temporarily set required system env var
  local_sys_env_vars(c(DIMEX_STORE = cfg_dir))

  act_path <- get_dat_path(tst_key, "main", cfg = tst_cfg)

  exp_path <- file.path(cfg_dir, "jump", "skip")
  expect_equal(act_path, exp_path)
})

test_that("get_dat_path gives expected path for env 'main'", {

  tst_key <-"top.locations.other_path"

  tst_cfg <- get_test_cfg()

  # This path should be cleaned up afterwards
  cfg_dir <- local_mk_dir()

  # Temporarily set required system env var
  local_sys_env_vars(c(DIMEX_STORE = cfg_dir))

  act_path <- get_dat_path(tst_key, "main", cfg = tst_cfg)

  exp_path <- file.path(cfg_dir, "jump", "skip")
  expect_equal(act_path, exp_path)
})

test_that("get_dat_path gives expected path for env 'ref'", {

  tst_key <-"top.locations.base_path"

  tst_cfg <- get_test_cfg()

  # This path should be cleaned up afterwards
  cfg_dir <- local_mk_dir()

  # Temporarily set required system env var
  local_sys_env_vars(c(DIMEX_STORE_REF = cfg_dir))

  act_path <- get_dat_path(tst_key, "ref",  cfg = tst_cfg)

  exp_path <- file.path(cfg_dir, "basedir", "subdir", "level3dir")
  expect_equal(act_path, exp_path)
})
