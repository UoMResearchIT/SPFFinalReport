# SPDX-FileCopyrightText: [2024] University of Manchester
# SPDX-License-Identifier: apache-2.0

test_that("imputed tus data matches reference data", {

  skip_on_cran()
  skip_on_bioc()
  skip_on_ci()

  # To run this test, set either of these env vars as follows:
  #   > Sys.setenv(DIMEX_RUN_HLDT_2 = "true")
  #   > Sys.setenv(DIMEX_RUN_ALL_HLDT = "true")
  # Check these vars with:
  #   > Sys.getenv("DIMEX_RUN_HLDT_2")
  #   > Sys.getenv("DIMEX_RUN_ALL_HLDT")
  # Unset these vars with:
  #   > Sys.unsetenv("DIMEX_RUN_HLDT_2")
  #   > Sys.unsetenv("DIMEX_RUN_ALL_HLDT")

  # Check system env vars to determine whether to run this high level data test
  skip_if_r_cmd_check_or_not_configured("DIMEX_RUN_HLDT_2")

  cfg <- get_cfg()

  key <- "store.dat.interim.dirs.base"
  act_dat_root <- get_dat_path(key, "main", cfg = cfg)
  exp_dat_root <- get_dat_path(key, "ref", cfg = cfg)

  key <- "store.dat.interim.fnames.imputed_tus_dat"
  dat_fname <- get_cfg_val(key, cfg = cfg)

  exp_dat <- readRDS(file.path(exp_dat_root, dat_fname))
  act_dat <- readRDS(file.path(act_dat_root, dat_fname))

  expect_equal(act_dat, exp_dat, ignore_attr = TRUE)
})
