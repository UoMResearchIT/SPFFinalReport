# SPDX-FileCopyrightText: [2024] University of Manchester
# SPDX-License-Identifier: apache-2.0

test_that("population data matches reference data", {

  skip_on_cran()
  skip_on_bioc()
  skip_on_ci()

  # To run this test, set either of these env vars as follows:
  #   > Sys.setenv(DIMEX_RUN_HLDT_1B = "true")
  #   > Sys.setenv(DIMEX_RUN_ALL_HLDT = "true")
  # Reverse this with:
  #   > Sys.unsetenv("DIMEX_RUN_HLDT_1B")
  #   > Sys.unsetenv("DIMEX_RUN_ALL_HLDT")

  # Check system env vars to determine whether to run this high level data test
  skip_if_r_cmd_check_or_not_configured("DIMEX_RUN_HLDT_1B")

  # ------------------------------------ #

  cfg <- get_cfg()

  # --- --- --- #
  # Population data

  # Shapefiles dir
  key1 <- "store.dat.wrangled.dirs.base"
  key2 <- "store.dat.wrangled.dirs.population"
  pop_dat_root_ref <- get_dat_path(c(key1, key2), "ref", cfg = cfg)

  # Population reference data
  key <- "store.dat.wrangled.population_ref.pop_dat"
  pop_dat_fname_ref <- get_cfg_val(key, cfg = cfg)

  # ------------------------------------ #
  # pop_dat will be loaded in setup.R
  # Save the actual pop_dat as it will be overwritten
  act_pop_dat <- pop_dat

  # This will load the population data produced by running the package code
  # *NB* Load this /after/ setting pop_dat as it will be overwritten here
  # This should load 1 object 'pop_dat' into the current environment; it is a
  # data frame
  load(file.path(pop_dat_root_ref, pop_dat_fname_ref))

  expect_equal(act_pop_dat, pop_dat)
})
