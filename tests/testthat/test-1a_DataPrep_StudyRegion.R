# SPDX-FileCopyrightText: [2024] University of Manchester
# SPDX-License-Identifier: apache-2.0

test_that("study region data matches reference data", {

  skip_on_cran()
  skip_on_bioc()
  skip_on_ci()

  # To run this test, set either of these env vars as follows:
  #   > Sys.setenv(DIMEX_RUN_HLDT_1A = "true")
  #   > Sys.setenv(DIMEX_RUN_ALL_HLDT = "true")
  # Check these vars with:
  #   > Sys.getenv("DIMEX_RUN_HLDT_1A")
  #   > Sys.getenv("DIMEX_RUN_ALL_HLDT")
  # Unset these vars with:
  #   > Sys.unsetenv("DIMEX_RUN_HLDT_1A")
  #   > Sys.unsetenv("DIMEX_RUN_ALL_HLDT")

  # Check system env vars to determine whether to run this high level data test
  skip_if_r_cmd_check_or_not_configured("DIMEX_RUN_HLDT_1A")

  # ------------------------------------ #

  cfg <- get_cfg()

  # --- --- --- #
  # Shapefiles

  # Shapefiles dir
  key1 <- "store.dat.wrangled.dirs.base"
  key2 <- "store.dat.wrangled.dirs.shapefiles"
  shape_dat_root <- get_dat_path(c(key1, key2), "main", cfg = cfg)
  shape_dat_root_ref <- get_dat_path(c(key1, key2), "ref", cfg = cfg)

  # Shapefiles reference data
  key <- "store.dat.wrangled.shapefiles_ref.msoa"
  msoa_fname_ref <- get_cfg_val(key, cfg = cfg)

  # Shapefiles ew_msoa data
  key <- "store.dat.wrangled.shapefiles.ew_msoa"
  ew_msoa_fname <- get_cfg_val(key, cfg = cfg)

  # ------------------------------------ #
  # Load data and run test

  # This should load 3 objects: ew_msoa, ew_msoa_region and uk_full into the
  # current environment. They are S4 objects of type 'Large
  # SpatialPolygonsDataFrame'
  load(file.path(shape_dat_root_ref, msoa_fname_ref))

  # This will load the ew_msoa data produced by running the package code
  # Note: The objects in the reference shapefiles.RData are S4 objects of type
  # Large SpatialPolygonsDataFrame whilst this is a data frame
  act_ew_msoa <- readRDS(file.path(shape_dat_root, ew_msoa_fname))

  # Retain only vars required for comparison
  act_ew_msoa <- as.data.frame(act_ew_msoa) %>%
    dplyr::select(area_id,
                  area_name,
                  cent_long,
                  cent_lat,
                  parent_area_name)

  # Note: The reference data has row names numbered from zero instead of 1, so
  # make the data match this before checking for equality
  rownames(act_ew_msoa) <- seq_len(nrow(act_ew_msoa)) - 1

  expect_equal(act_ew_msoa, as.data.frame(ew_msoa), ignore_attr = TRUE)
})
