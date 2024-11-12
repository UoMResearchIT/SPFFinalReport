# SPDX-FileCopyrightText: [2024] University of Manchester
# SPDX-License-Identifier: apache-2.0

test_that("activities 2021 data matches reference data", {

  skip_on_cran()
  skip_on_bioc()
  skip_on_ci()

  # Check system env vars to determine whether to run this high level data test
  skip_if_r_cmd_check_or_not_configured("DIMEX_RUN_HLDT_2")

  pop_dat <- get_pop_dat(env = "main", cfg = NULL)

  # Read config and set number of MSOAs to test
  cfg <- get_cfg(overrides = list(run = list(msoa_lim = 3L)))

  # Note: There are 346 areas (hence 346 tests to run if msoa_lim is NA)
  msoa_lim <- get_cfg_val_msoa(cfg = cfg)
  areas <- get_area_id_list(pop_dat$area_id, msoa_lim = NA)

  if (!is.na(msoa_lim)) {
    last <- floor(msoa_lim / 2)
    first <- msoa_lim - last

    areas <- areas[c(1:first, (length(areas) - last + 1):length(areas))]
  }

  # Example area IDs:
  #   c("E02000984", "E02000985", "E02006916", "E02006917")

  out_file_ext <- "rds"
  out_ref_file_ext <- "RData"

  key1 <- "store.out_ref.dirs.base"
  key2 <- "store.out_ref.dirs.activities"
  activities_out_ref_dir <- get_dat_path(c(key1, key2), env = "ref",
                                         cfg = cfg)

  key1 <- "store.out.dirs.base"
  key2 <- "store.out.dirs.activities"
  activities_out_dir <- get_dat_path(c(key1, key2), env = "main", cfg = cfg)

  key <- "store.out_ref.nm_patterns.activities"
  activities_nm_pattern_ref <- get_cfg_val(key, cfg = cfg)

  key <- "store.out.nm_patterns.activities"
  activities_nm_pattern <- get_cfg_val(key, cfg = cfg)

  for (k in areas) {

    # Reference data
    # Example activities output path for env 'ref':
    #   "Output_ref/CaseStudy2/Activities/activities_E02000984.RData"
    fpath_ref <- file.path(activities_out_ref_dir,
                           glue::glue("{activities_nm_pattern_ref}{k}",
                                      ".{out_ref_file_ext}"))

    # Data produced by running package code
    # Example activities output path for env 'main':
    #   "Output/CaseStudy2/Activities/activities_E02000984.rds"
    fpath <- file.path(activities_out_dir,
                       glue::glue("{activities_nm_pattern}{k}.{out_file_ext}"))

    # This should load one object 'activities_complete' into the current
    # environment; it is a data frame which is the historical reference data
    load(file = fpath_ref)

    act_activities_complete <- readRDS(fpath)

    expect_equal(act_activities_complete, activities_complete)
  }

})
