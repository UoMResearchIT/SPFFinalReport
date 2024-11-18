# SPDX-FileCopyrightText: [2024] University of Manchester
# SPDX-License-Identifier: apache-2.0

#' Drop the name of the 'location' column from a vector of names
#'
#' @param str_nms A character vector of names.
#' @param nm_to_drop A character string: the name to drop from `str_nms`.
#'
#' @return The supplied character vector excluding `nm_to_drop`.
#' @export
#'
#' @examples
#' drop_locn(c("age", "type", "activity", "location"))
#' # => c("age", "type", "activity")
#'
#' drop_locn(c("age", "type", "activity", "locn"), nm_to_drop = "locn")
#' # => c("age", "type", "activity")
#'
drop_locn <- function(str_nms, nm_to_drop = NULL) {
  nm_to_drop <- nm_to_drop %||% "location"
  str_nms[-which(str_nms == nm_to_drop)]
}



#' Get grouping variables for the time use survey data
#'
#' @param strata An optional character vector containing names of stratification
#'   variables. If supplied, these are added to the activity and location
#'   variables to be used for grouping. Default: none.
#'
#' @return A character vector of names of grouping variables. These will be
#'   'activity' and 'location' together with any stratification variables (if
#'   supplied).
#' @export
#'
#' @examples
#' get_grouping_vars()
#' get_grouping_vars(c("sex", "agegr4", "nssec5", "daytype"))
#'
get_grouping_vars <- function(strata = NULL) {
  vars <- c("activity", "location")

  if (!is.null(strata)) {
    vars <- c(strata, vars)
  }
  vars
}

#' Get most popular location for each activity
#'
#' Get most popular location for each activity, possibly with stratification.
#'
#' @inheritParams get_grouping_vars
#' @param tus_dat A data frame of the time use survey data. This is expected to
#'   be in the format saved by [run_data_prep_tus()].
#' @param col_nm A character string: name of new column to create which will
#'   hold the most popular location for each activity (possibly by strata).
#' @param missing_ids An integer vector of the numeric id's which indicate
#'   missing locations.
#'
#' @return The summarised time use survey data with 'most popular' location
#'   added and rows then filtered to include one line for each combination of
#'   strata and activity. If there are multiple locations matching the maximum
#'   proportion of time spent in each location by stratum and activity then the
#'   first location id will be chosen.
#' @export
#'
#' @examples
#' \dontrun{
#' tus_dat <- get_tus_dat()
#' missing_ids <- c(-9, 0, 10, 90)
#' strata <- c("sex", "agegr4", "nssec5", "daytype")
#' get_most_popular_locn(tus_dat, "most_popular_locn", missing_ids, strata)
#' }
get_most_popular_locn <- function(tus_dat, col_nm, missing_ids, strata = NULL) {

  vars <- get_grouping_vars(strata)

  tus_dat <- tus_dat %>%

    # Only keep non-missing locations
    dplyr::filter(!(location %in% missing_ids)) %>%

    # Summarise by strata and activity
    dplyr::group_by_at(vars) %>%
    dplyr::summarise(n = length(location)) %>%
    dplyr::group_by_at(drop_locn(vars)) %>%

    # Get proportion of time spent in each location by strata and activity
    dplyr::mutate(p = n/sum(n),
                  max_propn = max(p)) %>%

    # Keep only most popular activity
    dplyr::filter(p == max_propn) %>%

    # Keep first location if more than one selected
    dplyr::mutate(first_locn = min(location)) %>%

    dplyr::filter(location == first_locn) %>%

    # Remove grouping structure
    dplyr::ungroup() %>%

    # keep only relevant columns
    dplyr::select_at(vars) %>%

    # Rename columns
    dplyr::rename({{ col_nm }} := location)

  tus_dat
}

#' Impute missing location data
#'
#' @inheritParams get_grouping_vars
#' @inheritParams get_most_popular_locn
#' @param .data A data frame or data frame extension (e.g. a tibble).
#'
#' @return The supplied `tus_dat` data frame with imputed data for missing
#'   locations. Imputed values are computed as the most popular location for
#'   activity by (optional) strata.
#' @export
#'
#' @examples
#' \dontrun{
#' tus_dat <- get_tus_dat()
#' strata <- c("sex", "agegr4", "nssec5", "daytype")
#' impute_missing(tus_dat, tus_dat, "most_popular_locn", missing_ids, strata)
#' }
#'
impute_missing <- function(.data, tus_dat, col_nm, missing_ids, strata = NULL) {

  vars <- get_grouping_vars(strata)

  # Merge most popular location for each activity by strata
  .data %>%
    dplyr::left_join(
      get_most_popular_locn(tus_dat, {{ col_nm }}, missing_ids, vars),
      by = drop_locn(vars)
    ) %>%

    # If location_popular is NA, substitute the most popular location
    dplyr::mutate(location_popular = ifelse(is.na(location_popular),
                                            .data[[col_nm]],
                                            location_popular)) %>%

    # Drop the 'most popular location' column
    dplyr::select(-c({{ col_nm }}))
}

#' Impute missing tus data
#'
#' @inheritParams ensure_valid_env
#' @inheritParams get_user_cfg_dir
#' @inheritParams get_user_cfg_name
#' @inheritParams write_cfg_template
#'
#' @return A data frame with the imputed tus data.
#' @export
#'
#' @examples
#' \dontrun{
#' # To override the config to save the imputed tus data:
#' overrides = list(store = list(save = list(imputed_tus_dat = TRUE)))
#' impute_tus_dat(cfg = get_cfg(overrides = overrides))
#' }
impute_tus_dat <- function(env = NULL, cfg_dir = NULL, cfg_name = NULL,
                           cfg = NULL) {

  # --- --- --- #
  tus_dat <- get_tus_dat(env, cfg_dir, cfg_name, cfg)

  save_imputed <- get_cfg_val("store.save.imputed_tus_dat", cfg = cfg)

  # --- --- --- #
  key1 <- "store.dat.raw.dirs.base"
  key2 <- "store.dat.raw.dirs.tus"
  tus_dat_dir <- get_dat_path(c(key1, key2), env, cfg = cfg)

  key <- "store.dat.raw.tus.uk_metadata_location"
  tus_meta_nm <- get_cfg_val(key, cfg = cfg)

  # --- --- --- #
  strata <- c("sex", "agegr4", "nssec5", "daytype")

  #  0 Unspecified location
  # 10 Unspecified location (not travelling)
  # -9 No answer/refused
  # 90 Unspecified transport mode
  missing_ids <- c(-9, 0, 10, 99)

  # ------------------------------------ #
  # Expected column types for tus metadata
  tus_meta_col_spec <- readr::cols(
    location = readr::col_integer(),
    location_label = readr::col_character()
  )

  tus_meta <- readr::read_csv(file.path(tus_dat_dir, tus_meta_nm),
                              col_types = tus_meta_col_spec) %>%
    dplyr::select(location_popular = location,
                  location_popular_label = location_label)

  # ------------------------------------ #
  # Fill in missing data using most popular activities
  tus_dat <- tus_dat %>%

    # Set missing location data to NA (where 'missing' is determined according
    # to the values in missing_ids)
    dplyr::mutate(
      location_popular = ifelse(location %in% missing_ids, NA, location)
    ) %>%

    impute_missing(tus_dat, "most_popular_locn", missing_ids, strata) %>%

    impute_missing(tus_dat, "most_popular_locn", missing_ids) %>%

    # Merge on location labels
    dplyr::left_join(tus_meta, by = "location_popular")

  # ------------------------------------ #
  if (save_imputed) {
    key <- "store.dat.interim.dirs.base"
    imp_tus_dat_root <- get_dat_path(key, "main", cfg = cfg)

    key <- "store.dat.interim.fnames.imputed_tus_dat"
    imp_tus_dat_fname <- get_cfg_val(key, cfg = cfg)

    saveRDS(tus_dat, file.path(imp_tus_dat_root, imp_tus_dat_fname))
  }

  tus_dat
}

#' Process Activities data
#'
#' @inheritParams ensure_valid_env
#' @inheritParams get_user_cfg_dir
#' @inheritParams get_user_cfg_name
#' @inheritParams write_cfg_template
#'
#' @return NULL (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' process_activities()
#' }
#'
process_activities <- function(env = NULL, cfg_dir = NULL, cfg_name = NULL,
                               cfg = NULL) {

  out_file_ext <- "rds"

  # ------------------------------------ #
  # Read population data
  pop_dat <- get_pop_dat(env, cfg_dir, cfg_name, cfg)

  key1 <- "store.out.dirs.base"
  key2 <- "store.out.dirs.activities"
  activities_out_dir <- get_dat_path(c(key1, key2), env, cfg = cfg)

  key <- "store.out.nm_patterns.activities"
  activities_nm_pattern <- get_cfg_val(key, cfg = cfg)

  # Number of msoa areas to process
  msoa_lim <- get_cfg_val_msoa()

  # Suppress summarise info without changing global options
  op <- options(dplyr.summarise.inform = FALSE)
  on.exit(options(op), add = TRUE, after = FALSE)

  # Set seed
  seed_val <- get_cfg_val("run.seed_val")
  set.seed(seed_val)

  tus_dat <- impute_tus_dat(env, cfg_dir, cfg_name, cfg)

  # ------------------------------------ #
  # Sample activity sequences (Method 1) - Complete sequences only

  # Loop for each MSOA
  for (k in unique(pop_dat$area_id)[1:msoa_lim]) {
    # Sample activities
    activities_complete <- sample_population(subset(pop_dat, area_id == k),
                                             subset(tus_dat, percmissing == 0),
                                             nsample = 1,
                                             weights = "weights_diary",
                                             pop_strata = c('area_id'),
                                             tus_strata = c('sex', 'agegr4', 'nssec5', 'daytype'),
                                             start_date = '2020-11-30',
                                             end_date = '2021-12-31',
                                             keep = c('activity', 'activity_label', 'location', 'location_label'))

    # Activity sequences run from 04:00-03:59 so need to "shift"
    # the end of the profiles into the next day
    activities_complete <- activities_complete %>%
      # Getting hour and resting date after 00:00
      dplyr::mutate(hour = (floor((time-1)/6) + 4) %% 24,
                    date = dplyr::if_else(hour %in% 0:3, date + 1, date)) %>%
      # Removign day information as we have to shift the day
      dplyr::select(-c(day, day_label, daytype, daytype_label, season, season_label)) %>%
      # Adding on day information
      dplyr::mutate(day_label = weekdays(date),
                    day = as.numeric(factor(weekdays(date), levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))),
                    daytype = dplyr::case_when(day %in% c(1,7) ~ 1,
                                               day %in% 2:6 ~ 2),
                    daytype_label = dplyr::case_when(day %in% c(1,7) ~ 'Weekend',
                                                     day %in% 2:6 ~ 'Weekday'),
                    season = dplyr::case_when(lubridate::month(date) %in% c(12, 1, 2) ~ 1,
                                              lubridate::month(date) %in% c(3:5) ~ 2,
                                              lubridate::month(date) %in% c(6:8) ~ 3,
                                              lubridate::month(date) %in% c(9:11) ~ 4),
                    season_label = dplyr::case_when(lubridate::month(date) %in% c(12, 1, 2) ~ 'Winter',
                                                    lubridate::month(date) %in% c(3:5) ~ 'Spring',
                                                    lubridate::month(date) %in% c(6:8) ~ 'Summer',
                                                    lubridate::month(date) %in% c(9:11) ~ 'Autumn')) %>%
      # Removing first day
      dplyr::filter(date >= as.Date('2020-12-01') & date <= as.Date('2021-12-31'))

    # Adding micro-environments to the dataset
    activities_complete <- activities_complete %>%
      dplyr::mutate(micro_group = dplyr::case_when(location %in% c(11:12) ~ "home",
                                                   location %in% c(13, 14, 15, 16, 17, 19, 20, 21) ~ "indoor",
                                                   location %in% c(18, 31, 32) ~ "outdoor",
                                                   location %in% c(30, 33:49, 90) ~ "transport"))

    # Aggregating to the hourly time series
    activities_complete <- activities_complete %>%
      # Getting time within hour
      dplyr::mutate(minutes = ((time - 1) %% 6) + 1) %>%
      # Grouping by population, date and hour to reduce
      dplyr::group_by(pop_id, date, hour) %>%
      # Sample prop to the environments
      dplyr::mutate(sample = sample(1:6, size = 1)) %>%
      # Only keep sampled time point
      dplyr::filter(minutes == sample) %>%
      # Removing unecesary columns
      dplyr::select(-c(minutes, sample))

    # Saving datasets
    # Example activities output path for env 'main':
    #   "Output/CaseStudy2/Activities/activities_E02000984.rds"
    fpath <- file.path(activities_out_dir,
                       glue::glue("{activities_nm_pattern}{k}.{out_file_ext}"))
    saveRDS(activities_complete, file = fpath)

    # Printing index
    print(k)
  }

  invisible()
}
