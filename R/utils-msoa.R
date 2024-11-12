# SPDX-FileCopyrightText: [2024] University of Manchester
# SPDX-License-Identifier: apache-2.0

#' Get the MSOA value from the config
#'
#' @inheritParams get_cfg_val
#' @inheritParams get_user_cfg_dir
#' @inheritParams get_user_cfg_name
#' @inheritParams write_cfg_template
#'
#' @return An integer: the configured MSOA value which is either the number
#'   specified, or, if this has a value of `NA`, the total number of MSOAs in
#'   the given population data file.
#' @export
#'
#' @examples
#' get_cfg_val_msoa(cfg = generate_cfg_template())
#'
get_cfg_val_msoa <- function(key = NULL, cfg_dir = NULL, cfg_name = NULL,
                             cfg = NULL) {

  key <- key %||% "run.msoa_lim"
  cfg <- cfg %||% read_user_cfg(cfg_dir, cfg_name)

  get_cfg_val(key, cfg = cfg)
}

#' Get a list of unique MSOA area id's, possibly subsetted
#'
#' Retrieve a sorted list of unique Middle layer Super Output Area id's, limited
#'   to a maximum of `msoa_lim` if it is provided.
#'
#' @param area_ids The area id's as a character vector.
#' @param msoa_lim The maximum number of area id's to return, or NA for all of
#'   them. Default: NA
#'
#' @return The sorted unique area id's, limited to `msoa_lim` if it is supplied.
#' @export
#'
#' @examples
#' \dontrun{
#' pop_dat <- get_pop_dat()
#' # Get the first 5 unique area id's
#' get_area_id_list(pop_dat$area_id, 5)
#' }
get_area_id_list <- function(area_ids, msoa_lim = NULL) {
  msoa_lim <- msoa_lim %||% NA
  area_ids <- sort(unique(area_ids))
  if (!is.na(msoa_lim)) {
    area_ids <- area_ids[1:msoa_lim]
  }
  area_ids
}
