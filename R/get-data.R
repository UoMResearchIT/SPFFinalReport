# SPDX-FileCopyrightText: [2024] University of Manchester
# SPDX-License-Identifier: apache-2.0

#' Get the wrangled population data
#'
#' @inheritParams ensure_valid_env
#' @inheritParams get_user_cfg_dir
#' @inheritParams get_user_cfg_name
#' @inheritParams write_cfg_template
#'
#' @return A `data.frame` containing the population data. See [data.frame()].
#' @export
#'
#' @examples
#' \dontrun{
#' get_pop_dat()
#' }
#'
get_pop_dat <- function(env = NULL, cfg_dir = NULL, cfg_name = NULL,
                        cfg = NULL) {

  env <- env %||% "main"
  cfg <- cfg %||% read_user_cfg(cfg_dir, cfg_name)

  key1 <- "store.dat.wrangled.dirs.base"
  key2 <- "store.dat.wrangled.dirs.population"
  pop_dat_dir <- get_dat_path(c(key1, key2), env, cfg = cfg)

  key <- "store.dat.wrangled.files.shapefiles.pop_dat"
  pop_dat_nm <- get_cfg_val(key, cfg = cfg)

  readRDS(file.path(pop_dat_dir, pop_dat_nm))
}

#' Get the wrangled time use survey data
#'
#' @inheritParams ensure_valid_env
#' @inheritParams get_user_cfg_dir
#' @inheritParams get_user_cfg_name
#' @inheritParams write_cfg_template
#'
#' @return A `data.frame` containing the time use survey data. See [data.frame()].
#' @export
#'
#' @examples
#' \dontrun{
#' get_tus_dat()
#' }
#'
get_tus_dat <- function(env = NULL, cfg_dir = NULL, cfg_name = NULL,
                        cfg = NULL) {

  env <- env %||% "main"
  cfg <- cfg %||% read_user_cfg(cfg_dir, cfg_name)

  key1 <- "store.dat.wrangled.dirs.base"
  key2 <- "store.dat.wrangled.dirs.tus"
  tus_dat_dir <- get_dat_path(c(key1, key2), env, cfg = cfg)

  key <- "store.dat.wrangled.files.shapefiles.tus_dat"
  tus_dat_nm <- get_cfg_val(key, cfg = cfg)

  readRDS(file.path(tus_dat_dir, tus_dat_nm))
}
