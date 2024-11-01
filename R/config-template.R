# SPDX-FileCopyrightText: [2024] University of Manchester
# SPDX-License-Identifier: apache-2.0

#' Create a config template containing default values
#'
#' @return A list of lists containing the required config keys and values.
#' @export
#'
#' @details
#'   TODO: Describe config format and values
#'
#' @examples
#' \dontrun{
#' generate_cfg_template()
#' }
#'
generate_cfg_template <- function() {

  # Define package configuration as a list
  cfg = list(

    # ------------------------------------------------------------------------ #
    # Runtime parameters
    run = list(
      # See for the RNG
      seed_val = 1409L,

      # For interactive use, to limit the number of msoa's processed
      msoa_lim = 2L
    ),

    # ------------------------------------------------------------------------ #
    # Paths for data and output
    store = list(

      # ---------------------------------------------------------------------- #
      # Data paths
      dat = list(

        # -------------------------------------------------------------------- #
        # Raw data, e.g. csv files
        raw = list (

          # ------------------------------------------------------------------ #
          # Base directory
          base = file.path("Data", "Raw"),

          # ------------------------------------------------------------------ #
          # Shapefiles
          shapefiles = list(
            dir = file.path("Shapefiles")
          )
        ),

        # -------------------------------------------------------------------- #
        # Wrangled (processed) data, i.e. transformed/merged
        wrangled = list (

          # ------------------------------------------------------------------ #
          # Base directory
          base = file.path("Data", "Processed"),

          # ------------------------------------------------------------------ #
          # Shapefiles
          shapefiles = list(

            # ---------------------------------------------------------------- #
            # Base directory
            dir = file.path("Shapefiles"),

            names_ref = list(
              msoa_file = "shapefiles.RData"
            ),

            names = list(
              ew_msoa_file        = "ew_msoa.rds",
              ew_msoa_region_file = "ew_msoa_region.rds",
              uk_full_file        = "uk_full.rds"
            )
          )
        )
      ),

      # ---------------------------------------------------------------------- #
      # Output paths
      out = list (

        # -------------------------------------------------------------------- #
        # Base directory
        base = file.path("Output")
      )
    )
  )

  cfg
}


#' Retrieve a character vector of expected config keys
#'
#' @return A character vector of expected config keys.
#' @export
#'
#' @details
#' The returned keys should be the same as those returned by
#'   `get_cfg_keys(cfg = generate_cfg_template())`.
#'
#' @examples
#' get_cfg_template_keys()
#'
get_cfg_template_keys <- function() {
  c(
    "run",
    "run.seed_val",
    "run.msoa_lim",
    "store",
    "store.dat",
    "store.dat.raw",
    "store.dat.raw.base",
    "store.dat.raw.shapefiles",
    "store.dat.raw.shapefiles.dir",
    "store.dat.wrangled",
    "store.dat.wrangled.base",
    "store.dat.wrangled.shapefiles",
    "store.dat.wrangled.shapefiles.dir",
    "store.dat.wrangled.shapefiles.names_ref",
    "store.dat.wrangled.shapefiles.names_ref.msoa_file",
    "store.dat.wrangled.shapefiles.names",
    "store.dat.wrangled.shapefiles.names.ew_msoa_file",
    "store.dat.wrangled.shapefiles.names.ew_msoa_region_file",
    "store.dat.wrangled.shapefiles.names.uk_full_file",
    "store.out",
    "store.out.base"
  )
}

#' Retrieve the config template file name
#'
#' @param cfg_template_name A character string specifying the template config
#'   file name, or NULL for the default. Default: 'config-template.yml'
#'
#' @return A character string: the supplied config template name or the default
#'   config template name if none was provided.
#' @export
#'
#' @examples
#' get_cfg_template_name()
#' get_cfg_template_name("conftemplate.yml")
#'
get_cfg_template_name <- function(cfg_template_name = NULL) {
  cfg_template_name %||% "config-template.yml"
}

#' Retrieve the config template path
#'
#' @inheritParams get_pkg_cfg_dir
#' @param template_name A character string: the name of the config template file
#'   or NULL for the default. Default: The name returned by
#'   [get_cfg_template_name()]
#'
#' @return A character string: the path to the config template file.
#' @export
#'
#' @examples
#' get_cfg_template_path()
#' \dontrun{
#' get_cfg_template_path("cfg")
#' get_cfg_template_path(template_name = "cfg-file.yml")
#' get_cfg_template_path("cfg", "cfg-file.yml")
#' }
#'
get_cfg_template_path <- function(template_dir_name = NULL,
                                  template_name = NULL) {

  template_dir_name <- get_pkg_cfg_dir(template_dir_name)
  template_name <- get_cfg_template_name(template_name)

  file.path(template_dir_name, template_name)
}

#' Write the configuration template to file
#'
#' @inheritParams get_pkg_cfg_dir
#' @inheritParams get_cfg_template_path
#' @param cfg A list of nested lists, with 'leaf' nodes specifying configuration
#'   values. Default: The template config returned by [generate_cfg_template()]
#'
#' @return NULL (invisibly)
#' @export
#'
#' @examples
#' write_cfg_template()
#'
write_cfg_template <- function(template_dir_name = NULL, template_name = NULL,
                               cfg = NULL) {

  cfg <- cfg %||% generate_cfg_template()

  template_path <- get_cfg_template_path(template_dir_name = template_dir_name,
                                         template_name = template_name)

  # Save the template configuration to a YAML file
  yaml::write_yaml(cfg, template_path)

  invisible()
}
