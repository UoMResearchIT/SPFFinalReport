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
          base_dir = file.path("Data", "Raw"),

          # ------------------------------------------------------------------ #
          # Directory names
          dir_names = list(
            shapefiles = file.path("Shapefiles")
          ),

          shapefile_layer_names = list(
            ew_msoa = paste0("Middle_Layer_Super_Output_Areas_",
                             "(December_2011)_Boundaries"),
            uk_full = "gadm36_GBR_0"
          )
        ),

        # -------------------------------------------------------------------- #
        # Wrangled (processed) data, i.e. transformed/merged
        wrangled = list (

          base_dir = file.path("Data", "Processed"),

          # ------------------------------------------------------------------ #
          # Directory names
          dir_names = list(
            shapefiles = file.path("Shapefiles")
          ),

          # ---------------------------------------------------------------- #
          # File names
          file_names = list(
            shapefiles = list(
              ew_msoa        = "ew_msoa.rds",
              ew_msoa_region = "ew_msoa_region.rds",
              uk_full        = "uk_full.rds"
            )
          ),

          file_names_ref = list(
            shapefiles = list(
              msoa = "shapefiles.RData"
            )
          )
        )
      ),

      # ---------------------------------------------------------------------- #
      # Output paths
      out = list (

        # -------------------------------------------------------------------- #
        # Base directory
        base_dir = file.path("Output")
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
  sort(c(
    "run",
    "run.msoa_lim",
    "run.seed_val",
    "store",
    "store.dat",
    "store.dat.raw",
    "store.dat.raw.base_dir",
    "store.dat.raw.dir_names",
    "store.dat.raw.dir_names.shapefiles",
    "store.dat.raw.shapefile_layer_names",
    "store.dat.raw.shapefile_layer_names.ew_msoa",
    "store.dat.raw.shapefile_layer_names.uk_full",
    "store.dat.wrangled",
    "store.dat.wrangled.base_dir",
    "store.dat.wrangled.dir_names",
    "store.dat.wrangled.dir_names.shapefiles",
    "store.dat.wrangled.file_names",
    "store.dat.wrangled.file_names_ref",
    "store.dat.wrangled.file_names_ref.shapefiles",
    "store.dat.wrangled.file_names_ref.shapefiles.msoa",
    "store.dat.wrangled.file_names.shapefiles",
    "store.dat.wrangled.file_names.shapefiles.ew_msoa",
    "store.dat.wrangled.file_names.shapefiles.ew_msoa_region",
    "store.dat.wrangled.file_names.shapefiles.uk_full",
    "store.out",
    "store.out.base_dir"
  ))
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
