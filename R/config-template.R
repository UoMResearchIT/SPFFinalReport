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
      # Seed for the RNG
      seed_val = 1409L,

      # Limit for the number of MSOAs that will be processed; to process all
      # MSOAs, set 'msoa_lim = NA'
      msoa_lim = 2L
    ),

    # ------------------------------------------------------------------------ #
    # Paths for data and output
    store = list(

      # ---------------------------------------------------------------------- #
      # What to save
      save = list(
        imputed_tus_dat = "false"
      ),

      # ---------------------------------------------------------------------- #
      # Data paths
      dat = list(

        # -------------------------------------------------------------------- #
        # Raw data, e.g. csv files
        raw = list (

          # ------------------------------------------------------------------ #
          # Directory names
          dirs = list(
            base = file.path("Data", "Raw"),
            shapefiles = file.path("Shapefiles"),
            population = file.path("Population"),
            tus = file.path("TimeUseSurvey"),
            misc = file.path("Misc")
          ),

          shapefile_layers = list(
            ew_msoa = paste0("Middle_Layer_Super_Output_Areas_",
                             "(December_2011)_Boundaries"),
            uk_full = "gadm36_GBR_0"
          ),

          tus = list(
            uk_metadata_location = "uktus_metadata_location.csv"
          ),

          misc = list(
            # National Statistics Socio-economic classification
            nssec_class = "nssec_classification.csv",
            # Standard Occupational Classification
            soc2010_class ="soc2010_classification.csv",
            # Standard Industrial Classification (SIC2007 labels and codes)
            sic2007_class = "sic2007_classification.csv"
          ),

          # Patterns for file names which have a number appended
          nm_patterns = list(
            pop_lad_tus = "lad_TUS_"
          )
        ),

        # -------------------------------------------------------------------- #
        # Interim data (semi-processed)
        interim = list (
          dirs = list(
            base = file.path("Data", "Interim")
          ),

          fnames = list (
            imputed_tus_dat = "imputed_tus_dat.rds"
          )
        ),

        # -------------------------------------------------------------------- #
        # Wrangled (processed) data, i.e. transformed/merged
        wrangled = list (

          # ------------------------------------------------------------------ #
          # Directory names
          dirs = list(
            base = file.path("Data", "Processed"),
            shapefiles = file.path("Shapefiles"),
            population = file.path("Population"),
            tus = file.path("TimeUseSurvey")
          ),

          # ---------------------------------------------------------------- #
          # File names

          shapefiles = list(
            ew_msoa        = "ew_msoa.rds",
            ew_msoa_region = "ew_msoa_region.rds",
            uk_full        = "uk_full.rds"
          ),

          shapefiles_ref = list(
            msoa = "shapefiles.RData"
          ),

          population = list(
            pop_dat = "pop_dat.rds"
          ),

          population_ref = list(
            pop_dat = "pop_dat.RData"
          ),

          time_use_survey = list(
            tus_dat = "tus_dat.rds"
          ),

          time_use_survey_ref = list(
            tus_dat = "tus_dat.RData"
          )
        )
      ),

      # ---------------------------------------------------------------------- #
      # Output paths
      out = list (

        # -------------------------------------------------------------------- #
        # Directory names
        dirs = list(
          base = file.path("Output"),
          activities = file.path("CaseStudy2", "Activities")
        ),

        # Patterns for file names which have a number appended
        nm_patterns = list(
          activities = "activities_"
        )
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
#' The returned keys should be the same as keys in the config that would be
#'   created with `generate_cfg_template()`.
#'
#' @examples
#' get_cfg_template_keys()
#'
get_cfg_template_keys <- function() {
  get_cfg_keys(cfg = generate_cfg_template())
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
