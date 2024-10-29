#' Create a config object to specify parameters for a run
#'
#' @param config A list containing parameters to control the run.
#'   TODO: Describe the config options and their defaults
#' @param overrides A list containing settings with which to override the
#'   defaults. If NULL then the returned list will contain all default values.
#'
#' @return A list containing settings for a run, constructed from defaults which
#'   would be overridden if `overrides` is supplied.
#' @export
#'
#' @examples
#' get_config()
#' get_config(overrides = list(seed_val = 200))
#'
get_config <- function(config = NULL, overrides = NULL) {

  # Set the default config
  def_config <- list(
    seed_val     = 1409,
    # For testing, to limit the number of msoa's
    tst_msoa_lim = 2
  )

  # TODO: Read config from file to override the defaults
  # loaded_config <- yaml::read_yaml(...)
  # def_config <- modifyList(def_config, loaded_config)

  config <- config %||% def_config

  # TODO: Read config from file to override the defaults

  # Apply supplied overrides
  if (!is.null(overrides)) {
    config <- modifyList(config, overrides)
  }

  config
}

#' Run the steps in the data prep, analysis and visualisation workflow pipeline
#'
#' This function runs each step in the data preparation, data analysis and
#' visualisation pipeline. Note that it will generally take an appreciable
#' amount of time to run.
#'
#' @inheritParams get_config
#' @param steps_to_run A character string specifying comma-separated identifiers
#'   for steps to run in the workflow. These can be:
#'   1a, 1b, 1c, 1d, 1e, 1f, 2, 3a, 3b, 4a, 4b, 5a, 5b
#'   Default: "1a, 1b, 1c, 1d, 1e, 1f, 2, 3a, 3b, 4a, 4b, 5a, 5b" (run all
#'   steps)
#' @param config_overrides A list containing settings with which to override the
#'   default config values if required.
#' @param banner_only A boolean indicating whether to only show banners for each
#'   step which would be run, or to run the function as well (useful for
#'   checking which steps would be run).
#'
#' @return NULL (invisibly).
#' @export
#'
#' @examples
#' run_workflow("1a", banner_only = TRUE)
#' \dontrun{
#' run_workflow("1a")
#' run_workflow("1a,1b,1c,1d,1e,1f,2,3a,3b,4a,4b,5a,5b", banner_only = TRUE)
#' run_workflow(banner_only = TRUE)
#' run_workflow("4a, 4b")
#' run_workflow("1a, 1b, 1c, 1d, 1e, 1f, 2, 3a, 3b, 4a, 4b")
#' run_workflow()
#' }
#'
run_workflow <- function(steps_to_run = NULL, config = NULL,
                         config_overrides = NULL, banner_only = NULL) {

  steps_to_run <- steps_to_run %||%
    "1a, 1b, 1c, 1d, 1e, 1f, 2, 3a, 3b, 4a, 4b, 5a, 5b"

  banner_only <- banner_only %||% FALSE

  # TODO: Validate config first!

  steps <- c(
    "1a" = "Running data prep Step 1a",
    "1b" = "Running data prep Step 1b",
    "1c" = "Running data prep Step 1c",
    "1d" = "Running data prep Step 1d",
    "1e" = "Running data prep Step 1e",
    "1f" = "Running data prep Step 1f",
    "2"  = "Processing Activities",
    "3a" = "Processing exposures for July 2021",
    "3b" = "Processing exposures for Q1 2021",
    "4a" = "Collating results for July 2021",
    "4b" = "Collating results for Q1 2021",
    "5a" = "Plotting results for July 2021",
    "5b" = "Plotting results for Q1 2021"
  )

  funcs <- list(
    "1a" = run_data_prep_study_region,
    "1b" = run_data_prep_population,
    "1c" = run_data_prep_tus,
    "1d" = run_data_prep_pm25_cams,
    "1e" = run_data_prep_pm25_emep,
    "1f" = run_data_prep_pm25_gm,
    "2"  = process_activities,
    "3a" = process_exposures_jul_2021,
    "3b" = process_exposures_q1_2021,
    "4a" = collate_results_jul_2021,
    "4b" = collate_results_q1_2021,
    "5a" = plot_results_jul_2021,
    "5b" = plot_results_q1_2021
  )

  steps_to_run <- parse_steps_to_run(steps_to_run)

  max_width <- max(vapply(steps, stringr::str_length, FUN.VALUE = integer(1)))

  lapply(steps_to_run, function(step_id) {
    descrip <- unname(steps[which(names(steps) == step_id)])
    show_workflow_banner(step_id, descrip, max_width)

    func <- funcs[[step_id]]
    if (!banner_only) {
      func(config, config_overrides)
    }
  })

  invisible()
}

#' Parse which steps to run from a string
#'
#' @param step_ids A character string specifying comma-separated identifiers for
#'   steps in the workflow. These can be:
#'   1a, 1b, 1c, 1d, 1e, 1f, 2, 3a, 3b, 4a, 4b, 5a, 5b
#'
#' @return A character vector of the step IDs.
#' @export
#'
#' @examples
#' parse_steps_to_run("1a")
#' parse_steps_to_run("1a, 1b")
#' parse_steps_to_run("1a,1b,1c,")
#'
parse_steps_to_run <- function(step_ids) {
  parsed <- stringr::str_split_1(step_ids, ", *")
  parsed[which(nzchar(parsed))]
}
