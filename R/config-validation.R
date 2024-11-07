# SPDX-FileCopyrightText: [2024] University of Manchester
# SPDX-License-Identifier: apache-2.0

#' Ensure parameters are of the expected type
#'
#' @inheritParams get_user_cfg_dir
#' @inheritParams get_user_cfg_name
#' @inheritParams write_cfg_template
#' @param exp_keys A character vector of expected config keys or NULL for the
#'   default. Default: Keys as returned by [get_cfg_template_keys()]
#' @param invalid_keys A character vector of keys which have been identified as
#'   invalid (could be empty).
#'
#' @return NULL (invisibly).
#' @export
#'
#' @examples
#' ensure_cfg_param_types()
#'
ensure_cfg_param_types <- function(exp_keys = NULL, cfg_dir = NULL,
                                   cfg_name = NULL, invalid_keys = NULL,
                                   cfg = NULL) {

  is_single_str <- function(x) {
    is.null(x) || (is.character(x) && is.vector(x) && length(x == 1))
  }

  is_str_vec <- function(x) {
    is.null(x) || (is_strict_vec(x, null_ok = TRUE) && is.character(x))
  }

  is_list <- function(x) {
    is.null(x) || is.list(x)
  }

  if (!is_str_vec(exp_keys)) {
    cli::cli_abort(c(
      "{.var exp_keys} is an unexpected type",
      "i" = "{.var exp_keys} must be a character vector.",
      "x" = "You've supplied an object of class '{class(exp_keys)}'."
    ))
  }

  if (!is_list(cfg)) {
    cli::cli_abort(c(
      "{.var cfg} is an unexpected type",
      "i" = "{.var cfg} must be a list.",
      "x" = "You've supplied an object of class '{class(cfg)}'."
    ))
  }

  if (!is_single_str(cfg_dir)) {
    cli::cli_abort(c(
      "{.var cfg_dir} is not valid",
      "i" = "{.var cfg_dir} must be a single character string.",
      "x" = "You've supplied \"{cfg_dir} ({class(cfg_dir)})\"."
    ))
  }

  if (!is_single_str(cfg_name)) {
    cli::cli_abort(c(
      "{.var cfg_name} is not valid",
      "i" = "{.var cfg_name} must be a single character string.",
      "x" = "You've supplied \"{cfg_name} ({class(cfg_name)})\"."
    ))
  }

  if (!is_list(invalid_keys)) {
    cli::cli_abort(c(
      "{.var invalid_keys} is an unexpected type",
      "i" = "{.var invalid_keys} must be a character vector.",
      "x" = "You've supplied an object of class '{class(invalid_keys)}'."
    ))
  }

  invisible()
}

#' Check the validity of a specified 'environment' string
#'
#' @param env A character string specifying the environment for which to get the
#'   path. This should be one of 'main', 'ref' or 'test'. See section *Details*
#'   for more information.
#' @param null_ok Whether it is valid for `env` to be NULL.
#'
#' @return NULL (invisibly)
#' @export
#'
#' @details
#' Valid environments for data are: <br>
#'   - main: Data produced by running the package code
#'   - ref:  Reference data (historical data which is used for regression
#'     testing)
#'   - test: Test data
#'
#' @examples
#' ensure_valid_env("ref")
#' \dontrun{
#' ensure_valid_env("nonexistent") # => Causes an error
#' }
#'
ensure_valid_env <- function(env, null_ok = NULL) {

  valid_envs <- c("main", "ref", "test")

  null_ok <- null_ok %||% TRUE

  if (!null_ok && is.null(env)) {
    cli::cli_abort(c(
      "{.var env} must be non-NULL",
      "i" = "Valid env{?s} {?is/are}: {cli::cli_vec(valid_envs)}.",
      "x" = "You've supplied '{env}'."
    ))
  }

  if (!is.null(env) && env %notin% valid_envs) {
    cli::cli_abort(c(
      "{.var env} must be a valid option",
      "i" = "Valid env{?s} {?is/are}: {cli::cli_vec(valid_envs)}.",
      "x" = "You've supplied '{env}'."
    ))
  }

  invisible()
}

#' Check the validity of a specified 'phase' string
#'
#' @param phase A character string specifying the phase. Valid options are:
#'   raw, processed and output
#' @param null_ok Whether it is valid for `phase` to be NULL.
#'
#' @return NULL (invisibly)
#' @export
#'
#' @examples
#' ensure_valid_phase("output")
#' \dontrun{
#' ensure_valid_phase("nonexistent") # => Causes an error
#' }
#'
ensure_valid_phase <- function(phase, null_ok = NULL) {

  valid_phases <- c("raw", "processed", "output")

  null_ok <- null_ok %||% TRUE

  if (!null_ok && is.null(phase)) {
    cli::cli_abort(c(
      "{.var phase} must be non-NULL",
      "i" = "Valid phase{?s} {?is/are}: {cli::cli_vec(valid_phases)}.",
      "x" = "You've supplied '{phase}'."
    ))
  }

  if (!is.null(phase) && phase %notin% valid_phases) {
    cli::cli_abort(c(
      "{.var phase} must be a valid option",
      "i" = "Valid phase{?s} {?is/are}: {cli::cli_vec(valid_phases)}.",
      "x" = "You've supplied '{phase}'."
    ))
  }

  invisible()
}

#' Retrieve any invalid keys from a config
#'
#' Identifies keys in the config which do not exist in `exp_keys`.
#'
#' @inheritParams ensure_cfg_param_types
#' @inheritParams get_user_cfg_dir
#' @inheritParams get_user_cfg_name
#' @inheritParams write_cfg_template
#'
#' @return A character vector containing any invalid keys found in the config.
#' @export
#'
#' @examples
#' get_invalid_cfg_keys()
#'
get_invalid_cfg_keys <- function(exp_keys = NULL, cfg_dir = NULL,
                                 cfg_name = NULL, cfg = NULL) {

  exp_keys <- exp_keys %||% get_cfg_template_keys()

  cfg_keys <- get_cfg_keys(cfg_dir, cfg_name, cfg)

  invalid <- list()
  if (!identical(cfg_keys, exp_keys)) {
    # Find keys in cfg that do not exist in exp_keys
    invalid <- as.list(sort(setdiff(cfg_keys, exp_keys)))
  }
  invalid
}

#' Check if a config has any invalid keys
#'
#' @inheritParams ensure_cfg_param_types
#' @inheritParams get_user_cfg_dir
#' @inheritParams get_user_cfg_name
#' @inheritParams write_cfg_template
#'
#' @return TRUE if the keys are all valid, FALSE otherwise.
#' @export
#'
#' @examples
#' cfg_keys_all_valid()
#'
cfg_keys_all_valid <- function(exp_keys = NULL, cfg_dir = NULL,
                               cfg_name = NULL, invalid_keys = NULL,
                               cfg = NULL) {

  ensure_cfg_param_types(exp_keys = exp_keys, cfg_dir = cfg_dir,
                         cfg_name = cfg_name, invalid_keys = invalid_keys,
                         cfg = cfg)

  invalid_keys <- invalid_keys %||% get_invalid_cfg_keys(
    exp_keys = exp_keys,
    cfg_dir = cfg_dir,
    cfg_name = cfg_name,
    cfg = cfg
  )

  length(invalid_keys) == 0
}

#' Validate the config
#'
#' Check that all entries in the config are valid.
#'
#' @inheritParams ensure_cfg_param_types
#' @inheritParams get_user_cfg_dir
#' @inheritParams get_user_cfg_name
#' @inheritParams write_cfg_template
#'
#' @return TRUE if the config is valid, FALSE otherwise.
#' @export
#'
#' @examples
#' # Validate the default config:
#' validate_cfg()
#' # TODO: Add example of validating a custom config
#'
validate_cfg <- function(exp_keys = NULL, cfg_dir = NULL,
                         cfg_name = NULL, cfg = NULL) {

  ensure_cfg_param_types(exp_keys = exp_keys, cfg_dir = cfg_dir,
                         cfg_name = cfg_name, cfg = cfg)

  # Initialize list to store structured error messages
  err_msgs <- list()

  # Check config keys
  invalid_keys <- get_invalid_cfg_keys(exp_keys, cfg, cfg_dir, cfg_name)

  if (!cfg_keys_all_valid(invalid_keys = invalid_keys)) {
    err_msgs <- append(err_msgs, list(
      "{.var cfg} must contain valid keys",
      "i" = "Refer to the documentation for a list of valid keys.",
      "x" = "Invalid keys found: {cli::cli_vec(invalid_keys)}.",
      "i" = "--- --- ---"
    ))
  }

  # TODO: Add remaining validations

  if (length(err_msgs) > 0) {
    # Print error messages using cli_bullets
    cli::cli_alert_danger("Config validation failed:")

    # TODO: Add a separator between groups of error messages
    # No. of error messages in each batch of messages
    err_msg_batch_len <- 4
    for (i in seq(1, length(err_msgs), by = err_msg_batch_len)) {
      cli::cli_bullets(c(
        err_msgs[[i]],
        err_msgs[[i + 1]],
        err_msgs[[i + 2]]
      ))
    }
    FALSE
  } else {
    # No errors, config is valid
    cli::cli_alert_success("Config validation succeeded.")
    TRUE
  }
}
