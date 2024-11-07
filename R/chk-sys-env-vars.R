# SPDX-FileCopyrightText: [2024] University of Manchester
# SPDX-License-Identifier: apache-2.0

#' Ensure the presence of required system environment variables
#'
#' Check for presence of required system environment variables and abort if they
#' are not found.
#'
#' @param varnames A character vector of variable names to check for in the
#'   user's environment.
#'
#' @return NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' ensure_sys_env_vars()
#' }
#'
ensure_sys_env_vars <- function(varnames = NULL) {

  varnames <- varnames %||% unname(get_req_sys_env_vars())

  not_found <- lapply(varnames, function(varname) {

    env_var <- Sys.getenv(varname, unset = NA)

    if (is.na(env_var)) {
      varname
    } else {
      c()
    }
  })

  not_found <- unlist(not_found)

  n_not_found <- length(not_found)

  if (n_not_found > 0) {

    not_found <- paste0(" ", seq_along(not_found), ". ", not_found)
    names(not_found) <- rep("i", length(not_found))

    cli::cli_abort(c(
      "{n_not_found} required system environment var{?s} {?is/are } not set",
      "i" = "You need a '.Renviron' file for this",
      "i" = "(e.g. set VAR_NAME=<value> in .Renviron for each variable).",
      "i" = "Not found in system environment:",
      not_found
    ))
  }

  invisible()
}
