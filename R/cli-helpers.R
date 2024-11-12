# SPDX-FileCopyrightText: [2024] University of Manchester
# SPDX-License-Identifier: apache-2.0

#' Print a banner to the console ahead of running a step in the workflow
#'
#' @param id A character string for the step id.
#' @param title A character string which is the title of the step.
#' @param pad_width The width to which to pad the title with blanks, unless it
#'   is NA in which case no padding will be done. Default: NA
#'
#' @return NULL (invisibly)
#' @export
#'
#' @examples
#' show_workflow_banner("1", "Step 1")
#' show_workflow_banner("1", "Step 1", 15)
#'
show_workflow_banner <- function(id, title, pad_width = NULL) {

  pad_width <- pad_width %||% NA

  suffix <- "..."
  title <- glue::glue("{title} {suffix}")
  if (!is.na(pad_width)) {
    pad_width <- pad_width + stringr::str_length(suffix)
    title <- stringr::str_pad(title, pad_width, "right")
  }

  cli::cat_rule()
  cli::cat_line(glue::glue("STEP {id}"), col = "#f2404f")
  cli::cat_boxx(glue::glue("-> {title}"))

  invisible()
}

#' Print an informational 'separator' rule with a preceding blank line
#'
#' @param title The title to print for the rule, or NULL for none. Default: NULL
#' @param width The rule width, or NULL for half the console width. Default:
#'   NULL
#'
#' @return The value returned by [cli::cat_rule()]
#' @export
#'
#' @examples
#' cat_rule_sep()
#' cat_rule_sep("An informational message")
#' cat_rule_sep("An informational message", width = cli::console_width())
#'
cat_rule_sep <- function(title = NULL, width = NULL) {
  title <- title %||% ""
  width <- width %||% floor(cli::console_width() / 2)
  cli::cat_line("")
  cli::cat_rule(title, width = width)
}

#' Print an informational message with a preceding blank line
#'
#' @param msg The message to print.
#'
#' @return The value returned by [cli::cli_inform()]
#' @export
#'
#' @examples
#' cat_info_sep("Doing something")
#'
cat_info_sep <- function(msg) {
  cli::cat_line("")
  cli::cli_inform(msg)
}
