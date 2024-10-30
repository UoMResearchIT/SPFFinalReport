chk_sys_env_vars <- function(varnames) {

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
      "i" = "You need a .Renviron file for this.",
      "i" = glue::glue("(e.g. set VAR_NAME=\"<value>\" in .Renviron",
                       " for each required variable)"),
      not_found
    ))
  }
}
