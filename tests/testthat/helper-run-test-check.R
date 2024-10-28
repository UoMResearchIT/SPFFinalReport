# SPDX-FileCopyrightText: [2024] University of Manchester
# SPDX-License-Identifier: apache-2.0

# Test helpers

is_r_cmd_check <- function() {
  # _R_CHECK_PACKAGE_NAME_ should only be set during package checking
  nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_"))
}

# is_not_cran <- function() {
#   nzchar(Sys.getenv("NOT_CRAN"))
# }

env_should_run_hl_data_test <- function(env_var_nm = NULL) {
  env_var_nm <- env_var_nm %||% ""
  # Read system env vars to determine whether to run the specified test
  run_all <- (Sys.getenv("DIMEX_RUN_ALL_HLDT", unset = "false") == "true")
  run_all || (Sys.getenv(env_var_nm, unset = "false") == "true")
}

skip_if_r_cmd_check_or_not_configured <- function(env_var_id = NULL) {
  if (is_r_cmd_check()) {
    skip(glue::glue("Skipping for R CMD CHECK: {env_var_id}"))

  } else if (!env_should_run_hl_data_test(env_var_id)) {
    skip(glue::glue("Configured not to run: {env_var_id} not 'true'"))

  } else {
    invisible()
  }
}
