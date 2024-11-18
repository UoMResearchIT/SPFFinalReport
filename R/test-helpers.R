# SPDX-FileCopyrightText: [2024] University of Manchester
# SPDX-License-Identifier: apache-2.0

# ---------------------------------------------------------------------------- #
# Test helpers
# These are located here in the R/ directory so that they are available for
# interactive testing.

#' Check if R CMD CHECK is currently running
#'
#' @return A boolean: TRUE if the system environment variable
#'   '_R_CHECK_PACKAGE_NAME_' is non-empty (which should indicate that
#'   'R CMD CHECK' is currently running), otherwise FALSE.
#'
#' @examples
#' \dontrun{
#' is_r_cmd_check()
#' }
#'
is_r_cmd_check <- function() {
  # _R_CHECK_PACKAGE_NAME_ should only be set during package checking
  # Note: Another potentially useful env var to check is
  # 'Sys.getenv("NOT_CRAN")'
  nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_"))
}

#' Skip a test on certain conditions
#'
#' Skip a test if not running within the context of 'R CMD CHECK'  or not
#' configured to run, as determined by the value of the supplied system
#' environment variable.
#'
#' @param env_var_id The system environment variable that specified whether or
#'   not to run the corresponding test.
#'
#' @return NULL (invisibly)
#'
#' @examples
#' \dontrun{
#' skip_if_r_cmd_check_or_not_configured()
#' Sys.setenv(DIMEX_RUN_HLDT_1D = "false")
#' skip_if_r_cmd_check_or_not_configured("DIMEX_RUN_HLDT_1D")
#' }
#'
skip_if_r_cmd_check_or_not_configured <- function(env_var_id = NULL) {
  if (is_r_cmd_check()) {
    testthat::skip(glue::glue("Skipping for R CMD CHECK: {env_var_id}"))

  } else if (!env_should_run_hl_data_test(env_var_id)) {
    testthat::skip(glue::glue("Configured not to run: {env_var_id} not 'true'"))

  } else {
    invisible()
  }
}

#' Check whether to run a particular high-level data test
#'
#' @inheritParams skip_if_r_cmd_check_or_not_configured
#'
#' @return A boolean: TRUE if either the specified test is configured to run or
#'   all high-level data tests are configured to run, otherwise FALSE.
#'
#' @examples
#' \dontrun{
#' env_should_run_hl_data_test()
#' env_should_run_hl_data_test("DIMEX_RUN_HLDT_1D")
#' }
#'
env_should_run_hl_data_test <- function(env_var_id = NULL) {
  env_var_id <- env_var_id %||% ""
  # Read system env vars to determine whether to run the specified test
  run_all <- (Sys.getenv("DIMEX_RUN_ALL_HLDT", unset = "false") == "true")
  run_all || (Sys.getenv(env_var_id, unset = "false") == "true")
}

get_hldt_env_vars <- function() {
  c(
    "DIMEX_RUN_HLDT_1A",
    "DIMEX_RUN_HLDT_1B",
    "DIMEX_RUN_HLDT_1C",
    "DIMEX_RUN_HLDT_1D",
    "DIMEX_RUN_HLDT_1E",
    "DIMEX_RUN_HLDT_1F",

    "DIMEX_RUN_HLDT_2",

    "DIMEX_RUN_HLDT_3A",
    "DIMEX_RUN_HLDT_3B",

    "DIMEX_RUN_HLDT_4A",
    "DIMEX_RUN_HLDT_4B",

    "DIMEX_RUN_ALL_HLDT"
  )
}

env_should_run_any_hl_data_tests <- function() {
  should_run <- FALSE
  for (env_var in get_hldt_env_vars()) {
    if (should_run) {
      return(TRUE)
    }
    should_run <- should_run || (Sys.getenv(env_var, unset = "false") == "true")
  }
  should_run
}

#' Test helper to create a test config
#'
#' @return A list which conforms to the package config structure
#'
#' @examples
#' \dontrun{
#' get_test_cfg()
#' }
#'
get_test_cfg <- function() {

  # Define package configuration as a list
  cfg = list(

    top = list(

      colours = list(
        main = "mazarine",

        sub = list (
          sub1 = "periwinkle",
          sub2 = "zaffre"
        )
      ),

      fruit = list(
        yellow = "banana",
        red = "apple",
        green = "grape"
      ),

      shapes = list(
        oblong = "rectangle"
      ),

      locations = list(
        base_path = file.path("basedir", "subdir", "level3dir"),
        other_path = file.path("jump", "skip")
      )
    )
  )

  cfg
}

#' Test helper to return expected keys for the test config
#'
#' @return A character vector: expected keys for the test config
#'
#' @examples
#' \dontrun{
#' get_test_cfg_exp_keys()
#' }
#'
get_test_cfg_exp_keys <- function() {
  c(
    "top",
    "top.fruit",
    "top.fruit.yellow",
    "top.fruit.red",
    "top.fruit.green",
    "top.colours",
    "top.colours.main",
    "top.colours.sub",
    "top.colours.sub.sub1",
    "top.colours.sub.sub2",
    "top.shapes",
    "top.shapes.oblong",
    "top.locations",
    "top.locations.base_path",
    "top.locations.other_path"
  )
}

#' Test helper to temporarily set sys env vars
#'
#' @param vars A named character vector with names corresponding to vars that
#'   should be temporarily changed with the values they should be changed to.
#' @param envir The environment to use for scoping which must default to
#'   [parent.frame()] - do NOT changed this by supplying an alternative as this
#'   is used by [withr::defer()].
#'
#' @return NULL (invisibly)
#'
#' @details
#' A call to `local_sys_env_vars()` will alter the value of the specified system
#'   environment variables and set up a deferred event to set them back to their
#'   original values when the calling function exits (i.e., within the
#'   appropriate scope as controlled by the `envir` argument).
#'
#' @references [Test fixtures in testthat](https://testthat.r-lib.org/articles/test-fixtures.html)
#'
#' @examples
#' \dontrun{
#' # For interactive use:
#' local_sys_env_vars(c(DIMEX_STORE = "xxx", DIMEX_STORE_REF = "yyy"))
#' # Run the deferred events to set the sys env vars back to their original
#' # values:
#' withr::deferred_run()
#' # Note: Use `withr::deferred_clear()` to clear the events without running
#' }
#'
local_sys_env_vars <- function(vars, envir = parent.frame()) {

  dimex_store_val <- Sys.getenv("DIMEX_STORE")
  dimex_store_ref_val <- Sys.getenv("DIMEX_STORE_REF")

  lapply(names(vars), function(var_nm) {
    switch (var_nm,
            DIMEX_STORE = Sys.setenv(DIMEX_STORE = vars[var_nm]),
            DIMEX_STORE_REF = Sys.setenv(DIMEX_STORE_REF = vars[var_nm]),

            DIMEX_TST_VAR1 = Sys.setenv(DIMEX_TST_VAR1 = vars[var_nm]),
            DIMEX_TST_VAR2 = Sys.setenv(DIMEX_TST_VAR2 = vars[var_nm]),
    )
  })

  withr::defer({
    Sys.setenv(DIMEX_STORE = dimex_store_val)
    Sys.setenv(DIMEX_STORE_REF = dimex_store_ref_val)

    Sys.unsetenv("DIMEX_TST_VAR1")
    Sys.unsetenv("DIMEX_TST_VAR2")
  }, envir = envir)

  invisible()
}

#' Test helper to create a temporary directory
#'
#' @param dir A temporary directory as a character string or as would be
#'   returned by [fs::file_temp()].
#' @param envir The environment to use for scoping which must default to
#'   [parent.frame()] - do NOT changed this by supplying an alternative as this
#'   is used by [withr::defer()].
#'
#' @return The name of the temporary directory as a character string (not as an
#'   `fs_path` object).
#'
#' @references [Test fixtures in testthat](https://testthat.r-lib.org/articles/test-fixtures.html)
#'
#' @examples
#' \dontrun{
#' tmp_dir <- local_mk_dir()
#' }
#'
local_mk_dir <- function(dir = fs::file_temp(pattern = "R-dimex-tst-dir_"),
                         envir = parent.frame()) {

  # Write config to file in temp directory
  cfg_dir <- as.character(dir)
  fs::dir_create(dir)

  # Clean up afterwards
  withr::defer(fs::dir_delete(dir), envir = envir)

  # Return the name of the temp directory (*not* the fs_path object)
  cfg_dir
}

#' Test helper to write a test config to file
#'
#' @inheritParams local_mk_dir
#' @param cfg_name A character string: name of the config file to write.
#'
#' @return The name of the temporary directory that contains the saved config
#'   file as a character string (not as an `fs_path` object).
#'
#' @details
#' A call to `local_write_cfg()` will write a config to file to a temporary
#'   directory and set up a deferred event to delete the directory (and file)
#'   when the calling function exits (i.e., within the appropriate scope as
#'   controlled by the `envir` argument).
#'
#' @references [Test fixtures in testthat](https://testthat.r-lib.org/articles/test-fixtures.html)
#'
#' @examples
#' \dontrun{
#' tmp_dir <- local_write_cfg()
#' }
#'
local_write_cfg <- function(cfg_name = "tmp_cfg.yml",
                            dir = fs::file_temp(pattern = "R-dimex-tst-cfg_"),
                            envir = parent.frame()) {

  # Write config to file in temp directory
  cfg_dir <- as.character(dir)
  write_user_cfg(cfg = get_test_cfg(), cfg_dir = cfg_dir, cfg_name = cfg_name,
                 overwrite = TRUE)

  # Clean up afterwards
  withr::defer(fs::dir_delete(dir), envir = envir)

  # Return the name of the temp directory (*not* the fs_path object)
  cfg_dir
}
