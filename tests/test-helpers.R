# Test helpers

env_should_run_test <- function(env_var_nm = NULL) {
  env_var_nm <- env_var_nm %||% ""
  # Read system env vars to determine whether to run the specified data test
  run_all <- Sys.getenv("SPF_RUN_ALL_HLDT", unset = "false") == "true"
  run_all || (Sys.getenv(env_var_nm, unset = "false") == "true")
}
