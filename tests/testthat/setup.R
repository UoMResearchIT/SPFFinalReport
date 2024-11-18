# ---------------------------------------------------------------------------- #
# Temporarily silence cli output in tests

# See
# Tuesday, February 6, 2024
# Please Shut Up! Verbosity Control in Packages
# By Mark Padgham, Maëlle Salmon
# https://ropensci.org/blog/2024/02/06/verbosity-control-packages/
# [15nov24]

op <- options(list(rlib_message_verbosity = "quiet"))

withr::defer(options(op), teardown_env())

# ---------------------------------------------------------------------------- #

if (env_should_run_any_hl_data_tests()) {
  # Load 'main' population data
  pop_dat <- get_pop_dat(env = "main", cfg = NULL)
}

# ---------------------------------------------------------------------------- #
