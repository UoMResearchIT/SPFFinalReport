# loading nonexistent sys env var shows correct msg

    Code
      ensure_sys_env_vars("DIMEX_TST_VAR1")
    Condition
      Error in `ensure_sys_env_vars()`:
      ! 1 required system environment var is not set
      i You need a '.Renviron' file for this
      i (e.g. set VAR_NAME=<value> in .Renviron for each variable).
      i Not found in system environment:
      i  1. DIMEX_TST_VAR1

# loading 2 sys env vars where 1 is nonexistent shows correct msg

    Code
      ensure_sys_env_vars(sys_env_vars)
    Condition
      Error in `ensure_sys_env_vars()`:
      ! 2 required system environment vars are not set
      i You need a '.Renviron' file for this
      i (e.g. set VAR_NAME=<value> in .Renviron for each variable).
      i Not found in system environment:
      i  1. DIMEX_TST_VAR1
      i  2. DIMEX_TST_VAR2

# loading 3 sys env vars where 2 are nonexistent shows correct msg

    Code
      ensure_sys_env_vars(sys_env_vars)
    Condition
      Error in `ensure_sys_env_vars()`:
      ! 2 required system environment vars are not set
      i You need a '.Renviron' file for this
      i (e.g. set VAR_NAME=<value> in .Renviron for each variable).
      i Not found in system environment:
      i  1. DIMEX_TST_VAR1
      i  2. DIMEX_TST_VAR2

