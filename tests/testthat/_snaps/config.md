# get_cfg_val fails if keys not found

    Code
      get_cfg_val("non.existent", cfg = tst_cfg)
    Condition
      Error in `get_cfg_val()`:
      ! Key 'non.existent' not found in config
      i The key must correspond to hierarchical names in the config, separated by '.'
      x You've supplied key 'non.existent'

# get_root gives expected msg if sys env var missing

    Code
      get_root("main")
    Condition
      Error in `ensure_sys_env_vars()`:
      ! 1 required system environment var is not set
      i You need a '.Renviron' file for this
      i (e.g. set VAR_NAME=<value> in .Renviron for each variable).
      i Not found in system environment:
      i  1. DIMEX_STORE

# get_dat_path gives error message for invalid env

    Code
      get_dat_path(env = "nonexistent")
    Condition
      Error in `ensure_valid_env()`:
      ! `env` must be a valid option
      i Valid envs are: main, ref, and test.
      x You've supplied 'nonexistent'.

