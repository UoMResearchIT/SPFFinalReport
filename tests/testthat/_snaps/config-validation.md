# cfg_keys_all_valid fails if cfg is a character vecor not a list

    Code
      cfg_keys_all_valid(cfg = c("a", "b", "c"))
    Condition
      Error in `ensure_cfg_param_types()`:
      ! `cfg` is an unexpected type
      i `cfg` must be a list.
      x You've supplied an object of class 'character'.

