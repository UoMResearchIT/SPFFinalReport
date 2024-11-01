# is_strict_vec shows expected msg when not(null_ok) and x is NULL

    Code
      is_strict_vec(NULL, null_ok = FALSE)
    Condition
      Error in `is_strict_vec()`:
      ! `x` is NULL
      i `x` cannot be NULL if `null_ok` is FALSE
      x `null_ok` is FALSE but `x` is NULL.

