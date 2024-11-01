# SPDX-FileCopyrightText: [2024] University of Manchester
# SPDX-License-Identifier: apache-2.0

# Convert a NULL into a string that says 'NULL'
ns <- function(x) x %||% "NULL"

#' Establish whether particular values exist in a vector.
#'
#' This is the negation of the `%in%` operator.
#'
#' @param x A vector or NULL: the values to be matched.
#' @param y A vector or NULL: the values to be checked for no match.
#'
#' @seealso [%in%()]
#'
#' @return A logical vector, indicating if a match was *not* located for each
#'   element of x (i.e., the values are TRUE or FALSE).
#' @export
#'
#' @examples
#' "a" %notin% c("b", "c")
#'
`%notin%` <- function(x, y) {
  !(x %in% y)
}

#' Check if an object is a vector
#'
#' @param x The object to check.
#' @param null_ok A boolean indicating whether a NULL value for `x` is
#'   acceptable. Default: FALSE
#'
#' @return If `null_ok` is TRUE, returns TRUE if `x` is NULL or `x` is a vector,
#'   otherwise returns FALSE; if `null_ok` is FALSE, throws an error if `x` is
#'   NULL and returns TRUE if `x` is a vector, otherwise returns FALSE.
#' @export
#'
#' @examples
#' is_strict_vec(c(10, 20, 30)) # => TRUE
#' is_strict_vec(list(10, 20, 30)) # => FALSE
#'
is_strict_vec <- function(x, null_ok = NULL) {

  null_ok <- null_ok %||% FALSE

  if (is.null(x)) {
    if (null_ok) {
      return(TRUE)
    } else {
      cli::cli_abort(c(
        "{.var x} is NULL",
        "i" = "{.var x} cannot be NULL if {.var null_ok} is FALSE",
        "x" = "{.var null_ok} is FALSE but {.var x} is NULL."
      ))
    }
  }

  # Note that is.vector(x) checks if the object x is a vector with *no
  # attributes other than names* so is not a reliable test for whether an
  # object is a vector; you could instead use 'is.atomic(x) || is.list(x)' to
  # test for any type that is a vector under the hood (which would include
  # lists, matrices, arrays, data frames etc.) but if you want to check for
  # 'strict' vector-ness:

  # Remove attributes from the object
  stripped_x <- unclass(x)
  # Check if the resulting object is a vector
  !is.list(stripped_x) && is.vector(stripped_x)
}
