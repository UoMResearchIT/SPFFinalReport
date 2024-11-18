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

#' Escape special characters in the given string
#'
#' Given a character string, ensure this would function as a regular expression
#' by escaping regex metacharacters which appear in the string.
#'
#' @param txt The string which should be escaped so that it can function as a
#'   regular expression.
#'
#' @return The given string with regex metacharacters escaped.
#' @export
#'
#' @examples
#' escape_txt(".") # => "\\."
#' escape_txt("ab[c]") # => "ab\\[c\\]"
#' escape_txt("$30") # => "\\$30"
#'
escape_txt <- function(txt) {
  # List of regex metacharacters that need to be escaped
  special_chars <- c(".", "\\", "|", "(", ")", "[", "]", "{", "}", "^", "$",
                     "*", "+", "?")

  # Escape each special character by prepending a backslash
  for (char in special_chars) {
    txt <- gsub(paste0("\\", char), paste0("\\\\", char), txt)
  }

  txt
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

#' Tests if a value is an integer.
#'
#' @param x The value to test.
#'
#' @return TRUE if the value is a number with no decimal value, FALSE otherwise.
#' @export
#'
#' @examples
#' is_int_val("5")     # => FALSE
#' is_int_val(10)      # => TRUE
#' is_int_val(16.5)    # => FALSE
#' is_int_val("apple") # => FALSE
#' is_int_val(-1)      # => TRUE
#'
is_int_val <- function(x) {
  if (is.numeric(x)) {
    x %% 1 == 0
  } else {
    FALSE
  }
}

#' Parse a string into a number, NA or to remain as a string
#'
#' @param x The character string to parse
#'
#' @return One of: a number, NA or the original string
#' @export
#'
#' @examples
#' parse_string("NA")
#' parse_string("notanumber")
#' parse_string("100")
#' parse_string("0.45")
#' parse_string("false")
#'
parse_string <- function(x) {

  if (x == "NA") {
    # Return NA
    NA
  } else if (x %in% c("true", "false")) {
    ifelse(x == "true", TRUE, FALSE)
  } else {
    y <- tryCatch({
      readr::parse_double(x)
    }, warning = function(w) {
    }, error = function(e) {
    })

    if (!(is.na(y) || is.null(y)) && is_int_val(y)) {
      # Return the number converted into an integer
      as.integer(y)
    } else if (is.na(y) || is.null(y)) {
      # Return the original value
      x
    } else {
      # Return the number as a double
      y
    }
  }
}

#' Recursively apply a function to each element of a nested list
#'
#' @param x The element on which to operate, which could be a list or some other
#'   type of object.
#' @param func The function to apply to each element of the list.
#'
#' @return A list which is the same as the list provided but with the given
#'   function applied to each element.
#' @export
#'
#' @examples
#' nested <- list(
#'   a = "1",
#'   b = "12.5",
#'   c = "NA",
#'   d = "alphabet",
#'   e = list(
#'     first = "NA",
#'     second = "notanumber",
#'     third = "100",
#'     fourth = "0.45",
#'     fifth = list(
#'       one = "teal",
#'       two = "NA",
#'       three = "999"
#'     )
#'   )
#' )
#' recurse_nested(nested, parse_string)
#'
recurse_nested <- function(x, func) {
  if (is.list(x)) {
    # If x is a list, recursively apply the function to each element
    purrr::map(x, ~ recurse_nested(.x, func))
  } else {
    # Otherwise, apply f directly to x
    func(x)
  }
}
