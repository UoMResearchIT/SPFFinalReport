# SPDX-FileCopyrightText: [2024] University of Manchester
# SPDX-License-Identifier: apache-2.0

#' Get path components from a path specification
#'
#' Split a path into its components in a platform-independent way. These
#' components can then be used for reconstructing the path on a different
#' platform.
#'
#' @param path The path from which to separate out components. This can be with
#'   unix-style or windows-style separators.
#'
#' @return A character vector of the path components from the given path.
#' @export
#'
#' @examples
#' linux_path_1 <- "/path/to-somewhere/else"
#' linux_path_2 <- "path/to somewhere/else"
#' linux_path_3 <- "/path/to somewhere/else"
#' windows_path_1 <- "C:\\Path\\to somewhere\\else"
#' windows_path_2 <- "Path\\to somewhere\\else"
#'
get_path_components <- function(path) {

  pattern <- "[/\\\\]"

  # TODO: Remove repetition from following code block
  leading <- NULL
  if (stringr::str_starts(path, "/")) {
    # This is an absolute *nix path starting from root
    leading <- ""
    if (stringr::str_length(path) < 2) {
      # In this case there are no other path components and we are done
      return(leading)
    }
    path <- stringr::str_sub(path, 2L)

  } else if (stringr::str_starts(path, "\\\\")) {
    # This is a UNC (Universal Naming Convention) path, e.g. \\theserver\share
    leading <- stringr::str_sub(path, end = 2L)
    if (stringr::str_length(path) < 3) {
      # In this case there are no other path components and we are done
      return(leading)
    }
    path <- stringr::str_sub(path, 3L)
  }

  # Split the path on file separators
  path_components <- stringr::str_split_1(path, pattern = pattern)

  if (!is.null(leading)) {
    path_components <- c(leading, path_components)
  }
  path_components
}

#' Construct a file system path from a vector of individual path components
#'
#' @param components A character vector: the path components. Note that this
#'   function calls [file.path()] as it does the same thing though with a vector
#'   of components instead of a variable number of character string arguments.
#'
#' @return The constructed path.
#' @export
#'
#' @examples
#' \dontrun{
#' # On *nix/macOS:
#' path_from_components(c("", "an", "absolute", "path"))
#' path_from_components(c("a", "relative", "path"))
#' # On Windows:
#' path_from_components(c("\\\\", "servername", "sharename"))
#' path_from_components(c("C:", "Data"))
#' }
#'
path_from_components <- function(components) {
  do.call(file.path, as.list(components))
}
