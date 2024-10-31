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
