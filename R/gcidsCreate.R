#' Standardized way to create gcids
#'
#' This uses the underscore delimiter between column names.
#'
#' @param ... The data.table with ONLY the columns on which to determine unique gcids
#' @export
#'
gcidsCreate <- function(...) {
  do.call(paste, c(list(...)[[1]], sep= "_"))
}
