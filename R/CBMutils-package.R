
#' `CBMutils`
#'
#' Utilities for modelling carbon in R and SpaDES based on the Carbon Budget Model
#' of the Canadian Forest Service v3 (CBM-CFS3).
#'
#' @keywords internal
#' @import methods
"_PACKAGE"

## usethis namespace: start
#' @useDynLib CBMutils, .registration = TRUE
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
NULL

# data.table package common variables
utils::globalVariables(c(
  ".", ":="
))

