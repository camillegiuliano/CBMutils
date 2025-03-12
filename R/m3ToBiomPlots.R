utils::globalVariables(c(
  "..id_col", "valueNumeric"
))

#' Plot all columns that are not id_col
#'
#' @param inc DESCRIPTION NEEDED
#' @param id_col DESCRIPTION NEEDED
#' @param nrow DESCRIPTION NEEDED
#' @param ncol DESCRIPTION NEEDED
#' @param filenameBase DESCRIPTION NEEDED
#' @param path DESCRIPTION NEEDED
#' @param title DESCRIPTION NEEDED
#' @param scales DESCRIPTION NEEDED
#'
#' @export
#' @importFrom data.table copy melt
#' @importFrom ggforce facet_wrap_paginate
#' @importFrom ggplot2 aes element_text facet_wrap geom_line ggplot ggsave labs theme_bw
#' @importFrom reproducible checkPath
#'
m3ToBiomPlots <- function(inc = "increments", id_col = "gcids", nrow = 5, ncol = 5,
                          filenameBase = "rawCumBiomass_", path = NULL,
                          title = "Cumulative merch fol other by gc id",
                          scales = "free_y") {
  if (is.null(path)) {
    stop("a valid 'path' must be specified.")
  }

  gInc <- copy(inc)
  gc <- data.table::melt(gInc, id.vars = c(id_col, "age"))
  ##TODO - make sure this warning is not a problem
  # Warning messages: 1: 'measure.vars' [id, totMerch, fol, other, ...] are not
  # all of the same type. By order of hierarchy, the molten data value column
  # will be of type 'double'. All measure variables not of type 'double' will be
  # coerced too. Check DETAILS in ?melt.data.table for more on coercion.
  gc[is.na(value), "value"] <- 0
  set(gc, NULL, "valueNumeric", as.numeric(gc$value))

  idSim <- unique(gc[, ..id_col])[[1]]
  plots <- gc |> # [id_ecozone %in% idSim[1:20]] |>
    ggplot(aes(x = age, y = valueNumeric, group = variable, color = variable)) +
    theme_bw() +
    geom_line() +
    facet_wrap(facets = id_col) +
    labs(title = title) +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(file.path(path, paste0(filenameBase, ".png")), plots)

  # Do first page, so that n_pages can calculate how many pages there are
  #   -- not used -- so a bit wasteful
  # numPages <- ceiling(length(idSim) / (nrow * ncol))
  #
  # path <- checkPath(path, create = TRUE)
  # for (i in seq(numPages)) {
  #   plotsByPage <- plots + facet_wrap_paginate(facets = id_col, scales = scales,
  #                                              page = i, nrow = nrow, ncol = ncol)
  #   ggsave(file.path(path, paste0(filenameBase, i, ".png")), plotsByPage)
  # }
  return(plots)
}

