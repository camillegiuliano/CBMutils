#' Suggest modules
#'
#' @param sim DESCRIPTION NEEDED
#' @param suggestedModules DESCRIPTION NEEDED
#'
#' @return DESCRIPTION NEEDED
#'
#' @export
#' @importFrom SpaDES.core currentModule inputObjects
suggestModules <- function(sim, suggestedModules) {
  io <- inputObjects(sim, currentModule(sim))
  objectNamesExpected <- io$objectName
  available <- objectNamesExpected %in% ls(sim)
  if (any(!available))
    stop("The inputObjects for ", currentModule(sim), " are not all available:",
         "These are missing:", paste(objectNamesExpected[!available], collapse = ", "),
         ". \n\nHave you run ",  suggestedModules, "?")
} ## TODO: this will end up in SpaDES.core, so we need to use that version when available
