utils::globalVariables(c("name", "noLoss", "row", "value"))

#' Make a `data.table` out of the carbon transfer matrices
#'
#' @param matricesIn DESCRIPTION NEEDED
#' @param indicesIn DESCRIPTION NEEDED
#'
#' @return a `data.table` object summarizing the carbon transfers
#'
#' @export
#' @importFrom data.table rbindlist
matrixDT <- function(matricesIn,  indicesIn) {
  matListDT <- lapply(matricesIn,as.data.table)
  matDT <- rbindlist(matListDT)
  matDTnrow <- lapply(matricesIn,nrow)
  matNrow <- do.call("rbind",matDTnrow)
  nameVec <- NULL
  for (i in 1:length(indicesIn)) {
    thisIndex <- rep(indicesIn[i], times = matNrow[i])
    nameVec <- c(nameVec,thisIndex)
  }
  matDT$name  <-  nameVec
  return(matDT)
}

#' check that the proportions add up to 1
#'
#' @param DTofMatrices DESCRIPTION NEEDED
#' @param tarCols DESCRIPTION NEEEDED
#'
#' @return DESCRIPTION NEEDED
#'
#' @export
checkProp <- function(DTofMatrices, tarCols) {
  cols <- c("row", "name")
  checkProp <- DTofMatrices[, noLoss := sum(value), by = cols][, .(name, row, noLoss)]
  return(checkProp)
}
