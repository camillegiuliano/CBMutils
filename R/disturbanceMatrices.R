utils::globalVariables(c("actProp", "noLoss2"))

#' Load disturbance matrix IDs
#'
#' @param disturbanceMatrixValues DESCRIPTION NEEDED
#' @param dbPools DESCRIPTION NEEDED
#'
#' @return DESCRIPTION NEEDED
#' @export
loadDisturbanceMatrixIds <- function(disturbanceMatrixValues, dbPools) {
  ids <- unique(disturbanceMatrixValues[, "disturbance_matrix_id"])

  # matches the pool def id by name for safety
  getPoolDefId <- function(dbPoolID) {
    dbPoolName <- dbPools[as.numeric(dbPools[, "id"]) == dbPoolID, "name"]
    get(dbPoolName)
  }

  # fill in the neutral transfers not covered by the matrix data
  # i.e., the matrix will not withdraw from any of the following pools
  neutrals <- NULL
  neutrals <- rbind(neutrals, c(Input, Input, 1))
  neutrals <- rbind(neutrals, c(CO2, CO2, 1))
  neutrals <- rbind(neutrals, c(CH4, CH4, 1))
  neutrals <- rbind(neutrals, c(CO, CO, 1))
  neutrals <- rbind(neutrals, c(Products, Products, 1))

  loadMatrix <- function(dmid) {
    dbmat <- disturbanceMatrixValues[disturbanceMatrixValues[, "disturbance_matrix_id"] == dmid, ]
    mat <- matrix(0, ncol = 3, nrow = nrow(dbmat))

    for (i in 1:nrow(dbmat)) {
      mat[i, ] <- c(
        getPoolDefId(dbmat[i, "source_pool_id"]),
        getPoolDefId(dbmat[i, "sink_pool_id"]),
        dbmat[i, "proportion"]
      )
    }

    mat <- rbind(mat, neutrals)

    return(mat)
  }
  ## ?? not sure is this is needed in the sim$ since it is not openly used anywhere else
  allMatrices <- NULL
  # return the matrix ids of the loaded matrices
  for (x in 1:length(ids)) {
    dm <- loadMatrix(ids[x])
    dm <- cbind(rep(ids[x], nrow(dm)), dm)
    allMatrices <- rbind(allMatrices, dm)
  }

  colnames(allMatrices) <- c("id", "row", "col", "value")
  allMats <- as.data.table(allMatrices)
  cols <- c("row","id")
  allMats[, noLoss := sum(value), by = cols]
  allMats[, actProp := value/noLoss]
  allMats[,noLoss2 := sum(actProp), by = cols]
  cols <- c("value", "actProp")
  allMats[, (cols) := list((actProp),NULL)]
  cols <- c("noLoss", "noLoss2")
  allMats[, (cols) := NULL]
  fixedDistMatrices <- as.matrix(allMats)
  return(fixedDistMatrices)
}
