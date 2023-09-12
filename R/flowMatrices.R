utils::globalVariables(c(
  "calcDist", "fluxOut", "noLoss"
))

#' Calculate the decay rate based on mean annual temperature and other parameters
#'
#' This is the ecological theory for decomposing being used in CBM.
#'
#' @param meanAnnualTemp scalar temperature in degrees Celsius
#' @param baseDecayRate scalar base decay rate constant
#' @param q10 the scalar q10 value
#' @param tref the reference temperature
#' @param max the maximum allowed decay rate
#'
#' @return the scalar rate of decay
#'
#' @export
decayRate <- function(meanAnnualTemp, baseDecayRate, q10, tref, max) {
  min(baseDecayRate * exp((meanAnnualTemp - tref) * log(q10) * 0.1), max)
}

#' Get decay rates
#'
#' Returns a vector of decay rates where the indices of the vector are the DOM pools of CBM.
#'
#' @param meanAnnualTemp scalar temperature in deg Celcius
#' @param decayParameters table of decay parameters for calculating the temperature dependent decay rate
#' @param domPools DESCRIPTION NEEDED
#'
#' @return the vector of decay rates
#'
#' @export
getDecayRates <- function(meanAnnualTemp, decayParameters, domPools) {
  decayRateOfNum <- function(decayParameter) {
    decayRate(
      meanAnnualTemp,
      decayParameter["OrganicMatterDecayRate"],
      decayParameter["Q10"],
      decayParameter["ReferenceTemp"],
      decayParameter["MaxDecayRate"]
    )
  }
  result <- apply(decayParameters, 1, decayRateOfNum)
  names(result) <- domPools[, "name"]
  return(result)
}

#' Spatial unit decay rates
#'
#' @param climate DESCRIPTION NEEDED
#' @param decayparameters DESCRIPTION NEEDED
#' @param domPools DESCRIPTION NEEDED
#'
#' @return DESCRIPTION NEEDED
#'
#' @export
spatialUnitDecayRates <- function(climate, decayparameters, domPools) {
  decayRates <- t(apply(
    as.matrix(climate[, "MeanAnnualTemperature"]), 1,
    function(t) getDecayRates(t, decayparameters, domPools)
  ))
  decayRates <- cbind(climate[, "SpatialUnitID"], decayRates)
  colnames(decayRates)[1] <- "SpatialUnitID"
  return(decayRates)
}

#' Calculate a portion of the DOM decay matrix
#'
#' @param mat the datatable
#' @param decayRates vector of annual decay rates by dom pool
#' @param propToAtmosphere vector of the proportions of decay emitted to the atmosphere as CO2 by this process
#' @param src the integer code for the dom pool being decayed
#' @param dst the integer code for the dom pool receiving non-emitted decayed matter
#' @param emission the integer code for the CO2 pool
#'
#' @return A modified copy of the input `mat`
#'
#' @export
domDecayMatrixItem <- function(mat, decayRates, propToAtmosphere, src, dst, emission) {
  offset <- .pooldefids[["HardwoodFineRoots"]]
  mat <- rbind(mat, c(src, src, 1 - decayRates[src - offset]))
  mat <- rbind(mat, c(src, dst, decayRates[src - offset] * (1 - propToAtmosphere[src - offset])))
  mat <- rbind(mat, c(src, emission, decayRates[src - offset] * propToAtmosphere[src - offset]))
  return(mat)
}

#' Compute a single dom decay matrix based on the specified table of decay rates
#'
#' @param decayRates vector of decay rates (each element represents a dom pool)
#' @param decayParameters table of cbm decay parameters
#' @param PoolCount DESCRIPTION NEEDED
#'
#' @return DESCRIPTION NEEDED
#'
#' @export
domDecayMatrix <- function(decayRates, decayParameters, PoolCount) {
  mat <- getIdentityCoordinateMatrix(PoolCount)
  propToAtmosphere <- decayParameters[, "PropToAtmosphere"]
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere,
                            .pooldefids[["AboveGroundVeryFastSoil"]],
                            .pooldefids[["AboveGroundSlowSoil"]],
                            .pooldefids[["CO2"]])
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere,
                            .pooldefids[["BelowGroundVeryFastSoil"]],
                            .pooldefids[["BelowGroundSlowSoil"]],
                            .pooldefids[["CO2"]])
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere,
                            .pooldefids[["AboveGroundFastSoil"]],
                            .pooldefids[["AboveGroundSlowSoil"]],
                            .pooldefids[["CO2"]])
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere,
                            .pooldefids[["BelowGroundFastSoil"]],
                            .pooldefids[["BelowGroundSlowSoil"]],
                            .pooldefids[["CO2"]])
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere,
                            .pooldefids[["MediumSoil"]],
                            .pooldefids[["AboveGroundSlowSoil"]],
                            .pooldefids[["CO2"]])
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere,
                            .pooldefids[["SoftwoodStemSnag"]],
                            .pooldefids[["AboveGroundSlowSoil"]],
                            .pooldefids[["CO2"]])
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere,
                            .pooldefids[["SoftwoodBranchSnag"]],
                            .pooldefids[["AboveGroundSlowSoil"]],
                            .pooldefids[["CO2"]])
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere,
                            .pooldefids[["HardwoodStemSnag"]],
                            .pooldefids[["AboveGroundSlowSoil"]],
                            .pooldefids[["CO2"]])
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere,
                            .pooldefids[["HardwoodBranchSnag"]],
                            .pooldefids[["AboveGroundSlowSoil"]],
                            .pooldefids[["CO2"]])

  return(mat)
}

#' Compute all dom decay matrices in coordinate matrix format
#'
#' The first column in the specified `decayRates` parameter acts as the key to each matrix.
#'
#' @param decayRates matrix of decay rates column 1 is the key for the values
#'                   in columns `1:n` and columns `1:n` are the DOM pool specific decay rates.
#' @param decayParameters table of CBM decay parameters
#' @param PoolCount DESCRIPTION NEEDED
#'
#' @return DESCRIPTION NEEDED
#'
#' @export
computeDomDecayMatrices <- function(decayRates, decayParameters, PoolCount) {
  matrices <- NULL
  for (x in 1:nrow(decayRates)) {
    mat <- domDecayMatrix(decayRates[x, -1], decayParameters, PoolCount)
    mat <- cbind(decayRates[x, 1], mat)
    matrices <- rbind(matrices, mat)
  }

  colnames(matrices) <- c("id", "row", "col", "value")

  # this is the modification to prevent the doubling of proportions in the
  # proportional transaction matrices
  dMat <- as.data.table(matrices)
  cols <- c("row","id")
  dMat[,noLoss := sum(value), by = cols]
  tryVec <- c(.pooldefids[["AboveGroundVeryFastSoil"]], .pooldefids[["BelowGroundVeryFastSoil"]],
              .pooldefids[["AboveGroundFastSoil"]], .pooldefids[["BelowGroundFastSoil"]],
              .pooldefids[["MediumSoil"]],
              .pooldefids[["AboveGroundSlowSoil"]], .pooldefids[["BelowGroundSlowSoil"]],
              .pooldefids[["SoftwoodStemSnag"]], .pooldefids[["SoftwoodBranchSnag"]],
              .pooldefids[["HardwoodStemSnag"]], .pooldefids[["HardwoodBranchSnag"]])
  dMat2 <- dMat[!(row %in% tryVec & value == 1),]
  dMat2[,noLoss := NULL]
  dMat3 <- as.matrix(dMat2)

  return(dMat3)
}

#' Slow decay matrix
#'
#' @param decayRates DESCRIPTION NEEDED
#' @param decayParameters DESCRIPTION NEEDED
#' @param PoolCount DESCRIPTION NEEDED
#'
#' @return DESCRIPTION NEEDED
#'
#' @export
slowDecayMatrix <- function(decayRates, decayParameters, PoolCount) {
  offset <- .pooldefids[["HardwoodFineRoots"]]
  mat <- getIdentityCoordinateMatrix(PoolCount)
  propToAtmosphere <- decayParameters[, "PropToAtmosphere"]
  mat <- rbind(mat, c(.pooldefids[["AboveGroundSlowSoil"]], .pooldefids[["AboveGroundSlowSoil"]],
                      1 - decayRates[.pooldefids[["AboveGroundSlowSoil"]] - offset]))
  mat <- rbind(mat, c(.pooldefids[["AboveGroundSlowSoil"]], .pooldefids[["CO2"]],
                      decayRates[.pooldefids[["AboveGroundSlowSoil"]] - offset] *
                        propToAtmosphere[.pooldefids[["AboveGroundSlowSoil"]] - offset]))
  mat <- rbind(mat, c(.pooldefids[["BelowGroundSlowSoil"]], .pooldefids[["BelowGroundSlowSoil"]],
                      1 - decayRates[.pooldefids[["BelowGroundSlowSoil"]] - offset]))
  mat <- rbind(mat, c(.pooldefids[["BelowGroundSlowSoil"]], .pooldefids[["CO2"]],
                      decayRates[.pooldefids[["BelowGroundSlowSoil"]] - offset] *
                        propToAtmosphere[.pooldefids[["AboveGroundSlowSoil"]] - offset]))
  return(mat)
}

#' Compute slow decay matrices
#'
#' @param decayRates DESCRIPTION NEEDED
#' @param decayParameters DESCRIPTION NEEDED
#' @param PoolCount DESCRIPTION NEEDED
#'
#' @return DESCRIPTION NEEDED
#'
#' @export
computeSlowDecayMatrices <- function(decayRates, decayParameters, PoolCount) {
  matrices <- NULL
  for (x in 1:nrow(decayRates)) {
    mat <- slowDecayMatrix(decayRates[x, -1], decayParameters, PoolCount)
    mat <- cbind(decayRates[x, 1], mat)
    matrices <- rbind(matrices, mat)
  }

  colnames(matrices) <- c("id", "row", "col", "value")
  dMat <- as.data.table(matrices)
  cols <- c("row","id")
  dMat[,noLoss := sum(value), by = cols]
  tryVec <- c(.pooldefids[["AboveGroundSlowSoil"]], .pooldefids[["BelowGroundSlowSoil"]])
  dMat2 <- dMat[!(row %in% tryVec & value == 1),]
  dMat2[,noLoss := NULL]
  dMat3 <- as.matrix(dMat2)
  return(dMat3)
}

#' Compute slow mixing matrix
#'
#' @param slowMixingRate DESCRIPTION NEEDED
#' @param PoolCount DESCRIPTION NEEDED
#'
#' @return DESCRIPTION NEEDED
#'
#' @export
computeSlowMixingMatrix <- function(slowMixingRate, PoolCount) {
  mat <- getIdentityCoordinateMatrix(PoolCount)
  mat <- mat[-.pooldefids[["AboveGroundSlowSoil"]],]
  mat <- rbind(mat, c(.pooldefids[["AboveGroundSlowSoil"]], .pooldefids[["BelowGroundSlowSoil"]], slowMixingRate))
  mat <- rbind(mat, c(.pooldefids[["AboveGroundSlowSoil"]], .pooldefids[["AboveGroundSlowSoil"]], 1 - slowMixingRate))
  mat <- cbind(rep(1, nrow(mat)), mat)
  colnames(mat) <- c("id", "row", "col", "value")
  return(mat)
}

#' DOM turnover matrix
#'
#' @param turnoverParam  DESCRIPTION NEEDED
#' @param PoolCount  DESCRIPTION NEEDED
#'
#' @return DESCRIPTION NEEDED
#'
#' @export
domTurnOverMatrix <- function(turnoverParam, PoolCount) {
  mat <- getIdentityCoordinateMatrix(PoolCount)

  mat <- rbind(mat, c(.pooldefids[["SoftwoodStemSnag"]], .pooldefids[["SoftwoodStemSnag"]],
                      1 - turnoverParam["StemSnagTurnoverRate"]))
  mat <- rbind(mat, c(.pooldefids[["SoftwoodStemSnag"]], .pooldefids[["MediumSoil"]],
                      turnoverParam["StemSnagTurnoverRate"]))

  mat <- rbind(mat, c(.pooldefids[["SoftwoodBranchSnag"]], .pooldefids[["SoftwoodBranchSnag"]],
                      1 - turnoverParam["BranchSnagTurnoverRate"]))
  mat <- rbind(mat, c(.pooldefids[["SoftwoodBranchSnag"]], .pooldefids[["AboveGroundFastSoil"]],
                      turnoverParam["BranchSnagTurnoverRate"]))

  mat <- rbind(mat, c(.pooldefids[["HardwoodStemSnag"]], .pooldefids[["HardwoodStemSnag"]],
                      1 - turnoverParam["StemSnagTurnoverRate"]))
  mat <- rbind(mat, c(.pooldefids[["HardwoodStemSnag"]], .pooldefids[["MediumSoil"]],
                      turnoverParam["StemSnagTurnoverRate"]))

  mat <- rbind(mat, c(.pooldefids[["HardwoodBranchSnag"]], .pooldefids[["HardwoodBranchSnag"]],
                      1 - turnoverParam["BranchSnagTurnoverRate"]))
  mat <- rbind(mat, c(.pooldefids[["HardwoodBranchSnag"]], .pooldefids[["AboveGroundFastSoil"]],
                      turnoverParam["BranchSnagTurnoverRate"]))

  return(mat)
}

#' Calculate turnover rates
#'
#' For all spatial units in the simulation, calculate turnover rates.
#'
#' @param turnoverRates DESCRIPTION NEEEDED
#' @param spatialUnitIds DESCRIPTION NEEEDED
#' @param spatialUnits DESCRIPTION NEEEDED
#'
#' @return  extracts the turnover rates for the specific SPU you are in.
#' These are used to in the core module to calculate the specific rates, which
#' are the used to calculate Net Primary Productivity (NPP) both in the core
#' module and in the next function.
#'
#' @export
#' @importFrom data.table as.data.table merge.data.table
calcTurnoverRates <- function(turnoverRates, spatialUnitIds, spatialUnits) {
  turnoverRates <- as.data.table(turnoverRates)
  SPU <- as.data.table(spatialUnitIds)
  SPU <- SPU[SpatialUnitID %in% unique(spatialUnitIds)]
  SPU <- merge(SPU, turnoverRates, by = "EcoBoundaryID", all.y = FALSE)
  SPU <- SPU[SpatialUnitID %in% unique(spatialUnits),]
  return(SPU)
}

#' Compute DOM turnover matrices
#'
#' @param turnoverParameters DESCRIPTION NEEDED
#' @param PoolCount DESCRIPTION NEEDED
#'
#' @return DESCRIPTION NEEDED
#'
#' @export
computeDomTurnoverMatrices <- function(turnoverParameters, PoolCount) {
  matrices <- NULL
  for (x in 1:nrow(turnoverParameters)) {
    mat <- domTurnOverMatrix(turnoverParameters[x, ], PoolCount)
    mat <- cbind(turnoverParameters[x, 1], mat)
    matrices <- rbind(matrices, mat)
  }

  colnames(matrices) <- c("id", "row", "col", "value")
  # proportional transaction matrices
  dMat <- as.data.table(matrices)
  cols <- c("row","id")
  dMat[,noLoss := sum(value), by = cols]
  tryVec <- c(.pooldefids[["SoftwoodStemSnag"]], .pooldefids[["SoftwoodBranchSnag"]],
              .pooldefids[["HardwoodStemSnag"]], .pooldefids[["HardwoodBranchSnag"]])
  dMat2 <- dMat[!(row %in% tryVec & value == 1),]
  dMat2[,noLoss := NULL]
  dMat3 <- as.matrix(dMat2)
  return(dMat3)
}

#' Biomass turnover matrix
#'
#' @param turnoverParam DESCRIPTION NEEDED
#' @param PoolCount DESCRIPTION NEEDED
#'
#' @return DESCRIPTION NEEDED
#'
#' @export
biomassTurnoverMatrix <- function(turnoverParam, PoolCount) {
  mat <- getIdentityCoordinateMatrix(PoolCount)

  mat <- rbind(mat, c(.pooldefids[["SoftwoodMerch"]], .pooldefids[["SoftwoodMerch"]],
                      1 - turnoverParam["StemAnnualTurnoverRate"]))
  mat <- rbind(mat, c(.pooldefids[["SoftwoodMerch"]], .pooldefids[["SoftwoodStemSnag"]],
                      turnoverParam["StemAnnualTurnoverRate"]))
  mat <- rbind(mat, c(.pooldefids[["SoftwoodFoliage"]], .pooldefids[["SoftwoodFoliage"]],
                      1 - turnoverParam["SoftwoodFoliageFallRate"]))
  mat <- rbind(mat, c(.pooldefids[["SoftwoodFoliage"]], .pooldefids[["AboveGroundVeryFastSoil"]],
                      turnoverParam["SoftwoodFoliageFallRate"]))
  mat <- rbind(mat, c(.pooldefids[["SoftwoodOther"]], .pooldefids[["SoftwoodOther"]],
                      1 - turnoverParam["SoftwoodBranchTurnoverRate"]))
  mat <- rbind(mat, c(.pooldefids[["SoftwoodOther"]], .pooldefids[["SoftwoodBranchSnag"]],
                      turnoverParam["OtherToBranchSnagSplit"] * turnoverParam["SoftwoodBranchTurnoverRate"]))
  mat <- rbind(mat, c(.pooldefids[["SoftwoodOther"]], .pooldefids[["AboveGroundFastSoil"]],
                      (1 - turnoverParam["OtherToBranchSnagSplit"]) * turnoverParam["SoftwoodBranchTurnoverRate"]))
  mat <- rbind(mat, c(.pooldefids[["SoftwoodCoarseRoots"]], .pooldefids[["SoftwoodCoarseRoots"]],
                      1 - turnoverParam["CoarseRootTurnProp"]))
  mat <- rbind(mat, c(.pooldefids[["SoftwoodCoarseRoots"]], .pooldefids[["AboveGroundFastSoil"]],
                      turnoverParam["CoarseRootAGSplit"] * turnoverParam["CoarseRootTurnProp"]))
  mat <- rbind(mat, c(.pooldefids[["SoftwoodCoarseRoots"]], .pooldefids[["BelowGroundFastSoil"]],
                      (1 - turnoverParam["CoarseRootAGSplit"]) * turnoverParam["CoarseRootTurnProp"]))
  mat <- rbind(mat, c(.pooldefids[["SoftwoodFineRoots"]], .pooldefids[["SoftwoodFineRoots"]],
                      1 - turnoverParam["FineRootTurnProp"]))
  mat <- rbind(mat, c(.pooldefids[["SoftwoodFineRoots"]], .pooldefids[["AboveGroundVeryFastSoil"]],
                      turnoverParam["FineRootAGSplit"] * turnoverParam["FineRootTurnProp"]))
  mat <- rbind(mat, c(.pooldefids[["SoftwoodFineRoots"]], .pooldefids[["BelowGroundVeryFastSoil"]],
                      (1 - turnoverParam["FineRootAGSplit"]) * turnoverParam["FineRootTurnProp"]))

  mat <- rbind(mat, c(.pooldefids[["HardwoodMerch"]], .pooldefids[["HardwoodMerch"]],
                      1 - turnoverParam["StemAnnualTurnoverRate"]))
  mat <- rbind(mat, c(.pooldefids[["HardwoodMerch"]], .pooldefids[["HardwoodStemSnag"]],
                      turnoverParam["StemAnnualTurnoverRate"]))
  mat <- rbind(mat, c(.pooldefids[["HardwoodFoliage"]], .pooldefids[["HardwoodFoliage"]],
                      1 - turnoverParam["HardwoodFoliageFallRate"]))
  mat <- rbind(mat, c(.pooldefids[["HardwoodFoliage"]], .pooldefids[["AboveGroundVeryFastSoil"]],
                      turnoverParam["HardwoodFoliageFallRate"]))
  mat <- rbind(mat, c(.pooldefids[["HardwoodOther"]], .pooldefids[["HardwoodOther"]],
                      1 - turnoverParam["HardwoodBranchTurnoverRate"]))
  mat <- rbind(mat, c(.pooldefids[["HardwoodOther"]], .pooldefids[["HardwoodBranchSnag"]],
                      turnoverParam["OtherToBranchSnagSplit"] * turnoverParam["HardwoodBranchTurnoverRate"]))
  mat <- rbind(mat, c(.pooldefids[["HardwoodOther"]], .pooldefids[["AboveGroundFastSoil"]],
                      (1 - turnoverParam["OtherToBranchSnagSplit"]) * turnoverParam["HardwoodBranchTurnoverRate"]))
  mat <- rbind(mat, c(.pooldefids[["HardwoodCoarseRoots"]], .pooldefids[["HardwoodCoarseRoots"]],
                      1 - turnoverParam["CoarseRootTurnProp"]))
  mat <- rbind(mat, c(.pooldefids[["HardwoodCoarseRoots"]], .pooldefids[["AboveGroundFastSoil"]],
                      turnoverParam["CoarseRootAGSplit"] * turnoverParam["CoarseRootTurnProp"]))
  mat <- rbind(mat, c(.pooldefids[["HardwoodCoarseRoots"]], .pooldefids[["BelowGroundFastSoil"]],
                      (1 - turnoverParam["CoarseRootAGSplit"]) * turnoverParam["CoarseRootTurnProp"]))
  mat <- rbind(mat, c(.pooldefids[["HardwoodFineRoots"]], .pooldefids[["HardwoodFineRoots"]],
                      1 - turnoverParam["FineRootTurnProp"]))
  mat <- rbind(mat, c(.pooldefids[["HardwoodFineRoots"]], .pooldefids[["AboveGroundVeryFastSoil"]],
                      turnoverParam["FineRootAGSplit"] * turnoverParam["FineRootTurnProp"]))
  mat <- rbind(mat, c(.pooldefids[["HardwoodFineRoots"]], .pooldefids[["BelowGroundVeryFastSoil"]],
                      (1 - turnoverParam["FineRootAGSplit"]) * turnoverParam["FineRootTurnProp"]))
  return(mat)
}

#' Compute Turnover Matrices
#'
#' @param turnoverParameters DESCRIPTION NEEDED
#' @param PoolCount DESCRIPTION NEEDED
#'
#' @return DESCRIPTION NEEDED
#'
#' @export
computeBioTurnoverMatrices <- function(turnoverParameters, PoolCount) {
  matrices <- NULL
  for (x in 1:nrow(turnoverParameters)) {
    mat <- biomassTurnoverMatrix(turnoverParameters[x, ], PoolCount)
    mat <- cbind(turnoverParameters[x, 1], mat)
    matrices <- rbind(matrices, mat)
  }

  colnames(matrices) <- c("id", "row", "col", "value")
  # proportional transaction matrices
  dMat <- as.data.table(matrices)
  cols <- c("row", "id")
  dMat[, noLoss := sum(value), by = cols]
  tryVec <- c(.pooldefids[["SoftwoodMerch"]],
              .pooldefids[["SoftwoodFoliage"]],
              .pooldefids[["SoftwoodOther"]],
              .pooldefids[["SoftwoodCoarseRoots"]],
              .pooldefids[["SoftwoodFineRoots"]],
              .pooldefids[["HardwoodMerch"]],
              .pooldefids[["HardwoodFoliage"]],
              .pooldefids[["HardwoodOther"]],
              .pooldefids[["HardwoodCoarseRoots"]],
              .pooldefids[["HardwoodFineRoots"]])
  dMat2 <- dMat[!(row %in% tryVec & value == 1),]
  dMat2[, noLoss := NULL]
  dMat3 <- as.matrix(dMat2)
  return(dMat3)
}
