utils::globalVariables(c(
  "AboveGroundFastSoil", "AboveGroundSlowSoil", "AboveGroundVeryFastSoil",
  "BelowGroundFastSoil", "BelowGroundSlowSoil", "BelowGroundVeryFastSoil",
  "calcDist", "CH4", "CO", "CO2", "fluxOut",
  "HardwoodBranchSnag", "HardwoodStemSnag", "Input", "MediumSoil", ".N",
  "Products", "SoftwoodBranchSnag", "SoftwoodStemSnag", "V1"
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
#' @return A modified copy of the input \code{mat}
#'
#' @export
domDecayMatrixItem <- function(mat, decayRates, propToAtmosphere, src, dst, emission) {
  offset <- HardwoodFineRoots
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
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere, AboveGroundVeryFastSoil, AboveGroundSlowSoil, CO2)
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere, BelowGroundVeryFastSoil, BelowGroundSlowSoil, CO2)
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere, AboveGroundFastSoil, AboveGroundSlowSoil, CO2)
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere, BelowGroundFastSoil, BelowGroundSlowSoil, CO2)
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere, MediumSoil, AboveGroundSlowSoil, CO2)
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere, SoftwoodStemSnag, AboveGroundSlowSoil, CO2)
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere, SoftwoodBranchSnag, AboveGroundSlowSoil, CO2)
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere, HardwoodStemSnag, AboveGroundSlowSoil, CO2)
  mat <- domDecayMatrixItem(mat, decayRates, propToAtmosphere, HardwoodBranchSnag, AboveGroundSlowSoil, CO2)

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
  tryVec <- c(AboveGroundVeryFastSoil,BelowGroundVeryFastSoil,
              AboveGroundFastSoil,BelowGroundFastSoil,MediumSoil,
              AboveGroundSlowSoil,
              BelowGroundSlowSoil,
              SoftwoodStemSnag,
              SoftwoodBranchSnag,
              HardwoodStemSnag,
              HardwoodBranchSnag)
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
  offset <- HardwoodFineRoots
  mat <- getIdentityCoordinateMatrix(PoolCount)
  propToAtmosphere <- decayParameters[, "PropToAtmosphere"]
  mat <- rbind(mat, c(AboveGroundSlowSoil, AboveGroundSlowSoil, 1 - decayRates[AboveGroundSlowSoil - offset]))
  mat <- rbind(mat, c(AboveGroundSlowSoil, CO2, decayRates[AboveGroundSlowSoil - offset] * propToAtmosphere[AboveGroundSlowSoil - offset]))
  mat <- rbind(mat, c(BelowGroundSlowSoil, BelowGroundSlowSoil, 1 - decayRates[BelowGroundSlowSoil - offset]))
  mat <- rbind(mat, c(BelowGroundSlowSoil, CO2, decayRates[BelowGroundSlowSoil - offset] * propToAtmosphere[AboveGroundSlowSoil - offset]))
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
  tryVec <- c(AboveGroundSlowSoil,
              BelowGroundSlowSoil)
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
  mat <- mat[-AboveGroundSlowSoil,]
  mat <- rbind(mat, c(AboveGroundSlowSoil, BelowGroundSlowSoil, slowMixingRate))
  mat <- rbind(mat, c(AboveGroundSlowSoil, AboveGroundSlowSoil, 1 - slowMixingRate))
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

  mat <- rbind(mat, c(SoftwoodStemSnag, SoftwoodStemSnag, 1 - turnoverParam["StemSnagTurnoverRate"]))
  mat <- rbind(mat, c(SoftwoodStemSnag, MediumSoil, turnoverParam["StemSnagTurnoverRate"]))

  mat <- rbind(mat, c(SoftwoodBranchSnag, SoftwoodBranchSnag, 1 - turnoverParam["BranchSnagTurnoverRate"]))
  mat <- rbind(mat, c(SoftwoodBranchSnag, AboveGroundFastSoil, turnoverParam["BranchSnagTurnoverRate"]))

  mat <- rbind(mat, c(HardwoodStemSnag, HardwoodStemSnag, 1 - turnoverParam["StemSnagTurnoverRate"]))
  mat <- rbind(mat, c(HardwoodStemSnag, MediumSoil, turnoverParam["StemSnagTurnoverRate"]))

  mat <- rbind(mat, c(HardwoodBranchSnag, HardwoodBranchSnag, 1 - turnoverParam["BranchSnagTurnoverRate"]))
  mat <- rbind(mat, c(HardwoodBranchSnag, AboveGroundFastSoil, turnoverParam["BranchSnagTurnoverRate"]))

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
  tryVec <- c(SoftwoodStemSnag,
              SoftwoodBranchSnag,
              HardwoodStemSnag,
              HardwoodBranchSnag)
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

  mat <- rbind(mat, c(SoftwoodMerch, SoftwoodMerch, 1 - turnoverParam["StemAnnualTurnoverRate"]))
  mat <- rbind(mat, c(SoftwoodMerch, SoftwoodStemSnag, turnoverParam["StemAnnualTurnoverRate"]))
  mat <- rbind(mat, c(SoftwoodFoliage, SoftwoodFoliage, 1 - turnoverParam["SoftwoodFoliageFallRate"]))
  mat <- rbind(mat, c(SoftwoodFoliage, AboveGroundVeryFastSoil, turnoverParam["SoftwoodFoliageFallRate"]))
  mat <- rbind(mat, c(SoftwoodOther, SoftwoodOther, 1 - turnoverParam["SoftwoodBranchTurnoverRate"]))
  mat <- rbind(mat, c(SoftwoodOther, SoftwoodBranchSnag,
                      turnoverParam["OtherToBranchSnagSplit"] * turnoverParam["SoftwoodBranchTurnoverRate"]))
  mat <- rbind(mat, c(SoftwoodOther, AboveGroundFastSoil,
                      (1 - turnoverParam["OtherToBranchSnagSplit"]) * turnoverParam["SoftwoodBranchTurnoverRate"]))
  mat <- rbind(mat, c(SoftwoodCoarseRoots, SoftwoodCoarseRoots, 1 - turnoverParam["CoarseRootTurnProp"]))
  mat <- rbind(mat, c(SoftwoodCoarseRoots, AboveGroundFastSoil,
                      turnoverParam["CoarseRootAGSplit"] * turnoverParam["CoarseRootTurnProp"]))
  mat <- rbind(mat, c(SoftwoodCoarseRoots, BelowGroundFastSoil,
                      (1 - turnoverParam["CoarseRootAGSplit"]) * turnoverParam["CoarseRootTurnProp"]))
  mat <- rbind(mat, c(SoftwoodFineRoots, SoftwoodFineRoots, 1 - turnoverParam["FineRootTurnProp"]))
  mat <- rbind(mat, c(SoftwoodFineRoots, AboveGroundVeryFastSoil,
                      turnoverParam["FineRootAGSplit"] * turnoverParam["FineRootTurnProp"]))
  mat <- rbind(mat, c(SoftwoodFineRoots, BelowGroundVeryFastSoil,
                      (1 - turnoverParam["FineRootAGSplit"]) * turnoverParam["FineRootTurnProp"]))

  mat <- rbind(mat, c(HardwoodMerch, HardwoodMerch, 1 - turnoverParam["StemAnnualTurnoverRate"]))
  mat <- rbind(mat, c(HardwoodMerch, HardwoodStemSnag, turnoverParam["StemAnnualTurnoverRate"]))
  mat <- rbind(mat, c(HardwoodFoliage, HardwoodFoliage, 1 - turnoverParam["HardwoodFoliageFallRate"]))
  mat <- rbind(mat, c(HardwoodFoliage, AboveGroundVeryFastSoil, turnoverParam["HardwoodFoliageFallRate"]))
  mat <- rbind(mat, c(HardwoodOther, HardwoodOther, 1 - turnoverParam["HardwoodBranchTurnoverRate"]))
  mat <- rbind(mat, c(HardwoodOther, HardwoodBranchSnag,
                      turnoverParam["OtherToBranchSnagSplit"] * turnoverParam["HardwoodBranchTurnoverRate"]))
  mat <- rbind(mat, c(HardwoodOther, AboveGroundFastSoil,
                      (1 - turnoverParam["OtherToBranchSnagSplit"]) * turnoverParam["HardwoodBranchTurnoverRate"]))
  mat <- rbind(mat, c(HardwoodCoarseRoots, HardwoodCoarseRoots, 1 - turnoverParam["CoarseRootTurnProp"]))
  mat <- rbind(mat, c(HardwoodCoarseRoots, AboveGroundFastSoil,
                      turnoverParam["CoarseRootAGSplit"] * turnoverParam["CoarseRootTurnProp"]))
  mat <- rbind(mat, c(HardwoodCoarseRoots, BelowGroundFastSoil,
                      (1 - turnoverParam["CoarseRootAGSplit"]) * turnoverParam["CoarseRootTurnProp"]))
  mat <- rbind(mat, c(HardwoodFineRoots, HardwoodFineRoots, 1 - turnoverParam["FineRootTurnProp"]))
  mat <- rbind(mat, c(HardwoodFineRoots, AboveGroundVeryFastSoil,
                      turnoverParam["FineRootAGSplit"] * turnoverParam["FineRootTurnProp"]))
  mat <- rbind(mat, c(HardwoodFineRoots, BelowGroundVeryFastSoil,
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
  cols <- c("row","id")
  dMat[,noLoss := sum(value), by = cols]
  tryVec <- c(SoftwoodMerch,
              SoftwoodFoliage,
              SoftwoodOther,
              SoftwoodCoarseRoots,
              SoftwoodFineRoots,
              HardwoodMerch,
              HardwoodFoliage,
              HardwoodOther,
              HardwoodCoarseRoots,
              HardwoodFineRoots)
  dMat2 <- dMat[!(row %in% tryVec & value == 1),]
  dMat2[,noLoss := NULL]
  dMat3 <- as.matrix(dMat2)
  return(dMat3)
}

#' Calculate C transfer for disturbances and annual processes post-disturbance
#'
#' Mismatch in C transfers when disturbance happens in C++ processing so bypassing it
#' C transfer functions: one for the disturbance (so small decimals errors in
#' matrices are corrected), and one for the annual processes.
#'
#' @param standIn DESCRIPTION NEEDED
#' @param transProp  DESCRIPTION NEEDED
#'
#' @return DESCRIPTION NEEDED
#'
#' @export
#' @importFrom data.table as.data.table
#' @rdname cTransfer
cTransfer <- function(standIn, transProp) {
  standIn <- as.data.table(cbind(standIn, row = c(1:length(standIn))))
  names(standIn) <- c("V1","row")
  transProp <- as.data.table(transProp)
  rowJoin <- standIn[transProp, on = "row"][, fluxOut := (V1*value)]

  # calculate carbon out and in
  outC <- rowJoin[, .(outC = sum(fluxOut)), by = row]
  inC <-  rowJoin[, .(inC = sum(fluxOut)), by = col]
  names(inC) <- c("row", "inC")
  fluxes <- merge(outC, inC, by = "row", all = TRUE)
  # no NAs allowed
  fluxes$inC[which(is.na(fluxes$inC))] <- 0

  standOut <- standIn[fluxes, on = "row"][, .(calcDist = V1 - outC + inC), by = "row"]

  return(standOut)
}

#' @export
#' @importFrom data.table as.data.table
#' @rdname cTransfer
cTransferDist <- function(standIn, transProp) {
  standIn <- as.data.table(cbind(standIn, row = c(1:length(standIn))))
  names(standIn) <- c("V1","row")
  transProp <- as.data.table(transProp)
  rowJoin <- standIn[transProp, on = "row"][, fluxOut := (V1*value)]

  # calculate carbon out and in
  outC <- rowJoin[, .(outC = sum(fluxOut)), by = row]
  inC <-  rowJoin[, .(inC = sum(fluxOut)), by = col]
  names(inC) <- c("row", "inC")
  fluxes <- merge(outC, inC, by = "row", all = TRUE)
  # no NAs allowed
  fluxes$inC[which(is.na(fluxes$inC))] <- 0

  standOut <- standIn[fluxes, on = "row"][, .(calcDist = V1 - outC + inC), by = "row"]

  # these two lines are "fixes for small decimal differences that should not be there
  # pools can't go negative
  standOut[calcDist < 0, "calcDist"] <- 0
  # if it does not transfer to itself it has to end-up empty
  rowsTofix <- transProp[row == col, .N, by = "row"]
  standOut[!(row %in% rowsTofix$row), "calcDist"] <- 0

  return(standOut)
}
