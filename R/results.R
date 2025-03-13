utils::globalVariables(c(
  "..colsAG", "..colsBG", "absCarbon",
  "SoftwoodMerch", "SoftwoodFoliage", "SoftwoodOther",
  "HardwoodMerch", "HardwoodFoliage", "HardwoodOther",
  "SoftwoodStemSnag", "SoftwoodBranchSnag",
  "HardwoodStemSnag", "HardwoodBranchSnag",
  "AboveGroundVeryFastSoil", "AboveGroundFastSoil",
  "AboveGroundSlowSoil",
  "SoftwoodCoarseRoots", "SoftwoodFineRoots",
  "HardwoodCoarseRoots", "HardwoodFineRoots",
  "BelowGroundVeryFastSoil",
  "BelowGroundFastSoil", "MediumSoil",
  "BelowGroundSlowSoil"
))

#' Sum carbon for `totalCarbon` or `aboveGround` or `belowGround`
#'
#' @param cbmPools DESCRIPTION NEEDED
#' @param poolToSum  DESCRIPTION NEEDED
#' @template masterRaster
#'
#' @return DESCRIPTION NEEDED
#'
#' @export
#' @importFrom terra res
calcC <- function(cbmPools, poolToSum, masterRaster) {
  # targetPool <- poolToSum
  # year <- time(RIApresentDayRuns)
  # cbmPools <- RIApresentDayRuns$cbmPools
  # masterRaster <- RIApresentDayRuns$masterRaster
  # calculate total carbon by pixelGroup
  if ("totalCarbon" %in% poolToSum) {
    targetPool <- apply(cbmPools[, SoftwoodMerch:HardwoodBranchSnag], 1, "sum")
    cbmPools <- cbind(cbmPools, targetPool)
  }
  ## Add AG and BG options here
  if ("aboveGround" %in% poolToSum) {
    colsAG <- c("SoftwoodMerch", "SoftwoodFoliage", "SoftwoodOther",
                "HardwoodMerch", "HardwoodFoliage", "HardwoodOther",
                "SoftwoodStemSnag", "SoftwoodBranchSnag",
                "HardwoodStemSnag", "HardwoodBranchSnag",
                "AboveGroundVeryFastSoil", "AboveGroundFastSoil",
                "AboveGroundSlowSoil")
    targetPool <- apply(cbmPools[, ..colsAG], 1, "sum")
    cbmPools <- cbind(cbmPools, targetPool)
  }
  ## belowGround
  if ("belowGround" %in% poolToSum) {
    colsBG <- c("SoftwoodCoarseRoots", "SoftwoodFineRoots",
                "HardwoodCoarseRoots", "HardwoodFineRoots",
                "BelowGroundVeryFastSoil",
                "BelowGroundFastSoil", "MediumSoil",
                "BelowGroundSlowSoil")
    targetPool <- apply(cbmPools[, ..colsBG], 1, "sum")
    cbmPools <- cbind(cbmPools, targetPool)
  }

  sumColsOnly <- cbmPools[, .(simYear,pixelCount, pixelGroup, targetPool)]
  ## check that all is good
  sumColsOnly[, sum(pixelCount), by = simYear]
  # simYear      V1
  # 1:    1985 3112425
  # 2:    1990 3112425
  # 3:    1995 3112425
  # 4:    2000 3112425
  # 5:    2005 3112425
  # 6:    2010 3112425
  # 7:    2013 3112425
  # 8:    2014 3112425
  # 9:    2015 3112425
  resInHa <- res(masterRaster)[1] * res(masterRaster)[2] / 10000
  sumColsOnly[, absCarbon := (pixelCount * resInHa * targetPool)]
  landscapeCarbon <- sumColsOnly[, sum(absCarbon) / 1000000, by = simYear]
  return(landscapeCarbon)
}
## END function to sum carbon----------------------------------------------

## Same function just for summing total carbon -----------------
calcTotalC <- function(cbmPools, masterRaster){
  # year <- time(RIApresentDayRuns)
  # cbmPools <- RIApresentDayRuns$cbmPools
  # masterRaster <- RIApresentDayRuns$masterRaster
  # calculate total carbon by pixelGroup
  totalCarbon <- apply(cbmPools[, SoftwoodMerch:HardwoodBranchSnag], 1, "sum")
  cbmPools <- cbind(cbmPools, totalCarbon)
  totColsOnly <- cbmPools[,.(simYear,pixelCount, pixelGroup, totalCarbon)]
  ## check that all is good
  totColsOnly[, sum(pixelCount), by = simYear]
  # simYear      V1
  # 1:    1985 3112425
  # 2:    1990 3112425
  # 3:    1995 3112425
  # 4:    2000 3112425
  # 5:    2005 3112425
  # 6:    2010 3112425
  # 7:    2013 3112425
  # 8:    2014 3112425
  # 9:    2015 3112425
  resInHa <- res(masterRaster)[1]*res(masterRaster)[2]/10000
  totColsOnly[, absCarbon := (pixelCount*resInHa*totalCarbon)]
  landscapeCarbon <- totColsOnly[,sum(absCarbon)/1000000, by = simYear]
  return(landscapeCarbon)

}
