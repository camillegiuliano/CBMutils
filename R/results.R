utils::globalVariables(c(
  ":=", "..colsAG", "..colsBG", "absCarbon"
))

#' Plot
#'
#' the `spatialPlot` function plots rasters directly, but does not return rasters, whereas
#' it does so here.
#'
#' @param pixelkeep DESCRIPTION NEEDED
#' @param cbmPools DESCRIPTION NEEDED
#' @param poolsToPlot DESCRIPTION NEEDED
#' @param years DESCRIPTION NEEDED
#' @template masterRaster
#'
#' @return `RasterLayer` but invoked for its side-effect of plotting the rasters.
#'
#' @export
#' @importFrom stats na.omit
#' @importFrom terra rast
#' @seealso spatialPlot
#'
spatialRaster <- function(pixelkeep, cbmPools, poolsToPlot, years, masterRaster) {
  cbmPools[is.na(cbmPools)] <- 0
  colnames(cbmPools)[c(1, 3, 4)] <- c("simYear", "pixelGroup", "age")
  # totalCarbon
  if ("totalCarbon" %in% poolsToPlot) {
    totalCarbon <- apply(cbmPools[, SoftwoodMerch:HardwoodBranchSnag], 1, "sum")
    cbmPools <- cbind(cbmPools, totalCarbon)
  }
  ## Add AG and BG options here
  if ("aboveGround" %in% poolsToPlot) {
    colsAG <- c("SoftwoodMerch", "SoftwoodFoliage", "SoftwoodOther",
                "HardwoodMerch", "HardwoodFoliage", "HardwoodOther",
                "SoftwoodStemSnag", "SoftwoodBranchSnag",
                "HardwoodStemSnag", "HardwoodBranchSnag",
                "AboveGroundVeryFastSoil", "AboveGroundFastSoil",
                "AboveGroundSlowSoil")
    aboveGround <- apply(cbmPools[, ..colsAG], 1, "sum")
    cbmPools <- cbind(cbmPools, aboveGround)
  }
  ## belowGround
  if ("belowGround" %in% poolsToPlot) {
    colsBG <- c("SoftwoodCoarseRoots", "SoftwoodFineRoots",
                "HardwoodCoarseRoots", "HardwoodFineRoots",
                "BelowGroundVeryFastSoil",
                "BelowGroundFastSoil", "MediumSoil",
                "BelowGroundSlowSoil")
    belowGround <- apply(cbmPools[, ..colsBG], 1, "sum")
    cbmPools <- cbind(cbmPools, belowGround)
  }
  if (any(!poolsToPlot %in% colnames(cbmPools))) {
    stop("The carbon pool you specified for plotting is not contained in the pool definitions")
  }

  cbmPools <- as.data.table( cbmPools)
  #Build rasters for every year and pool
  carbonStacks <- vector(mode = "list", length = length(poolsToPlot))
  names(carbonStacks) <- poolsToPlot

  for (pool in poolsToPlot) {
    carbonStacks[[pool]] <- lapply(years, FUN = function(x, poolsDT = cbmPools,
                                                         var = pool,
                                                         pixelKeep =  pixelkeep) {

      poolsDT <- poolsDT[order(pixelGroup)] %>% #order by stand index
        .[simYear == x, .(pixelGroup, "var" =  get(pool))] #subset by year
      #subset  pixelKeep
      colsToKeep <- c("pixelIndex", paste0("pixelGroup", x))

      pixelKeep <- pixelKeep[, colsToKeep, with = FALSE] %>%
        setnames(., c("pixelIndex", "pixelGroup"))
      # with=FALSE tells data.table colsToKeep isn't a column name until here it
      # is ok...then in 1993 - an extra line gets added from the merge below
      # Keep <- poolsDT[pixelKeep, on = c("pixelGroup")]
      pixelKeep <- merge(pixelKeep, poolsDT, by = "pixelGroup", all.x = TRUE) %>% #join with pixelKeep
        .[order(pixelIndex)] #order by rowOrder for raster prep
      #pixelKeep <- pixelKeep[poolsDT, on = c("pixelGroup")] %>% #join with pixelKeep
      pixels <- values(masterRaster)

      plotMaster <- rast(masterRaster)
      plotMaster[] <- 0
      plotMaster[pixelKeep$pixelIndex] <- pixelKeep$var
      # masterRaster[masterRaster == 0] <- NA #Species has zeroes instead of NA. Revisit if masterRaster changes
      # masterRaster[!is.na(masterRaster)] <- pixelKeep$var

      #name will begin with x if no character assigned
      return(plotMaster)
    })
  }

  names(carbonStacks) <- paste0(poolsToPlot)
  unlist(carbonStacks)
  return(carbonStacks)
}
# END raster plotting function for paper -----------------------------------------

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
