utils::globalVariables(c(
  "age", "ages", "AGturnover", "BGturnover",
  "CoarseRootTurnProp", "FineRootTurnProp", "grossGrowthAG", "grossGrowthBG",
  "HardwoodBranchTurnoverRate", "HardwoodFoliageFallRate",
  "HardwoodCoarseRoots", "HardwoodFineRoots", "HardwoodFoliage", "HardwoodMerch", "HardwoodOther",
  "HardwoodCoarseRoots.x", "HardwoodFineRoots.x", "HardwoodFoliage.x", "HardwoodMerch.x",
  "HardwoodOther.x",
  "HardwoodCoarseRoots.y", "HardwoodFineRoots.y", "HardwoodFoliage.y", "HardwoodMerch.y",
  "HardwoodOther.y",
  "id", "pixelGroup", "pixelIndex", "simYear",
  "SoftwoodBranchTurnoverRate", "SoftwoodFoliageFallRate",
  "SoftwoodCoarseRoots", "SoftwoodFineRoots", "SoftwoodFoliage", "SoftwoodMerch", "SoftwoodOther",
  "SoftwoodCoarseRoots.x", "SoftwoodFineRoots.x", "SoftwoodFoliage.x", "SoftwoodMerch.x",
  "SoftwoodOther.x",
  "SoftwoodCoarseRoots.y", "SoftwoodFineRoots.y", "SoftwoodFoliage.y", "SoftwoodMerch.y",
  "SoftwoodOther.y",
  "spatial_unit_id", "SpatialUnitID", "StemAnnualTurnoverRate",
  "value", "variable"
))

#' Disturbance matrix pool names and ids
#'
#' TODO: confirm these
#'
#' NOTE: A reminder that indexing in R starts at 1, whereas in C++ it starts at 0.
#' Several C++ data structures do not include the Input category, so the indices there are
#' defined using `.poolids - 1`.
#'
#' @export
#' @rdname poolids
.pooldef <- c(
  "Input",
  "SoftwoodMerch",
  "SoftwoodFoliage",
  "SoftwoodOther",
  "SoftwoodCoarseRoots",
  "SoftwoodFineRoots",
  "HardwoodMerch",
  "HardwoodFoliage",
  "HardwoodOther",
  "HardwoodCoarseRoots",
  "HardwoodFineRoots",
  "AboveGroundVeryFastSoil",
  "BelowGroundVeryFastSoil",
  "AboveGroundFastSoil",
  "BelowGroundFastSoil",
  "MediumSoil",
  "AboveGroundSlowSoil",
  "BelowGroundSlowSoil",
  "SoftwoodStemSnag",
  "SoftwoodBranchSnag",
  "HardwoodStemSnag",
  "HardwoodBranchSnag",
  "CO2",
  "CH4",
  "CO",
  "Products"
)

#' @export
#' @rdname poolids
.pooldefids <- as.list(1L:26L)
names(.pooldefids) <- .pooldef
.pooldefids <- list2env(.pooldefids, parent = emptyenv(), size = length(.pooldef))

#' @export
#' @rdname poolids
.poolnames <- .pooldef[2L:length(.pooldef)] ## without 'Input'

#' @export
#' @rdname poolids
.poolids <- as.list(c(1L:24, 26L)) ## NOTE: pool 25 is NO2; not currently used
names(.poolids) <- .poolnames
.poolids <- list2env(.poolids, parent = emptyenv(), size = length(.poolnames))

#' Plotting pools
#'
#' @param pixelkeep DESCRIPTION NEEDED
#' @param cbmPools DESCRIPTION NEEDED
#' @param poolsToPlot DESCRIPTION NEEDED
#' @param years DESCRIPTION NEEDED
#' @template masterRaster
#'
#' @export
#' @importFrom data.table as.data.table setnames
#' @importFrom magrittr %>%
#' @importFrom quickPlot clearPlot Plot
#'
#' @examples
#' \dontrun{
#' # include 'totalCarbon' in poolsToPlot to add plot of total carbon
#' plotCarbonRasters(
#'   cbmPools = spadesCBMout$cbmPools,
#'   poolsToPlot = c("totalCarbon", "BelowGroundSlowSoil"),
#'   masterRaster = spadesCBMout$masterRaster,
#'   pixelkeep = spadesCBMout$pixelKeep,
#'   years = c(1990, 2000, 2005)
#' )
#' }
plotCarbonRasters <- function(pixelkeep, cbmPools, poolsToPlot, years, masterRaster) {
  if ("totalCarbon" %in% poolsToPlot) {
    totalCarbon <- apply(cbmPools[, 5:25], 1, "sum")
    cbmPools <- cbind(cbmPools, totalCarbon)
  }

  if (any(!poolsToPlot %in% colnames(cbmPools))) {
    stop("The carbon pool you specified is not contained in the pool definitions")
  }

  cbmPools <- as.data.table(cbmPools)
  # Build rasters for every year and pool
  carbonStacks <- vector(mode = "list", length = length(poolsToPlot))
  names(carbonStacks) <- poolsToPlot

  for (pool in poolsToPlot) {
    carbonStacks[[pool]] <- lapply(years, FUN = function(x, poolsDT = cbmPools,
                                                         var = pool,
                                                         pixelKeep = pixelkeep) {
      poolsDT <- poolsDT[order(pixelGroup)] %>% # order by stand index
        .[simYear == x, .(pixelGroup, "var" = get(pool))] # subset by year
      # subset  pixelKeep
      colsToKeep <- c("pixelIndex", paste0("pixelGroup", x))

      pixelKeep <- pixelKeep[, colsToKeep, with = FALSE] %>%
        setnames(., c("pixelIndex", "pixelGroup")) # with=FALSE tells data.table colsToKeep isn't a column name
      pixelKeep <- pixelKeep[poolsDT, on = c("pixelGroup")] %>% # join with pixelKeep
        .[order(pixelIndex)] # order by rowOrder for raster prep

      masterRaster[masterRaster == 0] <- NA # Species has zeroes instead of NA. Revisit if masterRaster changes
      masterRaster[!is.na(masterRaster)] <- pixelKeep$var

      names(masterRaster) <- x # name will begin with x if no character assigned
      return(masterRaster)
    })
    names(carbonStacks[[pool]]) <- paste(pool, years, sep = "_")
  }
  names(carbonStacks) <- NULL
  temp <- unlist(carbonStacks)
  clearPlot()
  Plot(temp)
}

#' Produce a raster with `spUnits`
#'
#' @param spatialUnitsFile DESCRIPTION NEEDED
#' @param UserArea DESCRIPTION NEEDED
#' @param rasterRes The desired raster resolution.
#'
#' @export
#' @importFrom fasterize fasterize
#' @importFrom magrittr %>%
#' @importFrom raster couldBeLonLat crop crs extent raster shapefile
#' @importFrom sf st_as_sf
#' @importFrom sp identicalCRS spTransform
#'
#' @examples
#' \dontrun{
#'   test1 <- raster::shapefile("data/forIan/SK_data/CBM_GIS/SpadesCBM_TestArea.shp")
#'   out1 <- retrieveSpuRaster(UserArea = test1, rasterRes = c(250, 250))
#'   if (interactive()) Plot(out1)
#'
#' }
#'
retrieveSpuRaster <- function(spatialUnitsFile = shapefile("data/spUnit_Locator.shp"),
                              UserArea,
                              rasterRes = c(250, 250)) {
  if (!(inherits(UserArea, "SpatialPolygonsDataFrame") | inherits(UserArea, "RasterLayer"))) {
    stop("Please supply UserArea as a SpatialPolygonsDataFrame or RasterLayer")
  }

  if (!identicalCRS(spatialUnitsFile, UserArea)) {
    spatialUnitsFile <- spTransform(x = spatialUnitsFile, CRSobj = crs(UserArea)) ## TODO: use CRS()?
  }

  test <- couldBeLonLat(UserArea)
  if (test == TRUE && any(rasterRes > 1)) {
    warning("rasterRes is measured in units of the UserArea, which appears to be lat long")
  }

  temp <- crop(spatialUnitsFile, UserArea) %>%
    sf::st_as_sf(.)
  template <- raster(extent(temp), res = rasterRes, crs = crs(UserArea))
  spuRaster <- fasterize::fasterize(sf = temp, raster = template, field = "spu_id")

  return(spuRaster)
}

#' Calculate post-simulation net primary productivity
#'
#' @param pools DESCRIPTION NEEDED
#' @param pixelGroupSpu DESCRIPTION NEEDED
#' @param year1,year2 Consecutive start and end years (e.g., 1990, 1991)
#' @param turnoverRates DESCRIPTION NEEDED
#'
#' @export
#' @importFrom data.table as.data.table merge.data.table setkey
#'
#' @examples
#' \dontrun{
#'   spadesCBMout <- simInitAndSpades() ## TODO: fill this in
#'   NPP(pools = spadesCBMout$cbmPools,
#'       pixelGroupSpu = spadesCBMout$pixelGroupC[, .(pixelGroup, spatial_unit_id)],
#'       year1 = 1990, year2 = 1991,
#'       turnoverRates = spadesCBMout$turnoverRates)
#' }
NPP <- function(pools, pixelGroupSpu, year1, year2, turnoverRates) {
  a <- as.data.table(pools)[simYear == year1, .(
    pixelGroup,
    ages,
    SoftwoodMerch,
    SoftwoodFoliage,
    SoftwoodOther,
    SoftwoodCoarseRoots,
    SoftwoodFineRoots,
    HardwoodMerch,
    HardwoodFoliage,
    HardwoodOther,
    HardwoodCoarseRoots,
    HardwoodFineRoots
  )]
  b <- pixelGroupSpu[(pixelGroup %in% a$pixelGroup)]
  stockt <- merge(a, b, by = "pixelGroup")
  setkey(stockt, pixelGroup)

  stockt1 <- as.data.table(pools)[simYear == year2, .(
    pixelGroup,
    ages,
    SoftwoodMerch,
    SoftwoodFoliage,
    SoftwoodOther,
    SoftwoodCoarseRoots,
    SoftwoodFineRoots,
    HardwoodMerch,
    HardwoodFoliage,
    HardwoodOther,
    HardwoodCoarseRoots,
    HardwoodFineRoots
  )]
  setkey(stockt1, pixelGroup)

  stocks2t <- merge(stockt[, -c("ages", "spatial_unit_id")], stockt1[, -"ages"],
                    by = "pixelGroup", sort = TRUE)

  grossGrowth <- stocks2t[, .(
    pixelGroup,
    grossGrowthAG = (
      (SoftwoodMerch.y - SoftwoodMerch.x) +
        (SoftwoodFoliage.y - SoftwoodFoliage.x) +
        (SoftwoodOther.y - SoftwoodOther.x) +
        (HardwoodMerch.y - HardwoodMerch.x) +
        (HardwoodFoliage.y - HardwoodFoliage.x) +
        (HardwoodOther.y - HardwoodOther.x)),
    grossGrowthBG = (
      (SoftwoodCoarseRoots.y - SoftwoodCoarseRoots.x) +
        (SoftwoodFineRoots.y - SoftwoodFineRoots.x) +
        (HardwoodCoarseRoots.y - HardwoodCoarseRoots.x) +
        (HardwoodFineRoots.y - HardwoodFineRoots.x))
  )]

  turnoverRates <- turnoverRates[, spatial_unit_id := SpatialUnitID]

  turnoverComponents <- merge(stockt, turnoverRates, by = "spatial_unit_id")

  turnover <- turnoverComponents[, .(
    pixelGroup,
    AGturnover = (
      (SoftwoodMerch * StemAnnualTurnoverRate) +
        (SoftwoodFoliage * SoftwoodFoliageFallRate) +
        (SoftwoodOther * SoftwoodBranchTurnoverRate) +
        (HardwoodMerch * StemAnnualTurnoverRate) +
        (HardwoodFoliage * HardwoodFoliageFallRate) +
        (HardwoodOther * HardwoodBranchTurnoverRate)),
    BGturnover = (
      (SoftwoodCoarseRoots * CoarseRootTurnProp) +
        (SoftwoodFineRoots * FineRootTurnProp) +
        (HardwoodCoarseRoots * CoarseRootTurnProp) +
        (HardwoodFineRoots * FineRootTurnProp))
  )]

  NPP <- merge(turnover, grossGrowth, by = "pixelGroup")[, .(
    pixelGroup,
    NPP = (AGturnover + BGturnover + grossGrowthAG + grossGrowthBG)
  )]

  return(NPP)
}

#' m3ToVolCheckPlots
#'
#' Check the growth increments by above ground pool. This is
#' necessary because the Boudewyn et al parameters used to translate the
#' m3/ha into biomass/ha do not always work. Returns a list of plots, each
#' plot show the merch, fol, and other increments for a specific growth curve.
#'
#' @param sim A `SpaDES` CBM simulation object (`simList`).
#'
#' @export
#' @importFrom data.table melt
#' @importFrom ggplot2 aes geom_line ggplot
m3ToVolCheckPlots <- function(sim) {
  gInc <- as.data.table(sim$growth_increments)
  idSim <- unique(gInc$id)
  gcSim <- gInc[id %in% idSim, ]
  gc <- melt(gcSim, id.vars = c("id", "age"), measure.vars = 3:8)
  names(idSim) <- idSim
  plots <- lapply(idSim, function(idLoop) {
    ggplot(data = gc[id == idLoop], aes(x = age, y = value, group = variable, colour = variable)) +
      geom_line()
  })

  return(plots)
}
