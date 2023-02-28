utils::globalVariables(c(
  ".", ":=", "age", "ages", "AGturnover", "BGturnover",
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

#' Identify the ID number (CBM-CFS3 legacy) possible in the current spatial unit
#'
#' You give is spatial units you are targeting `mySpu` and it gives you the disturbance matrix id
#' that are possible/default in that specific spu and a descriptive name of that disturbance matrix
#' it creates a `data.frame` of length number of disturbances, with three columns:
#' `spatial_unit_is`, `disturbance_matrix_id`, and a `desciption` of the disturbance.
#'
#' TODO: can we have a Canada-wide SPU map and they locate themselves on the map?
#' this needs to be done before simulations are run so the user can provide this
#' info (location info) for the simulations - Ian is working on this.
#'
#' the function has the defaults from the SK managed forest example.
#' These can be changed by feeding in other SPU.
#'
#' @param mySpu Numeric spatial unit id(s).
#' @param dbPath Path to sqlite database file.
#'
#' @export
#' @importFrom RSQLite dbConnect dbDriver dbListTables dbReadTable
#' @examples
#' \dontrun{
#'   ## using raster
#'   library(terra)
#'   spuRaster <- rast(file.path("data/forIan/SK_data/CBM_GIS/spUnits_TestArea.tif"))
#'   spatial_unit_id <- values(spuRaster) # 28 27
#'   mySpu <- unique(spatial_unit_id)
#'
#'   ## using growth curves
#'   f <- file.path("spadesCBMinputs/data/SK_ReclineRuns30m/LookupTables/yieldRCBM.csv")
#'   gcIn <- as.matrix(read.csv(f))
#'   mySpu <- unique(gcIn[, 1])
#' }
spuDist <- function(mySpu, dbPath) {
  sqlite.driver <- dbDriver("SQLite")
  cbmDefaults <- dbConnect(sqlite.driver, dbname = dbPath)
  alltables <- dbListTables(cbmDefaults)
  cbmTables <- list()

  for (i in 1:length(alltables)) {
    cbmTables[[i]] <- dbReadTable(cbmDefaults, alltables[i])
  }

  # match mySpu with the disturbance_matrix_association table
  dmid <- unique(cbmTables[[7]][which(cbmTables[[7]][, 1] %in% mySpu), c(1, 3)])

  # add the descriptive names
  spuDist <- cbind(dmid, cbmTables[[6]][dmid$disturbance_matrix_id, 3])
  return(spuDist)
}

#' Historical disturbances
#'
#' Identifies the stand-replacing wildfire disturbance in each spatial unit.
#'
#' Historical disturbances in CBM-CFS3 are used for "filling-up" the soil-related carbon pools.
#' Boudewyn et al. (2007) translate the m3/ha curves into biomass per ha in each of four pools:
#' total biomass for stem wood, total biomass for bark, total biomass for branches and total
#' biomass for foliage.
#' Biomass in coarse and fine roots, in aboveground- and belowground- very-fast, -fast, -slow,
#' in medium-soil, and in snags still needs to be estimated.
#' In all spatial units in Canada, the historical disturbance is set to fire.
#' A stand-replacing fire disturbance is used in a disturb-grow cycle, where stands are disturbed
#' and regrown with turnover, overmature, decay, functioning until the dead organic matter pools
#' biomass values stabilize (+/- 10%).
#' ## TODO: (I think but that is in the Rcpp-RCMBGrowthIncrements.cpp so can't check).
#' By default the most recent is selected, but the user can change that.
#'
#' @param mySpu Numeric spatial unit id(s).
#'
#' @export
#' @importFrom stats aggregate
histDist <- function(mySpu = c(27, 28)) {
  # used the spuDist() function to narrow down the choice
  a <- spuDist(mySpu)
  # don't need all the cbm_default tables since column 3 of a give you the names
  # this searches for "wildfire"
  # a[which(grepl("wildfire",a[,3],ignore.case = TRUE)),]
  # if there are more then 1 "wildfire" designation, chose the maximum disturbance
  # matrix id number since that would be the most recent in the database
  b <- aggregate(disturbance_matrix_id ~ spatial_unit_id,
                 data = a[which(grepl("wildfire", a[, 3], ignore.case = TRUE)), ], max
  )
  c <- merge.data.frame(a, b)
  return(c)
}

#' See disturbances
#'
#' Get the descriptive name of the disturbance, the source pools, the sink pools, and
#' the proportions transferred.
#'
#' @param distId Description needed
#' @param dbPath Path to sqlite database file.
#'
#' @return A list of `data.frame`s, one per disturbance matrix id.
#'
#' @export
#' @importFrom RSQLite dbConnect dbDriver dbListTables dbReadTable
seeDist <- function(distId = c(161, 230, 313, 361),
                    dbPath = file.path("data", "cbm_defaults", "cbm_defaults.db")) {
  sqlite.driver <- dbDriver("SQLite")
  cbmDefaults <- dbConnect(sqlite.driver, dbname = dbPath)
  alltables <- dbListTables(cbmDefaults)
  cbmTables <- list()

  for (i in 1:length(alltables)) {
    cbmTables[[i]] <- dbReadTable(cbmDefaults, alltables[i])
  }

  # one copy of each distId
  matNum <- unique(distId)
  lookDists <- vector("list", length = length(matNum))
  c1 <- .poolnames
  c2 <- c(1L:24, 26L)
  poolNames <- as.data.table(cbind(c1,c2))

  # for each matNum, create a data.frame that explains the pool transfers
  for (i in 1:length(matNum)) {
    # get the lines specific to the distMatrix in question
    matD <- as.data.frame(cbmTables[[8]][which(cbmTables[[8]][, 1] == matNum[i]), ])
    names(poolNames) <- c("sinkName", "sink_pool_id")
    sinkNames <- merge.data.frame(poolNames, matD)

    names(poolNames) <- c("sourceName", "source_pool_id")
    sourceNames <- merge.data.frame(poolNames, sinkNames)
    lookDists[[i]] <- sourceNames[, c(5, 1:4, 6)]
  }
  # each data.frame gets a descriptive name
  names(lookDists) <- cbmTables[[6]][matNum, 3]
  # description
  # "Salvage uprooting and burn for Boreal Plains"
  return(lookDists)
}

#' get the descriptive name and proportions transferred for disturbances in a simulation
#' requires a simulation list post simulations (from spades())
#' and returns a list of data.frames. Each data had the descriptive name of a
#' disturbance used in the simulations, the disturbance matrix identification
#' number from cbm_defaults, the pool from which carbon is taken (source pools)
#' in this specific disturbance, the pools into which carbon goes, and the
#' proportion in which the carbon-transfers are completed.
#'
#' @param sim A `SpaDES` CBM simulation (`simList`) object.
#'
#' @export
simDist <- function(sim) {
  # put names to the pools
  poolNames <- as.data.frame(cbind(sim@.envir$pooldef[-1], c(1:24, 26)))
  names(poolNames) <- c("pool", "dmPoolId")

  # Getting the number of DisturbanceMatrixID
  matNum <- unique(sim$mySpuDmids[, 2])
  # matNum will be the lenght of the list of data.frames
  clearDists <- vector("list", length = length(matNum))

  # for each matNum, create a data.frame that explains the pool transfers
  for (i in 1:length(matNum)) {
    # get the lines specific to the distMatrix in question
    matD <- as.data.frame(sim@.envir$cbmData@disturbanceMatrixValues[
      which(sim@.envir$cbmData@disturbanceMatrixValues[, 1] == matNum[i]), ])
    names(poolNames) <- c("sinkName", "sink_pool_id")
    sinkNames <- merge.data.frame(poolNames, matD)

    names(poolNames) <- c("sourceName", "source_pool_id")
    sourceNames <- merge.data.frame(poolNames, sinkNames)
    clearDists[[i]] <- sourceNames[, c(5, 1:4, 6)]
  }
  # each data.frame gets a descriptive name
  names(clearDists) <- sim@.envir$cbmData@disturbanceMatrix[matNum, 3]
  # description
  # "Salvage uprooting and burn for Boreal Plains"
  return(clearDists)
}

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
#'   if (require(LandR)) {
#'     test2 <- LandR::randomStudyArea(seed = 100, size = 10000 * 100 * 300)
#'     out2 <- retrieveSpuRaster(UserArea = test2, rasterRes = c(250, 250))
#'     if (interactive())Plot(out2)
#'   }
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
