utils::globalVariables(c(
  "AGB", "AGlive", "BGB", "BGlive", "carbon", "DOM", "emissionsCH4", "emissionsCO", "emissionsCO2",
  "pixelCount", "pixNPP", "pool", "products", "res", "snags", "soil", "weight"
))

#' `m3ToBiomIncOnlyPlots`
#'
#' @param inc TODO
#'
#' @return TODO
#'
#' @export
#' @importFrom data.table as.data.table melt
#' @importFrom ggplot2 aes geom_line ggplot
m3ToBiomIncOnlyPlots <- function(inc) {
  gInc <- as.data.table(inc)
  idSim <- unique(gInc$id)
  gcSim <- gInc[id %in% idSim,]
  gc <- data.table::melt(gcSim, id.vars = c("id", "age"), measure.vars = 3:dim(gInc)[2])
  names(idSim) <- idSim
  plots <- lapply(idSim, function(idLoop) {
    ggplot(data = gc[id == idLoop], aes(x = age, y = value, group = variable, colour = variable)) +
      geom_line()
  })
  names(plots) <- paste0("id",names(plots))
  return(plots)
}

#' `spatialPlot`
#'
#' @param pixelkeep TODO
#' @param cbmPools TODO
#' @param poolsToPlot TODO
#' @param years TODO
#' @param masterRaster TODO
#'
#' @return TODO
#'
#' @export
#' @importFrom data.table as.data.table merge.data.table
#' @importFrom raster getValues raster
#' @importFrom quickPlot Plot
spatialPlot <- function(pixelkeep, cbmPools, poolsToPlot, years, masterRaster) {
  cbmPools[is.na(cbmPools)] <- 0
  colnames(cbmPools)[c(1,3,4)] <- c("simYear", "pixelGroup", "age")
  if ("totalCarbon" %in% poolsToPlot) {
    totalCarbon <- apply(cbmPools[, SoftwoodMerch:HardwoodBranchSnag], 1, "sum")
    cbmPools <- cbind(cbmPools, totalCarbon)
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
      pixels <- getValues(masterRaster)

      plotMaster <- raster(masterRaster)
      plotMaster[] <- 0
      plotMaster[pixelKeep$pixelIndex] <- pixelKeep$var
      # masterRaster[masterRaster == 0] <- NA #Species has zeroes instead of NA. Revisit if masterRaster changes
      # masterRaster[!is.na(masterRaster)] <- pixelKeep$var

      #name will begin with x if no character assigned
      return(plotMaster)
    })
  }

  names(carbonStacks) <- paste0(poolsToPlot)

  temp <- unlist(carbonStacks)
  quickPlot::Plot(temp, title = paste0(poolsToPlot, " in ", years, " MgC/ha"))#addTo = "temp",
}

#' `carbonOutPlot`
#'
#' @param cbmPools TODO
#' @param emissionsProducts TODO
#' @param masterRaster TODO
#'
#' @return TODO
#'
#' @export
#' @importFrom data.table as.data.table melt.data.table
#' @importFrom ggplot2 aes element_text ggplot geom_col labs
#' @importFrom ggplot2 scale_fill_discrete scale_y_continuous sec_axis theme xlab
#' @importFrom quickPlot Plot
carbonOutPlot <- function(cbmPools, emissionsProducts, masterRaster) {
  pixelCount <- cbmPools[, .(simYear, pixelGroup, pixelCount)]
  cols <- c("simYear", "pixelGroup")
  productEmissions <- merge(pixelCount, emissionsProducts,
                            by = cols)
  totalOutByYr <- productEmissions[, .(
    CO2 = sum(CO2 *(prod(res(masterRaster))/10000) * pixelCount),
    CH4 = sum(CH4 *(prod(res(masterRaster))/10000) * pixelCount),
    CO = sum(CO *(prod(res(masterRaster))/10000) * pixelCount),
    Products = sum(Products *(prod(res(masterRaster))/10000) * pixelCount)), by = .(simYear)]
  # Units: these are absolute values the total products and emissions, not per ha.

  # the CH4 and CO are so small compared to the CO2 that we will add all gases
  # and call it emissions
  totalOutByYr[ , Emissions:= (CO2 + CH4 + CO), by = .(simYear)]
  cols <- c("CO2", "CH4", "CO")
  totalOutByYr[ , (cols) := NULL]

  # Emissions only
  # a <- ggplot(data = totalOutByYr, aes(x = simYear)) +
  #   +     geom_line(aes(y = Emissions), size = 1.5, colour = "#69b3a2")
  # Emissions/4 + products
  #a + geom_line(aes(y = Products), size = 1.5, colour = "darkred")

  # emissions seem to be 4X bigger then products
  if(max(totalOutByYr$Emissions) > max(totalOutByYr$Products)){
    coeff <- round(max(totalOutByYr$Emissions)/max(totalOutByYr$Products),digits = 0)
  } else coeff <- round(max(totalOutByYr$Products)/max(totalOutByYr$Emissions),digits = 0)

  absCbyYrPlot <-
    ggplot(data = totalOutByYr, aes(x = simYear, y = Products)) +
    geom_line(colour = "darkred", size = 1.5) +
    geom_line(aes(y = Emissions/coeff), size = 1.5, colour = "#69b3a2") +
    scale_y_continuous(
      # Features of the first axis
      name = "Products in MgC",
      # Add a second axis and specify its features
      sec.axis = sec_axis(trans = ~.*coeff, name = "Emissions (CO2+CH4+CO) in MgC")
    ) +
    xlab("Simulation Years") +
    theme(axis.title.y = element_text(color = "darkred",size = 13),
          axis.title.y.right = element_text(color = "#69b3a2",size = 13, angle = 270))
  #ggtitle("Yearly Emissions and Forest Products")

  quickPlot::Plot(absCbyYrPlot, addTo = "absCbyYrPlot",
                  title = "Yearly Emissions and Forest Products")
}


#' `NPPplot`
#'
#' @param spatialDT TODO
#' @param NPP TODO
#' @param masterRaster TODO
#'
#' @return TODO
#'
#' @export
#' @importFrom data.table copy merge.data.table setkey
#' @importFrom quickPlot Plot
#' @importFrom raster raster
NPPplot <- function(spatialDT, NPP, masterRaster) {
  # Calculate the avgNPP (MgC/ha) by pixel group.
  npp <- as.data.table(copy(NPP))
  npp[,avgNPP := mean(NPP), by = c("pixelGroup")]
  cols <- c("simYear", "NPP")
  avgNPP <- unique(npp[, (cols) := NULL])
  # link that to the pixels
  t <- spatialDT[, .(pixelIndex, pixelGroup)]
  setkey(t,pixelGroup)
  setkey(avgNPP,pixelGroup)
  temp <- merge(t, avgNPP, on = "pixelGroup")
  setkey(temp, pixelIndex)
  #pixelCount[which(is.na(pixelCount$N)),"N"] <- 0
  # temp1 <- temp[which(!is.na(temp$simYear)),.(pixelIndex,NPP)]
  # temp1[order(pixelIndex)]
  #masterRaster[!masterRaster == 0] <- temp$NPP
  plotMaster <- raster(masterRaster)
  plotMaster[] <- 0
  # instead of tC/ha for each pixel,
  plotMaster[temp$pixelIndex] <- temp$avgNPP
  #pixel size in ha
  pixSize <- prod(res(masterRaster))/10000
  temp[,pixNPP := avgNPP*pixSize]
  overallAvgNpp <- sum(temp$pixNPP)/(nrow(temp)*pixSize)
  quickPlot::Plot(plotMaster, new = TRUE,
                  title = paste0("Pixel-level average NPP MgC/ha/yr.",
                                 "\n Landscape average: ", round(overallAvgNpp,3), "  MgC/ha/yr."))
}

#' `barPlot`
#'
#' @param cbmPools TODO
#' @param masterRaster TODO
#'
#' @return TODO
#'
#' @export
#' @importFrom data.table as.data.table
#' @importFrom ggplot2 aes geom_col ggplot labs scale_fill_discrete theme_bw
#' @importFrom quickPlot Plot
barPlot <- function(cbmPools, masterRaster) {
  #This needs to change to belowground living, aboveground living, soil, snag
  colnames(cbmPools)[c(1,3,4)] <- c("simYear", "pixelGroup", "age")
  #Need to first average values per ha
  cbmPools <- as.data.table(cbmPools)
  cbmPools$pixelGroup <- as.character(cbmPools$pixelGroup)
  # this ONLY works is the number of modelled pixels does not change per simulation year
  pixelNo <- sum(cbmPools$pixelCount/length(unique(cbmPools$simYear))) #Get pixel Sum
  cbmPools$simYear <- as.character(cbmPools$simYear)

  carbonCompartments <- cbmPools[, .(soil = sum(AboveGroundVeryFastSoil, BelowGroundVeryFastSoil,
                                                AboveGroundFastSoil, BelowGroundFastSoil,
                                                AboveGroundSlowSoil, BelowGroundSlowSoil, MediumSoil),
                                     AGlive = sum(SoftwoodMerch, SoftwoodFoliage, SoftwoodOther,
                                                  HardwoodMerch, HardwoodFoliage, HardwoodOther),
                                     BGlive = sum(SoftwoodCoarseRoots, SoftwoodFineRoots,
                                                  HardwoodCoarseRoots, HardwoodFineRoots),
                                     snags = sum(SoftwoodStemSnag, SoftwoodBranchSnag,
                                                 HardwoodStemSnag, HardwoodBranchSnag),
                                     weight = pixelCount/pixelNo),
                                 by = .(pixelGroup, simYear)]


  outTable <- carbonCompartments[, .(soil = sum(soil * weight),
                                     AGlive = sum(AGlive * weight),
                                     BGlive = sum(BGlive * weight),
                                     snags = sum(snags * weight)),
                                 by = simYear]

  outTable <- data.table::melt.data.table(outTable, id.vars = 'simYear',
                                          measure.vars = c("soil", "AGlive", "BGlive", "snags"),
                                          variable.name = 'pool',
                                          value.name = "carbon")
  outTable$simYear <- as.numeric(outTable$simYear)
  outTable$carbon <- as.numeric(outTable$carbon)
  barPlots <- ggplot(data = outTable, aes(x = simYear, y = carbon, fill = pool)) +
    geom_col(position = "fill") +
    scale_fill_discrete(name = "carbon compartment") +
    labs(x = "Year", y = "proportion") +
    theme_bw()

  quickPlot::Plot(barPlots, addTo = 'barPlots', title = "Proportion of C above and below ground compartments.")
}
