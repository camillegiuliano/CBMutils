utils::globalVariables(c(
  "AGB", "BGB", "carbon", "DOM", "emissionsCH4", "emissionsCO", "emissionsCO2",
  "pixelCount", "pool", "products", "res", "weight"
))

#' spatialPlot
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
    totalCarbon <- apply(cbmPools[,SoftwoodMerch:HardwoodBranchSnag], 1, 'sum')
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
  quickPlot::Plot(temp, addTo = "temp", title = paste0(poolsToPlot, " in ", years, " MgC/ha"))
}

#' carbonOutPlot
#'
#' @param cbmPools TODO
#' @param masterRaster TODO
#'
#' @return TODO
#'
#' @export
#' @importFrom data.table as.data.table melt.data.table
#' @importFrom ggplot2 aes ggplot geom_col labs scale_fill_discrete theme_bw
#' @importFrom quickPlot Plot
carbonOutPlot <- function(cbmPools, emissionsProducts, masterRaster) {

  pixelCount <- cbmPools[,.(simYear, pixelGroup, pixelCount)]
  cols <- c("simYear", "pixelGroup")
  productEmissions <- merge(pixelCount,emissionsProducts, by = cols)

  # get total emissions (right now it is carbon/ha for a pixelGroup)
  totalOutByYr <- productEmissions[, .(Products = sum(Products*(prod(res(masterRaster))/10000)*pixelCount),
                                       CH4 = sum(CH4*(prod(res(masterRaster))/10000)*pixelCount),
                                       CO = sum(CO*(prod(res(masterRaster))/10000)*pixelCount),
                                       CO2 = sum(CO2*(prod(res(masterRaster))/10000)*pixelCount)),
                                   by = .(simYear)]

  coeff <- 1000

  carbonOutPlot <- ggplot(data = totalOutByYr, aes(x = simYear)) +
    geom_col(aes(y = Products/coeff), size = 0.1, fill = "#69b3a2") +
    geom_line(aes(y = CH4), size=1.5, colour = "darkred") +
    geom_line(aes(y = CO), size=1.5, colour = "steelblue", linetype = "twodash") +
    geom_line(aes(y = CO2), size=1.5, colour = "forestgreen", linetype = "dotted") +
    scale_y_continuous(

      # Features of the first axis
      name = "Emissions in MgC",

      # Add a second axis and specify its features
      sec.axis = sec_axis(~.*coeff, name="Products in kgC")
    ) +
    xlab("Simulation Years") +
    #theme_bw() + labs(colour = "Legend") + scale_colour_manual(values = colours) +

    theme(
      axis.title.y = element_text(color = "black", size=13),
      axis.title.y.right = element_text(color = "#69b3a2", size=13, angle = 270),
      legend.position = c(1, 1)# TODO can't get legend to work
    )

  quickPlot::Plot(carbonOutPlot, addTo = 'carbonOutPlot', title = "Yearly Emissions and Forest Products")
}


#' NPPplot
#'
#' @param spatialDT TODO
#' @param changeInNPP TODO
#' @param masterRaster TODO
#' @param time TODO
#'
#' @return TODO
#'
#' @export
#' @importFrom data.table merge.data.table setkey
#' @importFrom quickPlot Plot
#' @importFrom raster raster
NPPplot <- function(spatialDT, NPP, masterRaster){
  # Calculate the avgNPP (MgC/ha) by pixel group.
  NPP[,avgNPP := mean(NPP), by = c("pixelGroup")]
  cols <- c("simYear", "NPP")
  avgNPP <- unique(NPP[, (cols) := NULL])
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
  quickPlot::Plot(plotMaster, new = TRUE, title = paste0("Pixel-level average NPP MgC/ha/yr.",
                                                         "\n Landscape average: ", round(overallAvgNpp,3), "  MgC/ha/yr."))
}


#' barPlot
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

