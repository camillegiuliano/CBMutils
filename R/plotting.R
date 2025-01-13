utils::globalVariables(c(
  "AboveGroundFastSoil", "AboveGroundSlowSoil", "AboveGroundVeryFastSoil", "AGB", "AGlive",
  "BelowGroundFastSoil", "BelowGroundSlowSoil", "BelowGroundVeryFastSoil", "BGB", "BGlive",
  "carbon", "CH4", "CO", "CO2",  "DOM", "Emissions", "emissionsCH4", "emissionsCO", "emissionsCO2",
  "HardwoodBranchSnag", "HardwoodStemSnag", "MediumSoil",
  "pixelCount", "pixNPP", "pool", "products", "Products",
  "res", "snags", "SoftwoodBranchSnag", "SoftwoodStemSnag", "soil", "weight"
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
#' @template masterRaster
#'
#' @return TODO
#'
#' @export
#' @importFrom data.table as.data.table merge.data.table
#' @importFrom quickPlot Plot
#' @importFrom terra rast values
spatialPlot <- function(cbmPools, years, masterRaster, spatialDT) {
  masterRaster <- terra::unwrap(masterRaster)
  cbmPools <- as.data.table(cbmPools)
  totalCarbon <- apply(cbmPools[, Merch:BranchSnag],
                       1, "sum")
  totalCarbon <- cbind(cbmPools, totalCarbon)
  totalCarbon <- totalCarbon[simYear == years,]
  t <- spatialDT[, .(pixelIndex, pixelGroup)]
  setkey(t, pixelGroup)
  setkey(totalCarbon, pixelGroup)
  temp <- merge(t, totalCarbon, allow.cartesian=TRUE)
  setkey(temp, pixelIndex)
  plotM <- terra::rast(masterRaster)
  plotM[] <- 0
  plotM[temp$pixelIndex] <- temp$totalCarbon
  pixSize <- prod(res(masterRaster))/10000
  temp[, `:=`(pixTC, totalCarbon * pixSize)]
  overallTC <- sum(temp$pixTC)/(nrow(temp) * pixSize)
  quickPlot::Plot(plotM, new = TRUE,
                  title = paste0("Total Carbon in ", years, " in MgC/ha"))
}

#' `carbonOutPlot`
#'
#' @param emissionsProducts TODO
#' @template masterRaster
#'
#' @return invoked for side effect of creating plot
#'
#' @export
#' @importFrom data.table as.data.table melt.data.table
#' @importFrom ggplot2 aes element_text geom_col geom_line ggplot labs
#' @importFrom ggplot2 scale_fill_discrete scale_y_continuous sec_axis theme xlab
#' @importFrom quickPlot Plot
carbonOutPlot <- function(emissionsProducts) {
  totalOutByYr <- as.data.table(emissionsProducts)
  cols <- c("CO2", "CH4", "CO")
  totalOutByYr[, `:=`((cols), NULL)]

  absCbyYrProducts <- ggplot(totalOutByYr, aes(x = simYear, y = Products)) +
    geom_line(linewidth = 1.5) +
    scale_y_continuous(name = "Products in MgC") +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    xlab("Simulation Years") + theme_classic() +
    theme(axis.title.y = element_text(size = 10),
          axis.title.x = element_text(size = 10))

  absCbyYrEmissions <- ggplot(data = totalOutByYr, aes(x = simYear, y = Emissions)) +
    geom_line(linewidth = 1.5) +
    scale_y_continuous(limits = c(0, NA)) +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    labs(x = "Simulation Years", y = expression(paste('Emissions (CO'[2]*'+CH'[4]*'+CO) in MgC'))) +
    theme_classic() +
    theme(axis.title.y = element_text(size = 10),
          axis.title.x = element_text(size = 10))


  quickPlot::Plot(absCbyYrProducts, addTo = "absCbyYrProducts",
                  title = "Yearly Forest Products")
  quickPlot::Plot(absCbyYrEmissions, addTo = "absCbyYrEmissions",
                  title = "Yearly Emissions")
}

#' `NPPplot`
#'
#' @param spatialDT TODO
#' @param NPP TODO
#' @template masterRaster
#'
#' @return TODO
#'
#' @export
#' @importFrom data.table copy merge.data.table setkey
#' @importFrom quickPlot Plot
#' @importFrom raster raster
NPPplot <- function(spatialDT, NPP, masterRaster) {
  masterRaster <- terra::unwrap(masterRaster)
  npp <- as.data.table(copy(NPP))
  npp[, `:=`(avgNPP, mean(NPP)), by = c("pixelGroup")]
  cols <- c("simYear", "NPP")
  avgNPP <- unique(npp[, `:=`((cols), NULL)])
  t <- spatialDT[, .(pixelIndex, pixelGroup)]
  setkey(t, pixelGroup)
  setkey(avgNPP, pixelGroup)
  temp <- merge(t, avgNPP, allow.cartesian=TRUE)
  setkey(temp, pixelIndex)
  plotMaster <- terra::rast(masterRaster)
  # plotMaster[] <- 0
  plotMaster[temp$pixelIndex] <- temp$avgNPP
  pixSize <- prod(res(masterRaster))/10000
  temp[, `:=`(pixNPP, avgNPP * pixSize)]
  overallAvgNpp <- sum(temp$pixNPP)/(nrow(temp) * pixSize)
  quickPlot::Plot(plotMaster, new = TRUE,
                  title = paste0("Pixel-level average NPP",
                                 "\n Landscape average: ", round(overallAvgNpp, 3), "  MgC/ha/yr."))
}


#' `barPlot`
#'
#' @param cbmPools TODO
#' @template masterRaster
#'
#' @return TODO
#'
#' @export
#' @importFrom data.table as.data.table melt.data.table
#' @importFrom ggplot2 aes geom_col ggplot labs scale_fill_discrete theme_bw
#' @importFrom quickPlot Plot
barPlot <- function(cbmPools) {
  cbmPools <- as.data.table(cbmPools)
  cbmPools$pixelGroup <- as.character(cbmPools$pixelGroup)
  pixelNo <- sum(cbmPools$pixelCount/length(unique(cbmPools$simYear)))
  cbmPools$simYear <- as.character(cbmPools$simYear)
  carbonCompartments <- cbmPools[, .(soil = sum(AboveGroundVeryFastSoil, BelowGroundVeryFastSoil,
                                                AboveGroundFastSoil, BelowGroundFastSoil,
                                                AboveGroundSlowSoil, BelowGroundSlowSoil, MediumSoil),
                                     AGlive = sum(Merch, Foliage, Other),
                                     BGlive = sum(CoarseRoots,FineRoots),
                                     snags = sum(StemSnag, BranchSnag), weight = pixelCount/pixelNo),
                                 by = .(pixelGroup, simYear)]
  outTable <- carbonCompartments[, .(soil = sum(soil * weight),
                                     AGlive = sum(AGlive * weight),
                                     BGlive = sum(BGlive * weight),
                                     snags = sum(snags * weight)),
                                 by = simYear]
  outTable <- data.table::melt.data.table(outTable, id.vars = "simYear",
                                          measure.vars = c("soil", "AGlive", "BGlive", "snags"),
                                          variable.name = "pool", value.name = "carbon")
  outTable$simYear <- as.numeric(outTable$simYear)
  outTable$carbon <- as.numeric(outTable$carbon)
  barPlots <- ggplot(data = outTable, aes(x = simYear, y = carbon, fill = pool)) +
    geom_col(position = "fill") +
    scale_y_continuous(expand = expansion(mult = c(0, .1))) +
    scale_fill_discrete(name = "Carbon Compartment") +
    labs(x = "Year", y = "Proportion") + theme_classic() +
    guides(fill = guide_legend(title.position= "top", title ="Carbon compartment") ) +
    scale_fill_brewer(palette = "Set1", labels = c("Soil", "AGlive", "BGlive", 'snags'))

  quickPlot::Plot(barPlots, addTo = "barPlots", title = "Proportion of C above and below ground compartments.")
}
