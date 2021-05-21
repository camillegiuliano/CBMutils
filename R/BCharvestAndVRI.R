utils::globalVariables("events")

#' `ws3Build`
#'
#' `ws3`, the harvest optimizer created by Greg Paradis provided the BC disturbance rasters
#' (harvest or harvest and fires) per TSA and not for the whole study area.
#' This function reads-in the file structure, pulls in all the TSA-level rasters,
#' knits them together and post-processes them to match the `rasterToMatch`.
#'
#' @param masterRaster `RasterLayer` object akin to `rasterToMatch` (i.e., the template raster)
#' @param tsaDirs character vector of paths to the TSA directories
#' @param years numeric or integer vector specifying the years for the data
#' @param pathsTifs character vector of paths for the geotiff files
#'
#' @export
#' @importFrom data.table data.table rbindlist
#' @importFrom raster mosaic ncell raster
#' @importFrom reproducible postProcess
#'
ws3Build <- function(masterRaster, tsaDirs, years, pathsTifs) {
  # this is the table that will get filled with all the fires
  fireDistsDT <- data.table(pixelIndex = integer(), year = integer(), events = integer())
  cutDistsDT <- data.table(pixelIndex = integer(), year = integer(), events = integer())

  for (i in 1:length(years)) {
    # the files have the same names and are in the same order in the 5 tsa folders
    distTifs <- grep(years[i], list.files(list.dirs(pathsTifs)[tsaDirs[1]])[-c(1:9)])
    # fire = distTifs[1]
    # cut = distTifs[2]
    fireList <- list()
    cutList <- list()
    for (j in 1:length(tsaDirs)) {
      fireList[[j]] <- raster::raster(file.path(
        list.dirs(pathsTifs)[tsaDirs[j]],
        list.files(list.dirs(pathsTifs)[tsaDirs[j]])[-c(1:9)][distTifs[1]]
      ))
      cutList[[j]] <- raster::raster(file.path(
        list.dirs(pathsTifs)[tsaDirs[j]],
        list.files(list.dirs(pathsTifs)[tsaDirs[j]])[-c(1:9)][distTifs[2]]
      ))
    }

    # put the 5 rasters together
    fireList$fun <- mean
    fireList$na.rm <- TRUE
    fireRast0 <- do.call(raster::mosaic, fireList)
    fireRast1yr <- postProcess(fireRast0, rasterToMatch = masterRaster)
    fireDT1yr <- data.table(pixelIndex = 1:ncell(fireRast1yr), year = years[i], events = fireRast1yr[])
    fireDistsDT <- rbindlist(list(fireDistsDT,fireDT1yr[!is.na(events)]))

    cutList$fun <- mean
    cutList$na.rm <- TRUE
    cutRast0 <- do.call(mosaic, cutList)
    cutRast1yr <- postProcess(cutRast0,
                              rasterToMatch = masterRaster)
    cutDT1yr <- data.table(pixelIndex = 1:ncell(cutRast1yr), year = years[i], events = cutRast1yr[])
    # cut events = 2
    cutDT1yr[!is.na(events)]$events <- 2
    cutDistsDT <- rbindlist(list(cutDistsDT,cutDT1yr[!is.na(events)]))
  }

  distList <- rbindlist(list(fireDistsDT, cutDistsDT))
  return(distList)
}
