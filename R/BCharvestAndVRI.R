

# ws3, the harvest optimizer created by Greg Paradis provided the BC disturbance
# rasters (harvest or harvest and fires) per TSA and not for the whole study
# area. This function reads-in the file structure, pulls in all the tsa-level
# rasters, knits them together and postprocesses them to match the raster to
# match.

ws3Build <- function(masterRaster, tsaDirs, years, pathsTifs){
  # this is the table that will get filled with all the firest
  fireDistsDT <- data.table(pixelIndex = integer(), year = integer(), events = integer())
  cutDistsDT <- data.table(pixelIndex = integer(), year = integer(), events = integer())

  for(i in 1:length(years)){
    # the files have the same names and are in the same order in the 5 tsa folders
    distTifs <- grep(years[i], list.files(list.dirs(pathsTifs)[tsaDirs[1]])[-c(1:9)])
    # fire = distTifs[1]
    # cut = distTifs[2]
    fireList <- list()
    cutList <- list()
    for(j in 1:length(tsaDirs)){
      fireList[[j]] <- raster::raster(file.path(list.dirs(pathsTifs)[tsaDirs[j]],
                                                list.files(list.dirs(pathsTifs)[tsaDirs[j]])[-c(1:9)][distTifs[1]]))
      cutList[[j]] <- raster::raster(file.path(list.dirs(pathsTifs)[tsaDirs[j]],
                                               list.files(list.dirs(pathsTifs)[tsaDirs[j]])[-c(1:9)][distTifs[2]]))
    }

    # put the 5 rasters together
    fireList$fun <- mean
    fireList$na.rm <- TRUE
    fireRast0 <- do.call(mosaic, fireList)
    fireRast1yr <- postProcess(fireRast0,
                               rasterToMatch = masterRaster)
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

# this function reads-in the BC VRI, with growth curve information (from ws3),
# and creates a raster stack of the age and gcID.
prepInputsVRI <- function(VRIurl, dPath, rasterToMatch){
  VRIin <- prepInputs(url = VRIurl,
                      fun = "sf::st_read",
                      destinationPath = dPath)
  RIA_VRI <- st_transform(VRIin, crs = st_crs(rasterToMatch))
  gcIDRaster <- fasterize::fasterize(RIA_VRI, rasterToMatch, field = "curve2")
  ageRaster <- fasterize::fasterize(RIA_VRI, rasterToMatch, field = "PROJ_AGE_1")
  gcIDRaster[] <- as.integer(gcIDRaster[])
  ageRaster[] <- as.integer(ageRaster[])
  VRIraster <- raster::stack(gcIDRaster, ageRaster)
  return(VRIraster)
}
