#' `prepInputsEcozones`
#'
#' @param url A URL to the data
#' @param dPath destination path
#' @template rasterToMatch
#'
#' @export
#' @importFrom fasterize fasterize
#' @importFrom reproducible prepInputs
#' @importFrom sf st_as_sf st_crop st_crs st_transform
prepInputsEcozones <- function(url = NULL, dPath, rasterToMatch) {
  .Defunct("LandR::prepEcozonesRst")
}

#' `prepInputsVRI`
#'
#' Read in the BC VRI, with growth curve information (from `ws3`), and creates a raster stack of
#' the age and `gcID`.
#'
#' @inheritParams prepInputsEcozones
#' @export
#' @importFrom fasterize fasterize
#' @importFrom raster stack
#' @importFrom sf st_read st_transform
prepInputsVRI <- function(url, dPath, rasterToMatch) {
  VRIin <- prepInputs(url = url,
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

#' Read in the BC VRI, with growth curve information (from `ws3`), and creates a raster stack of
#' the age
#'
#' @param VRIurl a URL to the data
#' @param dPath destination path
#' @template rasterToMatch
#' @param targetFile A `gdb.zip` file that is the inventory file.
#' @param field The age column name in the inventory file.
#'
#' @export
#' @importFrom fasterize fasterize
#' @importFrom raster crs crs<- stack
#' @importFrom reproducible prepInputs
#' @importFrom sf st_crs st_read st_transform
prepInputsVRIage <- function(VRIurl, dPath, rasterToMatch, targetFile, field = "PROJ_AGE_1") {
  sa <- as(extent(rasterToMatch), "SpatialPolygons")
  crs(sa) <- crs(rasterToMatch)
  VRIin <- prepInputs(url = VRIurl,
                      targetFile = targetFile,
                      archive = NA,
                      fun = NA,
                      destinationPath = dPath,
                      studyArea = sa)
  vriAge2015 <- sf::st_read(file.path(dPath, targetFile))
  RIA_VRI <- sf::st_transform(vriAge2015, st_crs(sa))
  # RIA_VRI <- st_transform(VRIin, crs = st_crs(rasterToMatch))
  ageRaster <- fasterize::fasterize(RIA_VRI, rasterToMatch, field = "PROJ_AGE_1")
  ageRaster[] <- as.integer(ageRaster[])
  mrWdata <- !is.na(rasterToMatch)
  ageRaster[!mrWdata] <- NA
  return(ageRaster)
}
