
#' CBM data prep: disturbanceRasters
#'
#' Process a list of \code{disturbanceRasters} into \code{disturbanceEvents} for the CBM_core module.
#' Optionally resample to align with a template raster.
#'
#' @param disturbanceRastersList list of \code{disturbanceRasters}.
#' Each \code{disturbanceRasters} item must be named by a 4 digit year
#' such that a single terra \code{\link[terra]{SpatRaster}} layer can be accessed
#' for each disturbance year (e.g.  \code{disturbanceRasters[["1"]][["2025"]]}).
#' This can either be a list named with disturbance event IDs such that every
#' non-NA raster value is considered an event of that type,
#' or a 1-length unnamed list of \code{disturbanceRasters} where the raster values
#' contain event IDs.
#' @param templateRast terra \code{\link[terra]{SpatRaster}}.
#' Template raster to align rasters with.
#' @param year digit or character.
#' One or more years to summarize disturbance events for.
#' If NULL, all available years are summarized.
#'
#' @return \code{disturbanceEvents}
#' data.table with integer columns 'pixelIndex', 'year', 'eventID'.
#'
#' @importFrom data.table data.table
#' @importFrom exactextractr exact_resample
#' @importFrom reproducible Cache
#' @importFrom terra compareGeom rast
#' @export
dataPrep_disturbanceRasters <- function(
    disturbanceRastersList, templateRast = NULL, year = NULL){

  # Set table template
  tableTemplate <- data.table::data.table(
    pixelIndex = integer(0),
    year       = integer(0),
    eventID    = integer(0)
  )

  # If no disturbances provided: return empty table
  if (length(disturbanceRastersList) == 0) return(tableTemplate)

  # Check disturbanceRastersList input
  if (!is.list(disturbanceRastersList)) stop("'disturbanceRastersList' must be a list")
  if (is.null(names(disturbanceRastersList)) & length(disturbanceRastersList) > 1) stop(
    "if length('disturbanceRastersList') > 1 it must be named but disturbance event ID")

  distListNames <- lapply(disturbanceRastersList, names)
  if (any(sapply(distListNames, is.null)) ||
      any(do.call(c, lapply(distListNames, nchar)) != 4)) stop(
    "'disturbanceRastersList' items must be named by disturbance year")

  # Set event IDs
  if (!is.null(names(disturbanceRastersList))){
    eventIDs <- suppressWarnings(tryCatch(
      as.integer(names(disturbanceRastersList)),
      error = function(e) NA))
    if (any(is.na(eventIDs))) stop(
      "'disturbanceRastersList' names must match integer event IDs")
  }

  # Read template raster
  if (!is.null(templateRast) && !inherits(templateRast, "SpatRaster")){
    templateRast <- tryCatch(
      terra::rast(templateRast),
      error = function(e) stop(
        "'templateRast' could not be converted to SpatRaster: ", e$message,
        call. = FALSE))
  }

  # Choose years to summarize
  if (is.null(year)) year <- sort(unique(do.call(c, lapply(disturbanceRastersList, names))))

  # Read disurbances and summarize into a table
  do.call(rbind, lapply(year, function(yr){

    do.call(rbind, lapply(1:length(disturbanceRastersList), function(i){

      if (as.character(yr) %in% names(disturbanceRastersList[[i]])){

        # Get year disturbances
        annualDist <- disturbanceRastersList[[i]][[as.character(yr)]]

        # Convert to SpatRaster
        if (!is(annualDist, "SpatRaster")){
          annualDist <- tryCatch(
            terra::rast(annualDist),
            error = function(e) stop(
              "'disturbanceRaster' for year ", yr, " failed to be read as terra SpatRaster: ",
              e$message, call. = FALSE))
        }

        # Align with template raster
        if (!is.null(templateRast)){

          needsAlign <- !terra::compareGeom(
            annualDist, templateRast,
            lyrs = FALSE,
            crs = TRUE, warncrs = FALSE,
            ext = TRUE, rowcol = TRUE, res = TRUE,
            stopOnError = FALSE, messages = FALSE)

          if (needsAlign){

            # assumption: max is faster if values are not required
            annualDist <- exactextractr::exact_resample(
              annualDist, templateRast,
              fun = ifelse(is.null(names(disturbanceRastersList)), "mode", "max")
            ) |> Cache()
          }
        }

        # Get raster values
        rasVals <- as.integer(terra::values(annualDist)[,1])

        # Summarize events into a table
        if (!is.null(names(disturbanceRastersList))){

          data.table::data.table(
            pixelIndex = which((rasVals > 0) %in% TRUE),
            year       = as.integer(yr),
            eventID    = eventIDs[[i]]
          )

        }else{

          # Set event IDs
          eventIDs <- suppressWarnings(tryCatch(
            as.integer(rasVals),
            error = function(e) NULL))
          if (!is.integer(eventIDs)) stop(
            "Disturbance raster values must be integer event IDs")

          # Summarize events into a table
          data.table::data.table(
            pixelIndex = as.integer(1:length(rasVals)),
            year       = as.integer(yr),
            eventID    = eventIDs
          )[(eventIDs > 0) %in% TRUE,]
        }

      }else tableTemplate
    }))
  }))
}


#' CBM data prep: disturbanceRastersURL
#'
#' Process \code{disturbanceRastersURL} into \code{disturbanceRasters}.
#'
#' @param disturbanceRastersURL character.
#' URL of either an archive of raster files or a single raster file.
#' @param bandYears 4 digit numeric or character years.
#' If the URL is of a single raster file,
#' provide the disturbance years that each raster band represents.
#' @param ... additional arguments to reproducible \code{\link[reproducible]{preProcess}}
#'
#' @return \code{disturbanceRasters}.
#' If URL is an archive: a list of terra \code{\link[terra]{SpatRaster}}
#' where each item is named by the disturbance year.
#' If URL is a single raster file: a terra \code{\link[terra]{SpatRaster}}
#' where each raster band layer is named by the disturbance year.
#'
#' @importFrom reproducible Cache preProcess
#' @importFrom terra nlyr rast
#' @export
dataPrep_disturbanceRastersURL <- function(
    disturbanceRastersURL, bandYears = NULL, ...){

  if (!is.null(bandYears)) if (!all(sapply(bandYears, nchar) == 4)) stop(
    "'bandYears' must be 4 character years (e.g. '2024')")

  # Download archive or file
  dlList <- preProcess(
    url = disturbanceRastersURL,
    fun = NA,
    ...
  )

  if (!is.null(bandYears)){

    # If a single file: there must be 1 band per disturbance year
    dlRast <- terra::rast(dlList$targetFilePath)

    if (terra::nlyr(dlRast) != length(bandYears)) stop(
      "'bandYears' is length ", length(bandYears), " but ",
      terra::nlyr(dlRast), " bands found in raster: ",
      dlList$targetFilePath)

    names(dlRast) <- as.character(bandYears)
    dlRast

  }else{

    # Check if target file is an extracted archive
    if (file.info(dlList$targetFilePath)$isdir){
      archiveDir <- dlList$targetFilePath
    }else if (dirname(dlList$targetFilePath) != dlList$destinationPath){
      archiveDir <- dirname(dlList$targetFilePath)
      while (dirname(archiveDir) != dlList$destinationPath){
        archiveDir <- dirname(archiveDir)
      }
    }else stop("URL did not retrieve an archive. ",
               "If URL is a single raster file, provide the 'bandYears' argument.")

    # List files by year
    archiveFiles <- list.files(archiveDir, recursive = TRUE, full.names = TRUE)

    dlInfo <- data.frame(
      path = archiveFiles,
      name = tools::file_path_sans_ext(basename(archiveFiles)),
      ext  = tolower(tools::file_ext(archiveFiles)),
      size = file.size(archiveFiles)
    )
    dlInfo$year_regexpr <- regexpr("[0-9]{4}", dlInfo$name)
    dlInfo$year <- sapply(1:nrow(dlInfo), function(i){
      if (dlInfo[i,]$year_regexpr != -1){
        paste(
          strsplit(dlInfo[i,]$name, "")[[1]][0:3 + dlInfo[i,]$year_regexpr],
          collapse = "")
      }else NA
    })

    if (all(is.na(dlInfo$year))) stop(
      "Disturbance raster file(s) from 'disturbanceRasterURL' must be named with 4-digit years")
    dlInfo <- dlInfo[!is.na(dlInfo$year),, drop = FALSE]

    # Choose file type to use for each year
    drYears <- unique(sort(dlInfo$year))
    distRast <- sapply(drYears, function(drYear){

      ## CRAN bind variables
      year <- ext <- year <- NULL

      drInfoYear <- subset(dlInfo, year == drYear)
      if (nrow(drInfoYear) > 1){
        if ("grd" %in% drInfoYear$ext) return(subset(drInfoYear, ext == "grd")$path)
        drInfoYear$path[drInfoYear$size == max(drInfoYear$size)][[1]]
      }else drInfoYear$path
    })
    names(distRast) <- drYears

    # Read as SpatRaster
    lapply(distRast, terra::rast)
  }
}


