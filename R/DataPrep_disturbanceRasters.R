
#' CBM data prep: disturbanceRasters
#'
#' Process \code{disturbanceRasters} into a table of \code{disturbanceEvents}
#' for the CBM_core module.
#' Optionally resample to align with a template raster.
#'
#' @param disturbanceRasters terra \code{\link[terra]{SpatRaster}} or list of SpatRaster.
#' Must be named with 4 digit years such that a single raster layer can be accessed
#' for each disturbance year (e.g.  \code{disturbanceRasters[["2025"]]}).
#' @param eventID integer. Disturbance event ID.
#' All non-NA cells will be included as events of this type.
#' If NULL, the raster cell values must be the event IDs.
#' @param templateRast terra \code{\link[terra]{SpatRaster}}.
#' Optional template raster to align rasters with.
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
    disturbanceRasters, eventID = NULL, templateRast = NULL, year = NULL){

  # Set table template
  tableTemplate <- data.table::data.table(
    pixelIndex = integer(0),
    year       = integer(0),
    eventID    = integer(0)
  )

  # If no disturbances provided: return empty table
  if (length(disturbanceRasters) == 0) return(tableTemplate)

  # Read template raster
  if (!is.null(templateRast) && !inherits(templateRast, "SpatRaster")){
    templateRast <- tryCatch(
      terra::rast(templateRast),
      error = function(e) stop(
        "'templateRast' could not be converted to SpatRaster: ", e$message,
        call. = FALSE))
  }

  # Set disturbance years
  distYears <- names(disturbanceRasters)
  if (is.null(distYears) || any(nchar(distYears) != 4)) stop(
    "'disturbanceRasters' must be named by 4 character disturbance year")

  if (!is.null(year)) distYears <- as.character(year)

  # Read disurbances and summarize into a table
  do.call(rbind, lapply(distYears, function(year){

    if (year %in% names(disturbanceRasters)){

      # Get year disturbances
      annualDist <- disturbanceRasters[[year]]

      # Convert to SpatRaster
      if (!is(annualDist, "SpatRaster")){
        annualDist <- tryCatch(
          terra::rast(annualDist),
          error = function(e) stop(
            "'disturbanceRaster' for year ", year, " failed to be read as terra SpatRaster: ",
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
            fun = ifelse(is.null(eventID), "mode", "max")
          ) |> Cache()
        }
      }

      # Get raster values
      rasVals <- terra::values(annualDist)[,1]

      # Summarize events into a table
      if (!is.null(eventID)){

        data.table::data.table(
          pixelIndex = which((rasVals > 0) %in% TRUE),
          year       = as.integer(year),
          eventID    = eventID
        )

      }else{

        # Set event IDs
        eventIDs <- suppressWarnings(tryCatch(
          as.integer(rasVals),
          error = function(e) NULL))
        if (!is.integer(eventIDs)) stop(
          "if eventID is NULL, disturbance raster values must be integer event IDs")

        # Summarize events into a table
        data.table::data.table(
          pixelIndex = as.integer(1:length(rasVals)),
          year       = as.integer(year),
          eventID    = eventIDs
        )[(eventIDs > 0) %in% TRUE,]
      }

    }else tableTemplate
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


