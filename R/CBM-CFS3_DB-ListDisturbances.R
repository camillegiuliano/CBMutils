
#' CBM-CFS3 Spatial Unit Disturbances
#'
#' Identify the disturbances possible in spatial units.
#'
#' @param spuIDs Spatial unit ID(s)
#' @param dbPath Path to CBM-CFS3 SQLite database file
#' @param localeID CBM-CFS3 locale_id
#'
#' @return \code{data.table} with columns
#' 'spatial_unit_id', 'disturbance_type_id', 'disturbance_matrix_id',
#' 'name', 'description'
#'
#' @export
#' @importFrom data.table data.table
#' @importFrom RSQLite dbConnect dbDisconnect dbDriver dbListTables dbReadTable
spuDist <- function(spuIDs, dbPath, localeID = 1) {

  if (length(spuIDs) <  1) stop("length(spuIDs) must be >= 1")
  if (length(dbPath) != 1) stop("length(dbPath) must be == 1")

  # Connect to database
  cbmDBcon <- dbConnect(dbDriver("SQLite"), dbname = dbPath)
  on.exit(dbDisconnect(cbmDBcon))

  # Read database tables
  ## Read more about the 6 tables related to disturbance matrices here:
  ## https://docs.google.com/spreadsheets/d/1TFBQiRH4z54l8ROX1N02OOiCHXMe20GSaExiuUqsC0Q
  cbmTableNames <- c(
    "disturbance_type", "disturbance_type_tr",
    "disturbance_matrix", "disturbance_matrix_tr", "disturbance_matrix_value",
    "disturbance_matrix_association"
  )

  cbmDB <- list()
  for (cbmTableName in cbmTableNames) {
    cbmDB[[cbmTableName]] <- dbReadTable(cbmDBcon, cbmTableName) |> data.table()
  }

  # match spuIDs with the disturbance_matrix_association table which has spu,
  # disturbance_type_id and disturbance_matrix_id
  dist_matrix_assoc <- unique(subset(cbmDB[["disturbance_matrix_association"]], spatial_unit_id %in% spuIDs))

  # disturbance_type_id is a more generic ID with general names.
  dist_type_name <- subset(
    cbmDB[["disturbance_type_tr"]], locale_id == localeID
  )[,.(disturbance_type_id, name)]

  # disturbance_matrix_id is region specific IDs and has specific descriptions.
  dist_matrix_desc <- subset(
    cbmDB[["disturbance_matrix_tr"]], locale_id == localeID
  )[,.(disturbance_matrix_id, description)]

  # add the names and descriptions and return all the disturbance_type_id and
  # disturbance_matrix_id that are associated with the provided spatial_unit_id
  # (spu).
  spuDist <- dist_matrix_assoc |>
    merge(dist_type_name,   by = "disturbance_type_id") |>
    merge(dist_matrix_desc, by = "disturbance_matrix_id")

  return(
    spuDist[,.(
      spatial_unit_id, disturbance_type_id, disturbance_matrix_id,
      name, description
    )]
  )
}


#' CBM-CFS3 Historical Disturbances
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
#' @param spuID Spatial unit ID(s)
#' @param dbPath Path to CBM-CFS3 SQLite database file
#' @param localeID CBM-CFS3 locale_id
#' @param listDist data.table. Optional. Result of a call to \code{\link{spuDist}}.
#' A list of possible disturbances in the spatial unit(s) with columns
#' 'spatial_unit_id', 'disturbance_type_id', 'disturbance_matrix_id', 'name', 'description'.
#' If provided, the \code{dbPath} and \code{localeID} arguments are not required.
#'
#' @export
#' @importFrom stats aggregate
histDist <- function(spuIDs, dbPath = NULL, localeID = 1, listDist = NULL) {

  # Set disturbance name matches
  histDistName <- list(`1` = "Wildfire")
  if (!as.character(localeID) %in% names(histDistName)) stop(
    "CBMutils::histDist does not support finding historical disturbances for locale_id ",
    localeID, " (yet).")

  # List possible spatial disturbances for the spatial units
  if (is.null(listDist)){

    listDist <- spuDist(spuIDs = spuIDs, dbPath = dbPath, localeID = localeID)

  }else{

    reqCols <- c("spatial_unit_id", "disturbance_type_id", "disturbance_matrix_id",
                 "name", "description")
    if (!all(reqCols %in% names(listDist))) stop(
      "listDist' must have the following columns: ",
      paste(shQuote(reqCols), collapse = ", "))

    if (!all(spuIDs %in% listDist$spatial_unit_id)) stop(
      "'listDist' does not contain any disturbances for spatial unit(s) ",
      paste(shQuote(setdiff(spuIDs, listDist$spatial_unit_id)),
            collapse = ", "))
  }

  # If there are more then 1 "wildfire" designation, chose the maximum disturbance
  # matrix id number since that would be the most recent in the database
  histDist <- do.call(rbind, lapply(spuIDs, function(spuID){
    subset(
      subset(listDist, spatial_unit_id == spuID &
               tolower(name) %in% tolower(histDistName[[as.character(localeID)]])),
      disturbance_matrix_id = max(disturbance_matrix_id))
  }))
  if (!all(spuIDs %in% histDist$spatial_unit_id)) stop(
    shQuote(histDistName[[as.character(localeID)]]),
    " disturbance(s) not found for spatial unit(s): ",
    paste(setdiff(spuIDs, histDist$spatial_unit_id), collapse = ", "))

  return(histDist)
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
#' @importFrom RSQLite dbConnect dbDisconnect dbDriver dbListTables dbReadTable
seeDist <- function(distId, dbPath) {

  # connect to database
  sqlite.driver <- dbDriver("SQLite")
  cbmDefaults <- dbConnect(sqlite.driver, dbname = dbPath)
  on.exit(dbDisconnect(cbmDefaults))

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


