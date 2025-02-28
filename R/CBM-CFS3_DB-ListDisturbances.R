
#' CBM-CFS3 Disturbances Match
#'
#' Match disturbance names with CBM-CFS3 spatial unit disturbances.
#'
#' @param distTable \code{data.table} with columns 'spatial_unit_id' and 'name' (or 'distName').
#' The name column will be matched with disturbance names and descriptions
#' in the CBM-CFS3 database.
#' @param ask logical.
#' If TRUE, prompt the user to choose the correct disturbance matches.
#' If FALSE, the function will look for exact name matches.
#' @param nearMatches logical. Allow for near matches; e.g. "clearcut" can match "clear-cut".
#' @param dbPath Path to CBM-CFS3 SQLite database file
#' @param localeID CBM-CFS3 locale_id
#' @param listDist data.table. Optional. Result of a call to \code{\link{spuDist}}.
#' A list of possible disturbances in the spatial unit(s) with columns
#' 'spatial_unit_id', 'disturbance_type_id', 'disturbance_matrix_id', 'name', 'description'.
#' If provided, the \code{dbPath} and \code{localeID} arguments are not required.
#'
#' @return \code{data.table} with columns 'spatial_unit_id'
#' 'disturbance_type_id', 'disturbance_matrix_id', 'name', 'description'
#'
#' @export
#' @importFrom data.table copy data.table
#' @importFrom knitr kable
#' @importFrom RSQLite dbConnect dbDisconnect dbDriver dbListTables dbReadTable
spuDistMatch <- function(distTable, ask = interactive(), nearMatches = TRUE,
                         dbPath = NULL, localeID = 1, listDist = NULL){

  # Check input
  if (!inherits(distTable, "data.table")){
    distTable <- tryCatch(
      data.table(distTable),
      error = function(e) stop(
        "'distTable' could not be converted to data.table: ", e$message, call. = FALSE))
  }

  reqCols <- c("spatial_unit_id", "name")
  if (!"name" %in% names(distTable) & "distName" %in% names(distTable)){
    names(distTable) <- gsub("^distName$", "name", names(distTable))
  }
  if (!all(reqCols %in% names(distTable))) stop(
    "'distTable' must have the following columns: ",
    paste(shQuote(reqCols), collapse = ", "))

  # List possible spatial disturbances for the spatial units
  if (is.null(listDist)){

    listDist <- spuDist(spuIDs = distTable$spatial_unit_id, dbPath = dbPath, localeID = localeID)

  }else{
    reqCols <- c("spatial_unit_id", "disturbance_type_id", "name", "description")
    if (!all(reqCols %in% names(listDist))) stop(
      "listDist' must have the following columns: ",
      paste(shQuote(reqCols), collapse = ", "))
  }

  # For each disturbance: choose a CBM-CFS3 match
  distMatch <- list()
  for (i in 1:nrow(distTable)){

    spuID    <- distTable[i,]$spatial_unit_id
    distName <- distTable[i,]$name

    if (!ask){

      # Find only identical or near matches to name
      distMatches <- .spuDistMatches(
        spuID       = spuID,
        distName    = distName,
        listDist    = listDist,
        name        = TRUE,
        desc        = FALSE,
        identical   = TRUE,
        nearMatches = nearMatches
      )

      if (nrow(distMatches) != 1) stop(
        nrow(distMatches),
        " disturbance matches found for spatial_unit_id ", spuID, " ",
        "and disturbance name ", shQuote(distName), ". ",
        "Try rerunning with ask = TRUE ",
        "or use the spuDist function to review disturbance options.")

      distMatch[[i]] <- distMatches

    }else{

      # Helper function: prompt user to choose a match
      .spuDistMatchSelect <- function(distMatches, chooseID = "disturbance_type_id"){

        printTable <- distMatches[, intersect(
          c("disturbance_type_id", "sw_hw", "disturbance_matrix_id", "name", "description"),
          names(distMatches)), with = FALSE]

        repeat{

          ans <- readline(cat(paste(c(
            "",
            "Input disturbance information:",
            paste("  Spatial unit ID  :", spuID),
            paste("  Disturbance name :", shQuote(distName)),
            sapply(setdiff(names(distTable), c("spatial_unit_id", "name")), function(col){
              sprintf("  %-16s : %s", col, distTable[i,][[col]])
            }),
            "",
            "CBM-CFS3 disturbance(s) with a matching name or description:",
            knitr::kable(printTable[, setdiff(names(printTable), "description"), with = FALSE], format = "pipe"),
            "",
            crayon::yellow(
              "Enter the correct", chooseID,
              "or \"desc\" to view disturbance descriptions: ")
          ), collapse = "\n")))

          if (identical(trimws(tolower(ans)), "desc")){
            ans <- readline(cat(paste(c(
              knitr::kable(printTable, format = "pipe"),
              "",
              crayon::yellow("Enter the correct ", chooseID, ": ")
            ), collapse = "\n")))
          }

          userSelectID <- suppressWarnings(tryCatch(as.numeric(trimws(ans)), error = function(e) NULL))

          if (isTRUE(userSelectID %in% distMatches[[chooseID]])){
            return(distMatches[distMatches[[chooseID]] == userSelectID,])
          }
        }
      }

      # Find all matches to name or description
      distMatches <- .spuDistMatches(
        spuID       = spuID,
        distName    = distName,
        listDist    = listDist,
        name        = TRUE,
        desc        = TRUE,
        identical   = FALSE,
        nearMatches = nearMatches
      )

      if (nrow(distMatches) == 0) stop(
        "Disturbance match options not found ",
        "for spatial_unit_id ", spuID, " ",
        "and disturbance name ", shQuote(distName), ". ",
        "Use the spuDist function to review disturbance options.")

      # Prompt user to subset matches by disturbance_type_id
      distMatches <- .spuDistMatchSelect(distMatches, "disturbance_type_id")

      # Prompt user to subset matches by disturbance_matrix_id
      if ("disturbance_matrix_id" %in% names(distMatches) && nrow(distMatches) > 1){
        distMatches <- .spuDistMatchSelect(distMatches, "disturbance_matrix_id")
      }

      distMatch[[i]] <- distMatches
    }
  }

  do.call(rbind, distMatch)
}

# Subset CBM-CFS3 disturbances by matches to a spatial_unit_id and a disturbance name.
# @param name logical. Match name with listDist 'name' column
# @param desc logical. Match name with listDist 'description' column
# @param identical logical. Require matches to name and/or description to be identical.
# @param nearMatches logical. Allow for near matches e.g. "clearcut" can match "clear-cut".
.spuDistMatches <- function(
    listDist, spuID, distName,
    name = TRUE, desc = TRUE, identical = FALSE, nearMatches = TRUE){

  # Check input: listDist
  reqCols <- c("spatial_unit_id", "name", "description")
  if (!all(reqCols %in% names(listDist))) stop(
    "listDist' must have the following columns: ",
    paste(shQuote(reqCols), collapse = ", "))

  # Add additional matches for equivalent strings
  strEquivs <- list(
    `clearcut` = c("clear cut", "clear-cut"),
    `wildfire` = c("wild fire", "wild-fire")
  )
  distNameUser <- c(distName, if (nearMatches) strEquivs[[distName]])

  # Subset disturbances by spatial unit ID
  spuDists <- subset(data.table::copy(listDist), spatial_unit_id == spuID)
  if (nrow(spuDists) == 0) stop(
    "'listDist' does not contain any disturbances for spatial unit ", spuID)

  # Standardize strings for matching
  distNameUser <- trimws(tolower(distNameUser))
  distNameCBM  <- trimws(tolower(spuDists$name))
  distDescCBM  <- trimws(tolower(spuDists$description))

  # Check for matches
  distMatch <- list()

  .whichIdentical <- function(charVect) unname(which(sapply(charVect, function(char) any(char %in% distNameUser))))
  .whichPartial   <- function(charVect) unname(which(
    sapply(charVect, function(char) any(sapply(distNameUser, function(nm) grepl(nm, char, fixed = TRUE))))
  ))

  # Match disturbance name
  if (name){

    # Identical match to name
    distMatch[["nameIdentical"]] <- .whichIdentical(distNameCBM)

    if (!identical){
      distMatch[["namePartial"]] <- .whichPartial(distNameCBM)
    }
  }

  # Match disturbance description
  if (desc){

    # Identical match to desc
    distMatch[["descIdentical"]] <- .whichIdentical(distDescCBM)

    if (!identical){
      distMatch[["descPartial"]] <- .whichPartial(distDescCBM)
    }
  }

  # Return matches
  spuDists[unique(unlist(distMatch)),]

}


#' CBM-CFS3 Spatial Unit Disturbances
#'
#' Identify the disturbances possible in spatial units.
#'
#' @param dbPath Path to CBM-CFS3 SQLite database file
#' @param spuIDs Optional. Subset by spatial unit ID(s)
#' @param localeID CBM-CFS3 locale_id
#' @param disturbance_matrix_association data.frame. Optional.
#' Alternative disturbance_matrix_association table with columns
#' "spatial_unit_id", "disturbance_type_id", and "disturbance_matrix_id".
#'
#' @return \code{data.table} with 'disturbance_type_tr' columns
#' "spatial_unit_id", "disturbance_type_id", "name", "description"
#' and 'disturbance_matrix_association' columns
#' "spatial_unit_id" and "disturbance_matrix_id"
#'
#' @export
#' @importFrom data.table data.table
#' @importFrom RSQLite dbConnect dbDisconnect dbDriver dbListTables dbReadTable
spuDist <- function(dbPath, spuIDs = NULL, localeID = 1,
                    disturbance_matrix_association = NULL) {

  if (length(dbPath) != 1) stop("length(dbPath) must be == 1")

  # Connect to database
  cbmDBcon <- dbConnect(dbDriver("SQLite"), dbname = dbPath)
  on.exit(dbDisconnect(cbmDBcon))

  # Read database tables
  ## Read more about the 6 tables related to disturbance matrices here:
  ## https://docs.google.com/spreadsheets/d/1TFBQiRH4z54l8ROX1N02OOiCHXMe20GSaExiuUqsC0Q
  cbmTableNames <- c(
    "disturbance_type_tr",
    if (is.null(disturbance_matrix_association)) "disturbance_matrix_association")

  cbmDB <- list()
  for (cbmTableName in cbmTableNames) {
    cbmDB[[cbmTableName]] <- dbReadTable(cbmDBcon, cbmTableName) |> data.table()
  }

  if (!is.null(disturbance_matrix_association)){
    cbmDB[["disturbance_matrix_association"]] <- data.table(disturbance_matrix_association)
  }

  # Merge and return
  spuDist <- cbmDB[["disturbance_matrix_association"]][
    subset(cbmDB[["disturbance_type_tr"]], locale_id == localeID),
    on = .(disturbance_type_id = disturbance_type_id), nomatch = NULL]

  if (!is.null(spuIDs)) spuDist <- subset(spuDist, spatial_unit_id %in% spuIDs)

  return(
    spuDist[, intersect(
      c("spatial_unit_id", "disturbance_type_id", "sw_hw", "disturbance_matrix_id", "name", "description"),
      names(spuDist)),
      with = FALSE]
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
#' @param spuIDs Spatial unit ID(s)
#' @param dbPath Path to CBM-CFS3 SQLite database file
#' @param localeID CBM-CFS3 locale_id
#' @param listDist data.table. Optional. Result of a call to \code{\link{spuDist}}.
#' A list of possible disturbances in the spatial unit(s) with columns
#' 'spatial_unit_id', 'disturbance_type_id', 'disturbance_matrix_id', 'name', 'description'.
#' If provided, the \code{dbPath} and \code{localeID} arguments are not required.
#' @param ask logical.
#' If TRUE, prompt the user to choose the correct disturbance matches.
#' If FALSE, the function will look for exact name matches.
#'
#' @export
histDist <- function(spuIDs, dbPath = NULL, localeID = 1, listDist = NULL, ask = FALSE) {

  if (length(spuIDs) < 1) stop("length(spuIDs) must be >= 1")

  # Set disturbance name matches
  histDistName <- list(`1` = "Wildfire")
  if (!as.character(localeID) %in% names(histDistName)) stop(
    "CBMutils::histDist does not support finding historical disturbances for locale_id ",
    localeID, " (yet).")

  # Return matching records
  spuDistMatch(
    data.frame(spatial_unit_id = spuIDs, name = histDistName[[as.character(localeID)]]),
    dbPath = dbPath, localeID = localeID, listDist = NULL,
    ask = ask
  )
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


