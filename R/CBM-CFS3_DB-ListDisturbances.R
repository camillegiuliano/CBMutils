
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
spuDistMatch <- function(distTable, ask = interactive(),
                         dbPath = NULL, localeID = 1, listDist = NULL){

  # Check input
  if (!inherits(distTable, "data.table")){
    distTable <- tryCatch(
      data.table(distTable),
      error = function(e) stop(
        "'distTable' could not be converted to data.table: ", e$message, call. = FALSE))
  }

  reqCols <- c("spatial_unit_id", "name")
  if (!"name" %in% names(distTable)){
    names(distTable) <- gsub("^distName$", "name", names(distTable))
  }
  if (!all(reqCols %in% names(distTable))) stop(
    "'distTable' must have the following columns: ",
    paste(shQuote(reqCols), collapse = ", "))

  # List possible spatial disturbances for the spatial units
  if (is.null(listDist)){

    listDist <- spuDist(spuIDs = distTable$spatial_unit_id, dbPath = dbPath, localeID = localeID)

  }else{

    reqCols <- c("spatial_unit_id", "disturbance_type_id", "disturbance_matrix_id",
                 "name", "description")
    if (!all(reqCols %in% names(listDist))) stop(
      "listDist' must have the following columns: ",
      paste(shQuote(reqCols), collapse = ", "))

      if (!all(distTable$spatial_unit_id %in% listDist$spatial_unit_id)) stop(
        "'listDist' does not contain any disturbance for spatial unit(s) ",
        paste(shQuote(setdiff(distTable$spatial_unit_id, listDist$spatial_unit_id)),
              collapse = ", "))
  }

  # Create tables for easy matching by standardizing some equivalent strings standardized
  strEquivs <- list(
    `clearcut` = c("clear cut", "clear-cut"),
    `wildfire` = c("wild fire", "wild-fire")
  )

  # Helper function: prepare fields for matching
  .prepMatch <- function(str){
    str <- trimws(tolower(str))
    for (strReplace in names(strEquivs)){
      for (strPattern in strEquivs[[strReplace]]){
        str <- gsub(strPattern, strReplace, str, fixed = TRUE)
      }
    }
    str
  }

  matchUser <- data.table::copy(distTable)
  matchDist <- cbind(rowID = 1:nrow(listDist), data.table::copy(listDist)[, .(spatial_unit_id, name, description)])

  matchUser[, name        := .prepMatch(name)]
  matchDist[, name        := .prepMatch(name)]
  matchDist[, description := .prepMatch(description)]

  # For each disturbance: choose a CBM-CFS3 match
  distMatch <- list()
  for (i in 1:nrow(distTable)){

    if (!ask){

      # Find identical matches to name
      matchIdx <- c(

        # Identical match
        which(
          sapply(listDist$name, identical, distTable[i,]$name) &
            listDist$spatial_unit_id == distTable[i,]$spatial_unit_id
        ),

        # Nearly identical match
        which(
          sapply(matchDist$name, identical, matchUser[i,]$name) &
            matchDist$spatial_unit_id == matchUser[i,]$spatial_unit_id
        )
      )

      if (length(matchIdx) == 0) stop(
        "Disturbance match not found ",
        "for spatial_unit_id ", distTable[i,]$spatial_unit_id, " ",
        "and disturbance name ", shQuote(distTable[i,]$name), ". ",
        "Try rerunning with ask = TRUE ",
        "or use the listDist function to review disturbance options.")

      distMatch[[i]] <- listDist[matchIdx[[1]],]

    }else{

      # Subset disturbances by spatial unit
      spuMatches <- subset(matchDist, spatial_unit_id == distTable[i,]$spatial_unit_id)

      # Check for disturbances containing the provided name in "name" or "description"
      ## List these in order of their best rank
      matchIdx <- spuMatches$rowID[unique(c(

        # Identical match to name
        which(sapply(spuMatches$name,        identical, matchUser[i,]$name)),

        # Identical match to description
        which(sapply(spuMatches$description, identical, matchUser[i,]$name)),

        # Partial match to name
        which(grepl(matchUser[i,]$name, spuMatches$name, fixed = TRUE)),

        # Partial match to description
        which(grepl(matchUser[i,]$name, spuMatches$description, fixed = TRUE))

      ))]

      if (length(matchIdx) == 0) stop(
        "Disturbance match options not found ",
        "for spatial_unit_id ", distTable[i,]$spatial_unit_id, " ",
        "and disturbance name ", shQuote(distTable[i,]$name), ". ",
        "Use the listDist function to review disturbance options.")

      # Prompt user to select match
      printTable <- listDist[matchIdx, .(disturbance_matrix_id, disturbance_type_id, name, description)]

      repeat{

        ans <- readline(cat(paste(c(
          "",
          "Choose a CBM-CFS3 disturbance matrix ID match for:",
          paste("  Spatial unit ID  :", distTable[i,]$spatial_unit_id),
          paste("  Disturbance name :", shQuote(distTable[i,]$name)),
          sapply(setdiff(names(distTable), c("spatial_unit_id", "name")), function(col){
            sprintf("  %-16s : %s", col, distTable[i,][[col]])
          }),
          "",
          "CBM-CFS3 disturbance(s) with a matching name or description:",
          knitr::kable(printTable[,1:3], format = "pipe"),
          "",
          crayon::yellow(
            "Enter the correct disturbance_matrix_id",
            "or \"desc\" to view disturbance descriptions: ")
        ), collapse = "\n")))

        if (identical(trimws(tolower(ans)), "desc")){
          ans <- readline(cat(paste(c(
            knitr::kable(printTable, format = "pipe"),
            "",
            crayon::yellow("Enter the correct disturbance_matrix_id: ")
          ), collapse = "\n")))
        }

        userSelectID <- suppressWarnings(tryCatch(as.numeric(trimws(ans)), error = function(e) NULL))

        if (isTRUE(userSelectID %in% listDist[matchIdx,]$disturbance_matrix_id)){
          break
        }
      }

      distMatch[[i]] <- subset(listDist, disturbance_matrix_id == userSelectID)
    }
  }

  do.call(rbind, distMatch)
}


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


