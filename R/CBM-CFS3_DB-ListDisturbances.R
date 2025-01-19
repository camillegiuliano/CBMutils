
#' Identify the ID number (CBM-CFS3 legacy) possible in the current spatial unit
#'
#' You give is spatial units you are targeting `mySpu` and it gives you the disturbance matrix id
#' that are possible/default in that specific spu and a descriptive name of that disturbance matrix
#' it creates a `data.frame` of length number of disturbances, with three columns:
#' `spatial_unit_is`, `disturbance_matrix_id`, and a `desciption` of the disturbance.
#'
#' TODO: can we have a Canada-wide SPU map and they locate themselves on the map?
#' this needs to be done before simulations are run so the user can provide this
#' info (location info) for the simulations - Ian is working on this.
#'
#' the function has the defaults from the SK managed forest example.
#' These can be changed by feeding in other SPU.
#'
#' @param mySpu Numeric spatial unit id(s).
#' @param dbPath Path to sqlite database file.
#'
#' @export
#' @importFrom RSQLite dbConnect dbDisconnect dbDriver dbListTables dbReadTable
#' @examples
#' \dontrun{
#'   ## using raster
#'   library(terra)
#'   spuRaster <- rast(file.path("data/forIan/SK_data/CBM_GIS/spUnits_TestArea.tif"))
#'   spatial_unit_id <- values(spuRaster) # 28 27
#'   mySpu <- unique(spatial_unit_id)
#'
#'   ## using growth curves
#'   f <- file.path("spadesCBMinputs/data/SK_ReclineRuns30m/LookupTables/yieldRCBM.csv")
#'   gcIn <- as.matrix(read.csv(f))
#'   mySpu <- unique(gcIn[, 1])
#' }
spuDist <- function(mySpu, dbPath) {

  # connect to database
  sqlite.driver <- dbDriver("SQLite")
  archiveIndex <- dbConnect(sqlite.driver, dbname = dbPath)
  on.exit(dbDisconnect(archiveIndex))

  # get the matrices related tables
  alltables <- dbListTables(archiveIndex)
  matrixTables <- list()
  for (i in 1:length(grep("disturbance", alltables, ignore.case = TRUE))) {
    matrixTables[[i]] <- dbReadTable(archiveIndex, alltables[grep("disturbance", alltables, ignore.case = TRUE)[i]])
  }
  ## There are 6 tables taht have to do with disturbance matrices in the
  ## SQLight. They are described here
  ## https://docs.google.com/spreadsheets/d/1TFBQiRH4z54l8ROX1N02OOiCHXMe20GSaExiuUqsC0Q/edit?usp=sharing

  # match mySpu with the disturbance_matrix_association table which has spu,
  # disturbance_type_id and disturbance_matrix_id
  dmtid <- as.data.table(unique(matrixTables[[2]][which(matrixTables[[2]][, "spatial_unit_id"] %in% mySpu), ]))

  #disturbance_type_id is a more generic ID with general names.
  # Here we keep english names only.
  dist_type_name <- as.data.table(matrixTables[[6]])
  dist_type_name <- dist_type_name[locale_id <= 1,]
  dist_type_name <- dist_type_name[,.(disturbance_type_id, name)]

  # disturbance_matrix_id is region specific IDs and has specific descriptions.
  # Here we keep english descriptions only
  dist_matrix_desc <- as.data.table(matrixTables[[3]])
  dist_matrix_desc <- dist_matrix_desc[locale_id <= 1,]
  dist_matrix_desc <- dist_matrix_desc[,.(disturbance_matrix_id, description)]

  # add the names and descriptions and return all the disturbance_type_id and
  # disturbance_matrix_id that are associated with the provided spatial_unit_id
  # (spu).
  spuDist <- merge(dmtid, dist_type_name, by = "disturbance_type_id")
  spuDist <- merge(spuDist, dist_matrix_desc, by = "disturbance_matrix_id")
  spuDist <- spuDist[,.(disturbance_type_id, spatial_unit_id,
                        disturbance_matrix_id, name, description)]

  return(spuDist)
}

#' Historical disturbances
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
#' @param mySpu Numeric spatial unit id(s).
#'
#' @export
#' @importFrom stats aggregate
histDist <- function(mySpu = c(27, 28)) {
  # used the spuDist() function to narrow down the choice
  a <- spuDist(mySpu)
  # don't need all the cbm_default tables since column 3 of a give you the names
  # this searches for "wildfire"
  # a[which(grepl("wildfire",a[,3],ignore.case = TRUE)),]
  # if there are more then 1 "wildfire" designation, chose the maximum disturbance
  # matrix id number since that would be the most recent in the database
  b <- aggregate(disturbance_matrix_id ~ spatial_unit_id,
                 data = a[which(grepl("wildfire", a[, 3], ignore.case = TRUE)), ], max
  )
  c <- merge.data.frame(a, b)
  return(c)
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
seeDist <- function(distId = c(161, 230, 313, 361),
                    dbPath = file.path("data", "cbm_defaults", "cbm_defaults.db")) {

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


