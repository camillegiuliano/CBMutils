#' Create `cumPools` data.table
#'
#' @param fullSpecies DESCRIPTION NEEDED
#' @param gcMeta DESCRIPTION NEEDED
#' @param userGcM3 DESCRIPTION NEEDED
#' @param stable3 DESCRIPTION NEEDED
#' @param stable4 DESCRIPTION NEEDED
#' @param stable5 DESCRIPTION NEEDED
#' @param stable6 DESCRIPTION NEEDED
#' @param stable7 DESCRIPTION NEEDED
#' @param thisAdmin DESCRIPTION NEEDED
#'
#' @return `cumPools` data.table
#'
#' @export
#' @importFrom data.table data.table rbindlist
cumPoolsCreate <- function(fullSpecies, gcMeta, userGcM3,
                           stable3, stable4, stable5, stable6, stable7, thisAdmin) {

  counter <- 0L
  cumBiomList <- list()
  for (i in 1:length(fullSpecies)) {
    # matching on species name
    speciesMeta <- gcMeta[species == fullSpecies[i], ]
    # for each species name, process one gcID at a time
    for (j in 1:NROW(unique(speciesMeta, on = "gcids"))) {
      counter <- counter + 1L

      meta <- speciesMeta[j, ]
      ecozone <- meta$ecozones
      id <- userGcM3$gcids[which(userGcM3$gcids ==  meta$gcids)][-1]
      ## IMPORTANT BOUDEWYN PARAMETERS FOR NOT HANDLE AGE 0 ##
      age <- userGcM3[gcids == meta$gcids, Age]
      age <- age[which(age > 0)]
      # series of fncts results in curves of merch, foliage and other (SW or HW)

      cumBiom <- as.matrix(convertM3biom(
        meta = meta, gCvalues = userGcM3, spsMatch = gcMeta,
        ecozones = thisAdmin, params3 = unique(stable3), params4 = unique(stable4),
        params5 = unique(stable5), params6 = unique(stable6), params7 = unique(stable7)
      ))

      # going from tonnes of biomass/ha to tonnes of carbon/ha here
      cumBiom <- cumBiom * 0.5 ## this value is in sim$cbmData@biomassToCarbonRate
      # calculating the increments per year for each of the three pools (merch,
      # foliage and other (SW or HW))
      # inc <- diff(cumBiom)
      # CBM processes half the growth before turnover and OvermatureDecline, and
      # half after.
      # names(outInputs$allProcesses)
      # [1] "Disturbance"       "Growth1"           "DomTurnover"       "BioTurnover"
      # [5] "OvermatureDecline" "Growth2"           "DomDecay"          "SlowDecay"
      # [9] "SlowMixing"
      cumBiomList[[counter]] <- data.table(id, age, cumBiom, ecozone, gcids = meta$gcids)

      # cumPools <- rbind(cumPools, cumBiom)
    }
  }
  cumPools <- rbindlist(cumBiomList)
  return(cumPools)
}
