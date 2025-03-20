utils::globalVariables(c(
  "juris_id", "curve_id", ".I", "speciesCode", "canfi_spec", "CanfiCode", "LandR"
))

#' Convert total above ground biomass into 3 pools (\eqn{T/ha})
#'
#' Implements the flowchart from figure 3 of Boudewyn et al. (2007) using an alternative
#' set of parameter to divide total above ground biomass (\eqn{T/ha}) into total merchantable
#' stemwood biomass (\eqn{T/ha}), foliage biomass (\eqn{T/ha}), and other wood biomass (\eqn{T/ha}).
#'
#' @references
#' Boudewyn, P., Song, X., Magnussen, S., & Gillis, M. D. (2007). Model-based, volume-to-biomass
#' conversion for forested and vegetated land in Canada (BC-X-411). Natural Resource Canada,
#' Pacific Forestry Centre. <https://cfs.nrcan.gc.ca/pubwarehouse/pdfs/27434.pdf>
#'
#' @param allInfoAGBin `data.frame` with at least four following columns: `canfi_species`,
#' `ecozone`, `juris_id`, `age`, `B` and a column for pixel group identifier.
#'
#' @param table6 `data.frame` corresponding to Table 3 from Boudewyn et al. (2007),
#' available from <https://nfi.nfis.org/resources/biomass_models/appendix2_table6_tb.csv>.
#'
#' @param table7 `data.frame` corresponding to Table 4 from Boudewyn et al. (2007),
#' available from <https://nfi.nfis.org/resources/biomass_models/appendix2_table7_tb.csv>.
#'
#' @param pixGroupCol the name of the column in `allInfoAGBin` serving as the pixel group
#' identifier.

#' @return biomass (\eqn{T/ha}) in each above ground pool for each cohort per pixel group.
#'
#' @export
#' @importFrom data.table rbindlist setnames
cumPoolsCreateAGB <- function(allInfoAGBin, table6, table7, pixGroupCol){
  counter <- 0L
  cumBiomList <- list()

  expectedColumns <- c("canfi_species", "juris_id", "ecozone", "age", "B", pixGroupCol)

  if (any(!(expectedColumns %in% colnames(allInfoAGBin)))) {
    stop("The AGB table needs the following columns ", paste(expectedColumns, collapse = " "))
  }

  # Identify unique sp, juris_id, ecozone
  curves <- unique(allInfoAGBin[, .(canfi_species, juris_id, ecozone)])
  curves[,curve_id := .I]

  AGB <- merge(allInfoAGBin, curves, by = c("canfi_species", "juris_id", "ecozone"), all.x = TRUE)

  # Do one set of parameters at a time
  for (i_curve in curves$curve_id) {
    counter <- counter + 1L
    oneCurve <- AGB[curve_id == i_curve, ]

    ## IMPORTANT BOURDEWYN PARAMETERS FOR NOT HANDLE AGE 0 ##
    oneCurve <- oneCurve[which(age>0),]

    # Get AGB in each of the 3 pools
    cumBiom <- as.matrix(convertAGB2pools(oneCurve, table6, table7))

    # going from tonnes of biomass/ha to tonnes of carbon/ha here
    ### HARD CODED VALUE ####################
    cumBiom <- cumBiom * 0.5

    cumBiomList[[counter]] <- oneCurve[,
                                       .(speciesCode = speciesCode,
                                         age = age,
                                         pixGroupColValue = get(pixGroupCol))]
    setnames(cumBiomList[[counter]], "pixGroupColValue", pixGroupCol)
    cumBiomList[[counter]] <- cbind(cumBiomList[[counter]],
                                    cumBiom)

  }
  cumPools <- rbindlist(cumBiomList)
  return(cumPools)
}

#' Convert total above ground biomass into 3 pools (\eqn{T/ha})
#'
#' Implements the flowchart from figure 3 of Boudewyn et al. (2007) using an alternative
#' set of parameter to divide total above ground biomass (\eqn{T/ha}) into total merchantable
#' stemwood biomass (\eqn{T/ha}), foliage biomass (\eqn{T/ha}), and other wood biomass (\eqn{T/ha}).
#'
#' @references
#' Boudewyn, P., Song, X., Magnussen, S., & Gillis, M. D. (2007). Model-based, volume-to-biomass
#' conversion for forested and vegetated land in Canada (BC-X-411). Natural Resource Canada,
#' Pacific Forestry Centre. <https://cfs.nrcan.gc.ca/pubwarehouse/pdfs/27434.pdf>
#'
#' @param oneCurve `data.frame` with at least four following columns: `canfi_species`,
#' `ecozone`, `juris_id`, and `B`.
#'
#' @param table6 `data.frame` corresponding to Table 3 from Boudewyn et al. (2007),
#' available from <https://nfi.nfis.org/resources/biomass_models/appendix2_table6_tb.csv>.
#'
#' @param table7 `data.frame` corresponding to Table 4 from Boudewyn et al. (2007),
#' available from <https://nfi.nfis.org/resources/biomass_models/appendix2_table7_tb.csv>.

#' @return three-column matrix with columns corresponding to biomass (\eqn{T/ha}) for
#' total merchantable, foliage, and other wood.
#'
#'
#' @export
convertAGB2pools <- function(oneCurve, table6, table7){

  # get the parameters
  EquatParams <- getParameters(
    table6,
    table7,
    unique(oneCurve$canfi_species),
    unique(oneCurve$ecozone),
    unique(oneCurve$juris_id)
  )
  params6 <- EquatParams$params6
  params7 <- EquatParams$params7

  # get the proportions of each pool
  pVect <- biomProp(table6 = params6, table7 = params7, x = oneCurve$B, type = "biomass")
  totTree <-  oneCurve$B
  totalStemWood <- totTree * pVect[, 1]

  ##TODO
  # find actual data on the proportion of totTree that is merch
  # Problem: CBM currently uses "merch" and "other" as C-pools. In these
  # equations (this function that matches the Boudewyn et al 2007 workflow),
  # totalStemwood is the sum of totMerch (eq1), b_n (eq2[,1] - stem wood biomass
  # of live, nonmerchantable-sized trees) and b_s (eq3 - stem wood biomass of
  # live, sapling-sized trees). The "merch" and the "other" C-pool requires us
  # to know the proportion of totalStemWood that is "merch" and "other"
  ##### IMPORTANT HARD CODING INFORMATION #######
  ## current fix: using the same parameters as FORCS (Forest Carbon Succession
  ## Extension V3.1). Eq 1 on p20 is PropStem = a *(1-b^Age) where a is 0.7546
  ## and b is 0.983. FORCS also sets a minimum merchantable age per species.
  ## Because we are in the RIA, I am setting that at 15. This needs to be a
  ## parameter either from LandR or set by the user (by provinces by species? -
  ## this is usually a diamter not an age)

  ### HARD CODED minimum merchantable age, a, b
  minMerchAge <-  15
  a <- 0.7546
  b <- 0.983

  # if age < MinMerchAge, the propMerch is 0, otherwise use FORCS, until we find actual data.
  propMerch <- (oneCurve$age >= minMerchAge) * a * (1-b^oneCurve$age)

  merch <- propMerch * totalStemWood

  # otherStemWood is everything that is not totMerch
  otherStemWood <- totalStemWood - merch

  bark <- totTree * pVect[, 2]
  branch <- totTree * pVect[, 3]
  foliage <- totTree * pVect[, 4]
  other <- branch + bark + otherStemWood
  biomCumulative <- as.matrix(cbind(merch, foliage, other))
  return(biomCumulative)
}

#' Extract the parameters to apply to convert total biomass into pool biomass
#'
#' Extract the species- and location- specific parameters for equation 4-7 of
#' Boudewyn et al. (2007). If there is no match for the given ecozone, the parameters
#' for a different ecozone in the same province/territory is returned. If there
#' is no match for a given province/territory, the parameters for a different
#' province/territory in the same ecozone is returned. If there is no match for
#' the given ecozone and province/territory, the parameters for a different location
#' is returned.
#'
#' @references
#' Boudewyn, P., Song, X., Magnussen, S., & Gillis, M. D. (2007). Model-based, volume-to-biomass
#' conversion for forested and vegetated land in Canada (BC-X-411). Natural Resource Canada,
#' Pacific Forestry Centre. <https://cfs.nrcan.gc.ca/pubwarehouse/pdfs/27434.pdf>
#'
#' @param table6 `data.frame` corresponding to Table 3 from Boudewyn et al. (2007),
#' available from <https://nfi.nfis.org/resources/biomass_models/appendix2_table6_tb.csv>.
#'
#' @param table7 `data.frame` corresponding to Table 4 from Boudewyn et al. (2007),
#' available from <https://nfi.nfis.org/resources/biomass_models/appendix2_table7_tb.csv>.
#'
#' @param canfi_species the canfi code of the species
#'
#' @param ecozone the code of the ecozone
#'
#' @param juris_id the 2-letter code for the province/territory

#' @return a list with 2 vectors for the parameters in table6 and table7 respectively.
getParameters <- function(table6, table7, canfi_species, ecozone, juris_id){
  spec <- as.integer(canfi_species)
  ez <- ecozone
  admin <- juris_id

  if(!(spec %in% table6$canfi_spec) | !(spec %in% table7$canfi_spec)){
    stop("There are no parameters available for species ", spec)
  }

  params6 <- table6[canfi_spec == spec & ecozone == ez & juris_id == admin,][1]
  params7 <- table7[canfi_spec == spec & ecozone == ez & juris_id == admin,][1]

  if(any(is.na(params6))){
    missing_species <- LandR::sppEquivalencies_CA[CanfiCode == spec, LandR][1]
    params6 <- table6[canfi_spec == spec & ecozone == ez,][1]
    params7 <- table7[canfi_spec == spec & ecozone == ez,][1]
    if(any(is.na(params6))) {
      params6 <- table6[canfi_spec == spec & juris_id == admin,][1]
      params7 <- table7[canfi_spec == spec & juris_id == admin,][1]

      if(any(is.na(params6))) {
        params6 <- table6[canfi_spec == spec,][1]
        params7 <- table7[canfi_spec == spec,][1]

        message("No parameters for species ", missing_species, " in ecozone ",
                ez, " and juridiction ", juris_id, " using parameters of species ",
                missing_species, "in ecozone ", params6$ecozone, " and juridiction ",
                params6$juris_id, ".")
      } else {
        message("No parameters for species ", missing_species, " in ecozone ",
                ez, " using parameters of species ", missing_species, "in ecozone ",
                params6$ecozone, " and the same juridictions.")
      }
    } else {
      message("No parameters for species ", missing_species, " in  juridiction ",
              juris_id, " using parameters of species ", missing_species,
              "the same ecozone, but in juridiction ", params6$juris_id, ".")
    }
  }
  return(out = list(params6 = params6,
                    params7 = params7))
}
