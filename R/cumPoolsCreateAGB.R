## NOTES
#    FORCS parameters are hard coded: minimum merchantable age, a, and b (used
#    to calculate the proportion of merchantable Stemwood)

cumPoolsCreateAGB <- function(allInfoAGBin, table6, table7, pixGroupCol = "poolsPixelGroup"){
  counter <- 0L
  cumBiomList <- list()

  expectedColumns <- c("canfi_species", "juris_id", "ecozone", "age", "B", pixGroupCol)
  if (pixGroupCol == "yieldPixelGroup") {
    expectedColumns <- c(expectedColumns, "cohort_id")
  }
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
    cumBiom <- cumBiom * 0.5 ## this value is in sim$cbmData@biomassToCarbonRate

    # To handle cohortData as well
    if(pixGroupCol != "yieldPixelGroup") cohort_id <- NULL

    cumBiomList[[counter]] <- oneCurve[,
                                       .(gcids = cohort_id,
                                         species = speciesCode,
                                         age = age,
                                         pixGroupColValue = get(pixGroupCol))]  # Use get() to refer to pixGroupCol dynamically
    setnames(cumBiomList[[counter]], "pixGroupColValue", pixGroupCol)
    cumBiomList[[counter]] <- cbind(cumBiomList[[counter]],
                                    cumBiom)

  }
  cumPools <- rbindlist(cumBiomList)
  return(cumPools)
}

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
  pVect <- biomProp2(table6 = params6, table7 = params7, vol = oneCurve$B, type = "biomass")
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

  totMerch <- propMerch * totalStemWood

  # otherStemWood is everything that is not totMerch
  otherStemWood <- totalStemWood - totMerch

  bark <- totTree * pVect[, 2]
  branch <- totTree * pVect[, 3]
  fol <- totTree * pVect[, 4]
  other <- branch + bark + otherStemWood
  biomCumulative <- as.matrix(cbind(totMerch,fol,other))
  return(biomCumulative)
}

biomProp <- function(table6, table7, x, type = "volume") {
  if (type == "volume"){
    if(any(!(c("vol_min", "vol_max") %in% colnames(table7)))) {
      stop("The parameter tables do not have the correct columns for ", type, " inputs.")
    }
    caps <- as.numeric(table7[,c("vol_min", "vol_max")])
  } else if (type == "biomass") {
    if(any(!(c("biom_min", "biom_max") %in% colnames(table7)))) {
      stop("The parameter tables do not have the correct columns for ", type, " inputs.")
    }
    caps <- as.numeric(table7[,c("biom_min", "biom_max")])
  } else {
    stop("The argument type in biomProp() needs to be `volume` or `biomass`")
  }

  # flag if vol in below vol_min or above vol_max (when not NA)
  # the model was developed on
  # DC 2025-03-07: ONLY FOR VOLUME. MUTED FOR BIOMASS BECAUSE IT HAPPENS ALL THE
  # TIME WHEN CREATING YIELD TABLES FROM LANDR
  if (length(is.na(unique(caps[1]))) > 0 & type == "volume") {
    testVec <- min(vol) < unique(caps[1])
    if (any(testVec)) {
      message("Some volumes in the growth information provided are smaller than ",
              "the minimum volume the proportions model was developed with.")
    }
  }

  if (length(is.na(unique(caps[2]))) > 0 & type == "volume") {
    testVec <- max(vol) > unique(caps[2])
    if (any(testVec)) {
      message("Some volumes in the growth information provided are larger than ",
              "the maximum volume the proportions model was developed with.")
    }
  }


  lvol <- log(x + 5)

  ## denominator is the same for all 4 equations
  denom <- (1 + exp(table6[, a1] + table6[, a2] * x + table6[, a3] * lvol) +
              exp(table6[, b1] + table6[, b2] * x + table6[, b3] * lvol) +
              exp(table6[, c1] + table6[, c2] * x + table6[, c3] * lvol))
  ## for each proportion, enforce caps per table 7
  pstem <- 1 / denom
  pstem[which(x < caps[1])] <- table7$p_sw_low
  pstem[which(x > caps[2])] <- table7$p_sw_high

  pbark <- exp(table6[, a1] + table6[, a2] * x + table6[, a3] * lvol) / denom
  pbark[which(x < caps[1])] <- table7$p_sb_low
  pbark[which(x > caps[2])] <- table7$p_sb_high

  pbranches <- exp(table6[, b1] + table6[, b2] * x + table6[, b3] * lvol) / denom
  pbranches[which(x < caps[1])] <- table7$p_br_low
  pbranches[which(x > caps[2])] <- table7$p_br_high

  pfol <- exp(table6[, c1] + table6[, c2] * x + table6[, c3] * lvol) / denom
  pfol[which(x < caps[1])] <- table7$p_fl_low
  pfol[which(x > caps[2])] <- table7$p_fl_high

  propVect <- cbind(pstem = pstem, pbark = pbark, pbranches = pbranches, pfol = pfol)

  if(any(rowSums(propVect) - 1 > 0.01)) {
    stop("The sums of biomass proportions do not sum to 1...")
  }

  return(propVect)
}

getParameters <- function(table6, table7, canfi_species, ecozone, juris_id){
  spec <- as.integer(canfi_species)
  ez <- ecozone
  admin <- juris_id
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
