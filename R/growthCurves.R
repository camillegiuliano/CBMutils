utils::globalVariables(c(
  "a1", "a2", "a3", "b1", "b2", "b3", "c1", "c2", "c3",
  "Age", "canfi_species", "eco", "ecozone", "genus", "gcids",
  "spatialUnitID", "species", "speciesName"
))

#' Calculate stemwood biomass (per ha) of live merchantable trees
#'
#' Implements equation 1 of Boudewyn et al. (2007) to determines the total stemwood biomass of
#' merchantable trees (in metric tonnes per hectare; \eqn{T/ha}),
#' using parameters \eqn{a} and \eqn{b} from Table 3 (`table3`).
#'
#' @references
#' Boudewyn, P., Song, X., Magnussen, S., & Gillis, M. D. (2007). Model-based, volume-to-biomass
#' conversion for forested and vegetated land in Canada (BC-X-411). Natural Resource Canada,
#' Pacific Forestry Centre. <https://cfs.nrcan.gc.ca/pubwarehouse/pdfs/27434.pdf>
#'
#' @param table3 `data.frame` corresponding to Table 3 from Boudewyn et al. (2007),
#' available from <https://nfi.nfis.org/resources/biomass_models/appendix2_table3.csv>.
#'
#' @param vol gross merchantable volume per hectare (\eqn{m^3/ha})
#'
#' @return stemwood biomass of merchantable trees (\eqn{b_m} in units \eqn{T/ha})
#'
#' @export
b_m <- function(table3, vol) {
  # flag if vol in growth curve is above the max vol the model was developed on
  if (!is.na(unique(table3$volm))) {
    if (max(vol) > unique(table3$volm)) {
      message("The volumes in the growth information provided are greater than the maximum volume ",
              "the stem wood model was developed with.")
    }
  }
  b_m <- unique(table3$a) * vol ^ unique(table3$b)
  return(b_m)
}

#' Expansion factor for non-merchantable live tree biomass
#'
#' Implements equation 2 of Boudewyn et al. (2007), used to determine the total stem wood biomass
#' (in metric tonnes per hectare; \eqn{T/ha}) of non-merchantable trees (\eqn{B_n}), together
#' with the stemwood biomass of live merchantable and non-merchantable trees (\eqn{B_{nm}}),
#' using parameters \eqn{a}, \eqn{b}, and \eqn{k} from Table 4 (`table4`).
#'
#' @references
#' Boudewyn, P., Song, X., Magnussen, S., & Gillis, M. D. (2007). Model-based, volume-to-biomass
#' conversion for forested and vegetated land in Canada (BC-X-411). Natural Resource Canada,
#' Pacific Forestry Centre. <https://cfs.nrcan.gc.ca/pubwarehouse/pdfs/27434.pdf>
#'
#' @param table4 `data.frame` corresponding to Table 4 from Boudewyn et al. (2007),
#' available from <https://nfi.nfis.org/resources/biomass_models/appendix2_table4.csv>.
#'
#' @param eq1 stemwood biomass of merchantable trees (\eqn{T/ha}) from equation 1 of
#' Boudewyn et al. (2007) (i.e., the result of [b_m()]).
#'
#' @param vol gross merchantable volume per hectare (\eqn{m^3/ha})
#'
#' @return two-column matrix with columns corresponding to \eqn{b_n} and \eqn{b_{nm}}
#'
#' @export
nmfac <- function(table4, eq1, vol) {
  # flag if vol in growth curve is above the max vol the model was developed on
  if (!is.na(unique(table4$volm))) {
    if (max(vol) > unique(table4$volm)) {
      message("The volumes in the growth information provided are greater than the maximum volume ",
              "the non-merch sized tree model was developed with.")
    }
  }
  nmFac <- unique(table4$k) + (unique(table4$a) * eq1 ^ unique(table4$b))
  # caps on non-merch trees provided in table 4
  nmFac[which(nmFac > table4$cap)] <- unique(table4$cap)
  b_nm <- nmFac * eq1
  b_n <- b_nm - eq1
  return(cbind(b_n = b_n, b_nm = b_nm))
}

#' Expansion factor for sapling-sized trees
#'
#' Implements equation 3 of Boudewyn et al. (2007), used to determine the total stem wood biomass
#' (in metric tonnes per hectare; \eqn{T/ha}) of sapling-sized trees (\eqn{B_s}),
#' using parameters \eqn{a}, \eqn{b}, and \eqn{k} from Table 5 (`table5`).
#'
#' @references
#' Boudewyn, P., Song, X., Magnussen, S., & Gillis, M. D. (2007). Model-based, volume-to-biomass
#' conversion for forested and vegetated land in Canada (BC-X-411). Natural Resource Canada,
#' Pacific Forestry Centre. <https://cfs.nrcan.gc.ca/pubwarehouse/pdfs/27434.pdf>
#'
#' @param table5 `data.frame` corresponding to Table 5 from Boudewyn et al. (2007),
#' available from <https://nfi.nfis.org/resources/biomass_models/appendix2_table5.csv>.
#'
#' @param eq2 two-column matrix giving stemwood biomass of non-merchantable trees
#' (i.e., \eqn{b_n} given in units \eqn{T/ha}), and merchantable + non-merchantable trees
#' (i.e., \eqn{b_{nm}} given in units \eqn{T/ha}), from equation 2 of Boudewyn et al. (2007)
#' (i.e., the result of [nmfac()]).
#'
#' @param vol gross merchantable volume per hectare (\eqn{m^3/ha})
#'
#' @return stemwood biomass of sapling-sized trees (\eqn{b_s} in units \eqn{T/ha})
#'
#' @export
sapfac <- function(table5, eq2, vol){
  # flag if vol in growth curve is above the max vol the model was developed on
  if (!is.na(unique(table5$volm))) {
    if (max(vol) > unique(table5$volm)) {
      message("The volumes in the growth information provided are greater than the maximum volume ",
              "the sappling model was developed with.")
    }
  }
  # caps on sapling fraction provided in table5
  sapFac <- unique(table5$k) + (unique(table5$a) * eq2[, 2] ^ unique(table5$b))
  sapFac[which(sapFac > table5$cap)] <- unique(table5$cap)
  b_snm <- sapFac * eq2[, 2]
  b_s <- b_snm - eq2[, 2]
  return(b_s)
}

#' Proportions of total tree biomass in stemwood, bark, branches, and foliage
#'
#' Implements equations 4-7 of Boudewyn et al. (2007), used to determine the proportions
#' of total tree biomass in stemwood, bark, branches, and foliage
#' (\eqn{p_{stemwood}}, \eqn{p_{bark}}, \eqn{p_{branches}}, \eqn{p_{foliage}}, respectively),
#' using parameters \eqn{a}, \eqn{b} from Table 6 (`table6`) and volume-proportion caps
#' from Table 7 (`table7`).
#'
#' TODO: will eventually add species, ecozone
#'
#' @references
#' Boudewyn, P., Song, X., Magnussen, S., & Gillis, M. D. (2007). Model-based, volume-to-biomass
#' conversion for forested and vegetated land in Canada (BC-X-411). Natural Resource Canada,
#' Pacific Forestry Centre. <https://cfs.nrcan.gc.ca/pubwarehouse/pdfs/27434.pdf>
#'
#' @param table6 `data.frame` corresponding to Table 6 from Boudewyn et al. (2007),
#' available from <https://nfi.nfis.org/resources/biomass_models/appendix2_table6.csv>.
#' The alternative table 6 for equations using total biomass as independent variable
#' is available from <https://nfi.nfis.org/resources/biomass_models/appendix2_table6_tb.csv>.
#'
#' @param table7 `data.frame` corresponding to Table 7 from Boudewyn et al. (2007),
#' available from <https://nfi.nfis.org/resources/biomass_models/appendix2_table7.csv>.
#' The alternative table 7 for equations using total biomass as independent variable
#' is available from <https://nfi.nfis.org/resources/biomass_models/appendix2_table7_tb.csv>.
#'
#' @param x `vector` gross merchantable volume per hectare (\eqn{m^3/ha}) or
#' total biomass (\eqn{tonnes/ha})
#'
#' @param type `character` specifies if the `x` represents gross merchantable
#' volume per hectare ("volume") or total biomass ("biomass").
#'
#' @return four-column matrix will columns corresponding to \eqn{p_{stemwood}}, \eqn{p_{bark}},
#' \eqn{p_{branches}}, and \eqn{p_{foliage}}
#'
#' @export
biomProp <- function(table6, table7, x, type = "volume") {
  if (type == "volume"){
    if(any(!(c("vol_min", "vol_max") %in% colnames(table7)))) {
      stop("The parameter tables do not have the correct columns for ", type, " inputs.")
    }
    caps <- as.numeric(table7[1 ,c("vol_min", "vol_max")])
  } else if (type == "biomass") {
    if(any(!(c("biom_min", "biom_max") %in% colnames(table7)))) {
      stop("The parameter tables do not have the correct columns for ", type, " inputs.")
    }
    caps <- as.numeric(table7[1 ,c("biom_min", "biom_max")])
  } else {
    stop("The argument type in biomProp() needs to be `volume` or `biomass`")
  }

  # flag if vol in below vol_min or above vol_max (when not NA)
  # the model was developed on
  # DC 2025-03-07: ONLY FOR VOLUME. MUTED FOR BIOMASS BECAUSE IT HAPPENS ALL THE
  # TIME WHEN CREATING YIELD TABLES FROM LANDR
  if (length(is.na(unique(caps[1]))) > 0 & type == "volume") {
    testVec <- min(x) < unique(caps[1])
    if (any(testVec)) {
      message("Some volumes in the growth information provided are smaller than ",
              "the minimum volume the proportions model was developed with.")
    }
  }

  if (length(is.na(unique(caps[2]))) > 0 & type == "volume") {
    testVec <- max(x) > unique(caps[2])
    if (any(testVec)) {
      message("Some volumes in the growth information provided are larger than ",
              "the maximum volume the proportions model was developed with.")
    }
  }


  lvol <- log(x + 5)

 ## denominator is the same for all 4 equations
  denom <- (1 + exp(table6[1, a1] + table6[1, a2] * x + table6[1, a3] * lvol) +
              exp(table6[1, b1] + table6[1, b2] * x + table6[1, b3] * lvol) +
              exp(table6[1, c1] + table6[1, c2] * x + table6[1, c3] * lvol))
  ## for each proportion, enforce caps per table 7
  pstem <- 1 / denom
  pstem[which(x < caps[1])] <- table7[1, p_sw_low]
  pstem[which(x > caps[2])] <- table7[1, p_sw_high]

  pbark <- exp(table6[1, a1] + table6[1, a2] * x + table6[1, a3] * lvol) / denom
  pbark[which(x < caps[1])] <- table7[1, p_sb_low]
  pbark[which(x > caps[2])] <- table7[1, p_sb_high]

  pbranches <- exp(table6[1, b1] + table6[1, b2] * x + table6[1, b3] * lvol) / denom
  pbranches[which(x < caps[1])] <- table7[1, p_br_low]
  pbranches[which(x > caps[2])] <- table7[1, p_br_high]

  pfol <- exp(table6[, c1] + table6[1, c2] * x + table6[1, c3] * lvol) / denom
  pfol[which(x < caps[1])] <- table7[1, p_fl_low]
  pfol[which(x > caps[2])] <- table7[1, p_fl_high]

  propVect <- cbind(pstem = pstem, pbark = pbark, pbranches = pbranches, pfol = pfol)

  if(any(abs(rowSums(propVect) - 1) > 0.001)) {
    stop("The sums of biomass proportions do not sum to 1...")
  }

  return(propVect)
}

#' Calculate biomass from gross merchantable volume
#'
#' Implements the flowchart from figure 3 of Boudewyn et al. (2007) to determined the
#' total above ground biomass (\eqn{T/ha}) from gross merchantable volume (\eqn{m^3/ha}).
#'
#' @references
#' Boudewyn, P., Song, X., Magnussen, S., & Gillis, M. D. (2007). Model-based, volume-to-biomass
#' conversion for forested and vegetated land in Canada (BC-X-411). Natural Resource Canada,
#' Pacific Forestry Centre. <https://cfs.nrcan.gc.ca/pubwarehouse/pdfs/27434.pdf>
#'
#' @param meta DESCRIPTION NEEDED
#'
#' @param gCvalues DESCRIPTION NEEDED
#'
#' @param spsMatch DESCRIPTION NEEDED
#'
#' @param ecozones DESCRIPTION NEEDED
#'
#' @param params3 `data.frame` corresponding to Table 3 from Boudewyn et al. (2007),
#' available from <https://nfi.nfis.org/resources/biomass_models/appendix2_table3.csv>.
#'
#' @param params4 `data.frame` corresponding to Table 4 from Boudewyn et al. (2007),
#' available from <https://nfi.nfis.org/resources/biomass_models/appendix2_table4.csv>.
#'
#' @param params5 `data.frame` corresponding to Table 5 from Boudewyn et al. (2007),
#' available from <https://nfi.nfis.org/resources/biomass_models/appendix2_table5.csv>.
#'
#' @param params6 `data.frame` corresponding to Table 6 from Boudewyn et al. (2007),
#' available from <https://nfi.nfis.org/resources/biomass_models/appendix2_table6.csv>.
#'
#' @param params7 `data.frame` corresponding to Table 7 from Boudewyn et al. (2007),
#' available from <https://nfi.nfis.org/resources/biomass_models/appendix2_table7.csv>.
#'
#' @return three-column matrix with columns corresponding to biomass (\eqn{T/ha}) for
#' total merchantable, foliage, and other.
#'
#' @export
convertM3biom <- function(meta, gCvalues, spsMatch, ecozones, params3, params4, params5, params6,
                          params7) {

  oneCurve <- gCvalues[gcids == meta$gcids, ]
  # the Boudewyn models do not deal with 0s
  oneCurve <- oneCurve[Age != 0,]
  spec <- unique(spsMatch[species == meta$species, ]$canfi_species)
  ## might have to put in a loop here for each ecozone?
  ez <- ecozones[SpatialUnitID == meta$spatial_unit_id, ]$EcoBoundaryID
  gen <- unique(spsMatch[species == meta$species, ]$genus)

  params3 <- params3[canfi_species == spec & ecozone == ez,]
  params4 <- params4[canfi_species == spec & ecozone == ez,]
  # table 5 is different than the others
  params5 <- params5[genus == gen & ecozone == ez,]
  params6 <- params6[canfi_species == spec & ecozone == ez,]
  params7 <- params7[canfi_species == spec & ecozone == ez,]
  # Equations are numbered following the flowchart of the biomass model application in
  # Boudewyn et al. 2007 p7 (Fig3)
  # eq1 returns the total stem wood biomass in metric tonnes/ha, when you give it
  # the gross merchantable volume/ha. Parameters a and b are in table3
  eq1 <- b_m(params3, oneCurve$MerchVolume)
  # eq2 returns a two column matrix giving the biomass of the non-merch sized
  # trees (b_n) and b_nm which is the sum of the total stem wood biomass of merch size
  # live plus, the stem wood live of non merch-sized trees, given the total
  # stem wood biomass per ha of live merch size trees (in tonnes/ha)
  eq2 <- nmfac(params4, eq1 = eq1, vol = oneCurve$MerchVolume)
  # eq3 is for biomass of the saplings, the smallest of the non-merch trees. The
  # non-merch biomass from eq2, is needed. eq3 returns b_s, stem wood biomass of
  # live sapling-sized trees in tonnes/ha
  eq3 <- sapfac(params5, eq2 = eq2, vol = oneCurve$MerchVolume)
  #eq3[which(is.na(eq3))] <- 0
  # middle box flowchart3: total stem wood biomass (tonnes) /ha for all live trees

  totalStemWood <- eq1 + eq2[,1] + eq3
  totalStemWood[which(is.nan(totalStemWood))] <- NA
  # calculate the 4 proportions that should be returned: proportion for
  # stemwood, prop for bark, prop for branches, and prop for foliage.
  pVect <- biomProp(table6 = params6, table7 = params7, x = oneCurve$MerchVolume)
  # translating this into biomass values for the carbon pools
  totMerch <- eq1
  totTree <- totalStemWood / pVect[, 1]
  bark <- totTree * pVect[, 2]
  branch <- totTree * pVect[, 3]
  fol <- totTree * pVect[, 4]
  other <- branch + bark + eq2[, 1] + eq3
  biomCumulative <- as.matrix(cbind(totMerch = totMerch, fol = fol, other = other))
  return(biomCumulative)
}
