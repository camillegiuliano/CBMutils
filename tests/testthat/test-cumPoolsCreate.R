table6vol <- reproducible::prepInputs(url = "https://nfi.nfis.org/resources/biomass_models/appendix2_table6.csv",
                                      fun = "data.table::fread",
                                      destinationPath = "inputs",
                                      filename2 = "appendix2_table6.csv")
table7vol <- reproducible::prepInputs(url = "https://nfi.nfis.org/resources/biomass_models/appendix2_table7.csv",
                                      fun = "data.table::fread",
                                      destinationPath = "inputs",
                                      filename2 = "appendix2_table7.csv")
table6AGB <- reproducible::prepInputs(url = "https://nfi.nfis.org/resources/biomass_models/appendix2_table6_tb.csv",
                                   fun = "data.table::fread",
                                   destinationPath = "inputs",
                                   filename2 = "appendix2_table6_tb.csv")
table7AGB <- reproducible::prepInputs(url = "https://nfi.nfis.org/resources/biomass_models/appendix2_table7_tb.csv",
                                   fun = "data.table::fread",
                                   destinationPath = "inputs",
                                   filename2 = "appendix2_table7_tb.csv")

test_that("testing biomProp function", {
  params6 <- table6vol[juris_id == 'NS' & ecozone == 7 & canfi_species == 2804,][1] # Prunus virginiana
  params7 <- table7vol[juris_id == 'NS' & ecozone == 7 & canfi_species == 2804,][1]
  vol <- round(runif(10, 0, 800))
  vol[c(1:3)] <- c(0.1, 250, 750) # make sure to test eq, and under/over caps

  # with volume as input
  expect_message(biomProp(params6, params7, vol))
  prop <- biomProp(params6, params7, vol)
  prop1 <- biomProp(params6, params7, vol, type = "volume")
  expect_equal(prop, prop1)
  expect_true(all(abs(rowSums(prop) - 1) < 0.001))
  expect_equal(round(prop[1,],2),
               c(pstem = 0.58, pbark = 0.15, pbranches = 0.21, pfol = 0.07)
               ) # expected from table 7
  expect_equal(round(prop[2,],2),
               c(pstem = 0.64, pbark = 0.08, pbranches = 0.25, pfol = 0.03)
               ) # expected result by hand
  expect_equal(round(prop[3,],2),
               c(pstem = 0.67, pbark = 0.06, pbranches = 0.25, pfol = 0.02)
               ) # expected from table 7
  expect_error(biomProp(params6, params7, vol, type = "biomass"))
  expect_error(biomProp(params6, params7, vol, type = "notacorrectype"))

  # with AGB as input
  params6 <- table6AGB[juris_id == 'BC' & ecozone == 9 & canfi_spec == 105,] # white spruce
  params7 <- table7AGB[juris_id == 'BC' & ecozone == 9 & canfi_spec == 105,]
  B <- round(runif(10, 0, 1000))
  B[c(1:3)] <- c(1, 100, 500) # make sure to test eq, and under/over caps
  expect_error(biomProp(params6, params7, B))
  expect_error(biomProp(params6, params7, B, type = "volume"))
  prop <- biomProp(params6, params7, B, type = "biomass")
  expect_true(all(abs(rowSums(prop) - 1) < 0.001))
  expect_equal(round(prop[1,],2),
               c(pstem = 0.31, pbark = 0.05, pbranches = 0.32, pfol = 0.32)) # expected from table 7
  expect_equal(round(prop[2,],2),
               c(pstem = 0.67, pbark = 0.09, pbranches = 0.14, pfol = 0.09)) # expected result by hand
  expect_equal(round(prop[3,],2),
               c(pstem = 0.79, pbark = 0.10, pbranches = 0.08, pfol = 0.04)) # expected from table 7
})

test_that("testing getParameters function", {
  out <- getParameters(table6AGB, table7AGB, 101, 4, "BC")

  expect_is(out, "list")
  expect_named(out, c("params6", "params7"))
  expect_true(all(
    out$params6[,c("juris_id", "ecozone", "canfi_spec", "genus", "species")] ==
      data.table::data.table("BC", 4L, 101L, "PICE", "MAR")))
  expect_true(all(
    out$params7[,c("juris_id", "ecozone", "canfi_spec", "genus", "species")] ==
      data.table::data.table("BC", 4L, 101L, "PICE", "MAR")))
  expect_equal(out$params7$p_sb_high, 0.11207112)
  expect_equal(out$params6$c2, 0.0012709)

  expect_message(getParameters(table6AGB, table7AGB, 101, 99999, "BC"))
  expect_message(getParameters(table6AGB, table7AGB, 101, 4, "notaprovince"))
  expect_error(getParameters(table6AGB, table7AGB, 1, 4, "BC"))
})
