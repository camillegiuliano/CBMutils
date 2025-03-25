
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

table6AGB <- reproducible::prepInputs(url = "https://nfi.nfis.org/resources/biomass_models/appendix2_table6_tb.csv",
                                      fun = "data.table::fread",
                                      destinationPath = testDirs$temp$inputs,
                                      filename2 = "appendix2_table6_tb.csv")
table7AGB <- reproducible::prepInputs(url = "https://nfi.nfis.org/resources/biomass_models/appendix2_table7_tb.csv",
                                      fun = "data.table::fread",
                                      destinationPath = testDirs$temp$inputs,
                                      filename2 = "appendix2_table7_tb.csv")

test_that("getParameters", {
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

test_that("convertAGB2pools", {
  dt <- data.table(
    expand.grid(canfi_species = c(204), # PINU_CON
                age = c(3,15, 35),
                ecozone = 4,
                juris_id = "AB"
    )
  )
  dt$B <- round(runif(nrow(dt), 1, 100))
  out <- convertAGB2pools(dt, table6 = table6AGB, table7 = table7AGB)

  #sum of the pools equal total AGB
  expect_equal(rowSums(out), dt$B)
  expect_true(out[dt$age < 15, "merch"] ==  0)
  expect_true(all(colnames(out) == c("merch", "foliage", "other")))
  expect_true(all(!is.na(out)))
  expect_equal(dim(out), c(3,3))

})

test_that("cumPoolsCreateAGB", {
  dt <- data.table(
    expand.grid(canfi_species = c(204, 1201), # PINU_CON, POPU_TRE
                age = c(3,15, 35),
                ecozone = 4,
                juris_id = "AB",
                poolsPixelGroup = c(1,2)
    )
  )
  dt$B <- round(runif(nrow(dt), 1, 100))
  dt$speciesCode[dt$canfi_species == 204] <- "PINU_CON"
  dt$speciesCode[dt$canfi_species == 1201] <- "POPU_TRE"
  data.table::setorder(dt, speciesCode, age, poolsPixelGroup)

  out2 <- cumPoolsCreateAGB(dt, table6 = table6AGB, table7 = table7AGB, pixGroupCol ="poolsPixelGroup")

  expect_equal(rowSums(out2[,c("merch", "foliage", "other")]), dt$B/2)
  expect_true(all(out2[dt$age < 15, "merch"] ==  0))
  expect_equal(nrow(out2), nrow(dt))
  expect_true(all(colnames(out2) == c("speciesCode", "age", "poolsPixelGroup", "merch", "foliage", "other")))

})

