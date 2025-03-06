
table6AGB <- reproducible::prepInputs(url = "https://drive.google.com/file/d/1gvtV-LKBNbqD7hmlL4X0i40P3du75oJc",
                                   fun = "data.table::fread",
                                   destinationPath = "inputs",
                                   filename2 = "appendix2_table6_tb.csv")
table7AGB <- reproducible::prepInputs(url = "https://drive.google.com/file/d/16nQgTGW2p_IYF_Oavcc7WbWbgWl5uywt",
                                   fun = "data.table::fread",
                                   destinationPath = "inputs",
                                   filename2 = "appendix2_table7_tb.csv")

test_that("testing biomProp function", {
  params6 <- data.frame()
  params6 <- data.frame()
  vol <- data.frame()

  # with volume as input
  prop <- biomProp(params6, params7, vol)
  prop1 <- biomProp(params6, params7, vol, type = "volume")
  expect_equal(prop, prop1)
  expect_true(all(rowSum(prop) == 1))
  expect_equal(prop[1,], c()) # expected result by hand
  expect_error(biomProp(params6, params7, vol, type = "biomass"))
  expect_error(biomProp(params6, params7, vol, type = "notacorrectype"))

  # with AGB as input
  params6 <- table6AGB[juris_id == 'BC', ecozone == 9, canfi_spec == 105,] # white spruce
  params7 <- table7AGB[juris_id == 'BC', ecozone == 9, canfi_spec == 105,]
  B <- round(runif(10, 0, 10))

  expect_error(biomProp(params6, params7, B))
  expect_error(biomProp(params6, params7, B, type = "volume"))
  prop <- biomProp(params6, params7, B, type = "biomass")
  expect_true(all(rowSum(prop) == 1))
  expect_equal(prop[1,], c()) # expected result by hand

})
