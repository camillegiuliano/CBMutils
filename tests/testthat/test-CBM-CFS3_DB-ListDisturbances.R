
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("spuDist", {

  # Download CBM-CFS3 database
  dbURL <- "https://raw.githubusercontent.com/cat-cfs/libcbm_py/main/libcbm/resources/cbm_defaults_db/cbm_defaults_v1.2.8340.362.db"
  dbPath <- file.path(testDirs$temp$inputs, basename(dbURL))
  if (!file.exists(dbPath)){
    download.file(url = dbURL, destfile = dbPath, mode = "wb", quiet = TRUE)
  }

  listDist <- spuDist(27, dbPath = dbPath)

  expect_true(inherits(listDist, "data.table"))

  expect_true(all(c(
    "spatial_unit_id", "disturbance_type_id", "disturbance_matrix_id", "name", "description"
  ) %in% names(listDist)))

  expect_true(all(listDist$spatial_unit_id == 27))

})
