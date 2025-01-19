
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

# Download CBM-CFS3 database
dbURL <- "https://raw.githubusercontent.com/cat-cfs/libcbm_py/main/libcbm/resources/cbm_defaults_db/cbm_defaults_v1.2.8340.362.db"
dbPath <- file.path(testDirs$temp$inputs, basename(dbURL))
if (!file.exists(dbPath)){
  download.file(url = dbURL, destfile = dbPath, mode = "wb", quiet = TRUE)
}

test_that("spuDist", {

  listDist <- spuDist(27, dbPath = dbPath)

  expect_true(inherits(listDist, "data.table"))

  expect_true(all(c(
    "disturbance_type_id", "spatial_unit_id", "disturbance_matrix_id", "name", "description"
  ) %in% names(listDist)))

  expect_true(all(listDist$spatial_unit_id == 27))

})
