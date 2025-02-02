
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
    "spatial_unit_id", "disturbance_type_id", "disturbance_matrix_id", "name", "description"
  ) %in% names(listDist)))

  expect_true(all(listDist$spatial_unit_id == 27))

})

test_that("spuDistMatch", {

  # Set list of disturbance types
  distTypes <- rbind(
    data.frame(rasterID = 1, wholeStand = 1, distName = "Wildfire"),
    data.frame(rasterID = 2, wholeStand = 1, distName = "Clearcut harvesting without salvage"),
    data.frame(rasterID = 3, wholeStand = 0, distName = "Generic 20% mortality"),
    data.frame(rasterID = 4, wholeStand = 1, distName = "Deforestation")
  )

  # Try with a single spuID
  spuIDs <- 28
  distTable <- cbind(spatial_unit_id = spuIDs, distTypes)

  listDist <- spuDistMatch(distTable, dbPath = dbPath, ask = FALSE)

  expect_true(inherits(listDist, "data.table"))

  expect_true(all(c(
    "disturbance_type_id", "spatial_unit_id", "disturbance_matrix_id", "name", "description"
  ) %in% names(listDist)))

  expect_equal(listDist$disturbance_matrix_id, c(371, 160, 91, 26))

  # Try with 2 spuIDs
  spuIDs <- c(27, 28)
  distTable <- do.call(rbind, lapply(spuIDs, function(spuID){
    cbind(spatial_unit_id = spuID, distTypes)
  }))

  listDist <- spuDistMatch(distTable, dbPath = dbPath, ask = FALSE)

  expect_true(inherits(listDist, "data.table"))

  expect_true(all(c(
    "disturbance_type_id", "spatial_unit_id", "disturbance_matrix_id", "name", "description"
  ) %in% names(listDist)))

  expect_equal(listDist$disturbance_matrix_id, c(
    378, 160, 91, 26,
    371, 160, 91, 26
  ))

  # Expect error: name does not have an exact match and ask = FALSE
  expect_error(
    spuDistMatch(data.frame(spatial_unit_id = 27, name = "clearcut"), dbPath = dbPath, ask = FALSE)
  )
})

test_that("histDist", {

  listDist <- histDist(27, dbPath = dbPath)

  expect_true(inherits(listDist, "data.table"))

  expect_true(all(c(
    "spatial_unit_id", "disturbance_type_id", "disturbance_matrix_id", "name", "description"
  ) %in% names(listDist)))

  expect_true(all(listDist$spatial_unit_id == 27))

  expect_true(nrow(listDist) == 1)

  # Result should be for name "Wildfire"
  expect_true(listDist$disturbance_matrix_id == 378)

})

