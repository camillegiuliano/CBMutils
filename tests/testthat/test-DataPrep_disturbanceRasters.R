
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

# Set URL to disturbance rasters ZIP archive
## Raster test data is a subset of the Wulder and White disturbance rasters covering SK 1984-2011
disturbanceRastersURL <- file.path(
  "https://raw.githubusercontent.com/PredictiveEcology/CBMutils/refs/heads/development",
  "tests/testthat/testdata/SaskDist_1985-1857_crop.zip")

# Set output path
destinationPath <- file.path(testDirs$temp$outputs, "dataPrep_disturbanceRasters")
dir.create(destinationPath)

test_that("dataPrep_disturbanceRastersURL", {

  # Test: archive file with 1 raster file per year
  disturbanceRasters <- dataPrep_disturbanceRastersURL(
    destinationPath       = destinationPath,
    disturbanceRastersURL = disturbanceRastersURL,
  )

  expect_true(is.list(disturbanceRasters))
  expect_true(all(sapply(disturbanceRasters, class) == "SpatRaster"))
  expect_identical(names(disturbanceRasters), as.character(1985:1987))

  # Test: single file with 1 raster band per year
  disturbanceRasters <- dataPrep_disturbanceRastersURL(
    destinationPath       = destinationPath,
    disturbanceRastersURL = disturbanceRastersURL,
    targetFile            = "disturbance_testArea/SaskDist_1985_crop.tif",
    bandYears             = 1985
  )

  expect_s4_class(disturbanceRasters, "SpatRaster")
  expect_identical(names(disturbanceRasters), as.character(1985))
  expect_match(basename(terra::sources(disturbanceRasters)), as.character(1985))

})

test_that("dataPrep_disturbanceRasters", {

  # Read validation data
  validEvents <- lapply(
    list(
      resampleSkip = file.path(destinationPath, "SaskDist_1985-1857_crop/SaskDist_1985-1987_crop_events.csv"),
      resample10m  = file.path(destinationPath, "SaskDist_1985-1857_crop/SaskDist_1985_resample10m_events.csv"),
      resample100m = file.path(destinationPath, "SaskDist_1985-1857_crop/SaskDist_1985_resample100m_events.csv")
    ),
    function(csv) data.table::fread(csv, key = c("year", "eventID")) |> subset(eventID != 0)
  )

  # Read input rasters
  disturbanceRastersList <- list(
    files = lapply(
      list(
        "1985" = file.path(destinationPath, "SaskDist_1985-1857_crop/SaskDist_1985_crop.tif"),
        "1986" = file.path(destinationPath, "SaskDist_1985-1857_crop/SaskDist_1986_crop.tif"),
        "1987" = file.path(destinationPath, "SaskDist_1985-1857_crop/SaskDist_1987_crop.tif")
      ),
      terra::rast
    ))
  disturbanceRastersList[["bands"]] <- {
    inRast <- disturbanceRastersList[["files"]]
    rMerge <- c(inRast[[1]], inRast[[2]], inRast[[3]])
    names(rMerge) <- names(inRast)
    rMerge
  }

  # Expect error: list items not names by disturbance year
  expect_error(dataPrep_disturbanceRasters(unname(disturbanceRastersList[["files"]])))
  expect_error(dataPrep_disturbanceRasters(unname(disturbanceRastersList[["bands"]])))

  # Test: single SpatRaster with 1 raster band per year and pixel values as event IDs
  distEvents <- dataPrep_disturbanceRasters(disturbanceRastersList[["bands"]])

  expect_true(inherits(distEvents, "data.table"))
  for (colName in c("pixelIndex", "year", "eventID")){
    expect_true(colName %in% names(distEvents))
    expect_true(is.integer(distEvents[[colName]]))
    expect_true(all(!is.na(distEvents[[colName]])))
  }

  ## Compare with validation table
  expect_equal(
    data.table::setkey(
      distEvents[, .(count = .N), by = c("year", "eventID")],
      year, eventID),
    validEvents[["resampleSkip"]]
  )

  # Test: Summarize only a subset of years
  expect_identical(
    dataPrep_disturbanceRasters(disturbanceRastersList[["bands"]], year = 1985),
    subset(distEvents, year == 1985)
  )
  expect_identical(
    dataPrep_disturbanceRasters(disturbanceRastersList[["bands"]], year = c(1985, 1987)),
    subset(distEvents, year %in% c(1985, 1987))
  )

  # Test: set eventID
  distEventsWithID <- dataPrep_disturbanceRasters(disturbanceRastersList[["bands"]], eventID = 8)
  expect_equal(distEventsWithID, distEvents[, eventID := 8L])


  # Test: list of SpatRaster with one raster per year and pixel values as event IDs
  distEvents <- dataPrep_disturbanceRasters(disturbanceRastersList[["files"]])

  expect_true(inherits(distEvents, "data.table"))
  for (colName in c("pixelIndex", "year", "eventID")){
    expect_true(colName %in% names(distEvents))
    expect_true(is.integer(distEvents[[colName]]))
    expect_true(all(!is.na(distEvents[[colName]])))
  }

  ## Compare with validation table
  expect_equal(
    data.table::setkey(
      distEvents[, .(count = .N), by = c("year", "eventID")],
      year, eventID),
    validEvents[["resampleSkip"]]
  )

  # Test: Summarize only a subset of years
  expect_identical(
    dataPrep_disturbanceRasters(disturbanceRastersList[["files"]], year = 1985),
    subset(distEvents, year == 1985)
  )
  expect_identical(
    dataPrep_disturbanceRasters(disturbanceRastersList[["files"]], year = c(1985, 1987)),
    subset(distEvents, year %in% c(1985, 1987))
  )

  # Test: set eventID
  distEventsWithID <- dataPrep_disturbanceRasters(disturbanceRastersList[["bands"]], eventID = 8)
  expect_equal(distEventsWithID, distEvents[, eventID := 8L])


  # Test: with raster template: upsample
  distEvents10m <- dataPrep_disturbanceRasters(
    disturbanceRasters = disturbanceRastersList[["files"]]["1985"],
    templateRast       = terra::rast(
      res  = 10, vals = NA,
      xmin = -677500, xmax = -677500 + 10000,
      ymin =  704500, ymax =  704500 + 10000,
      crs  = "EPSG:3979"
    )
  )

  expect_equal(
    data.table::setkey(
      distEvents10m[, .(count = .N), by = c("year", "eventID")],
      year, eventID),
    validEvents[["resample10m"]]
  )

  # Test: with raster template: downsample
  distEvents100m <- dataPrep_disturbanceRasters(
    disturbanceRasters = disturbanceRastersList[["files"]]["1985"],
    templateRast       = terra::rast(
      res  = 100, vals = NA,
      xmin = -677500, xmax = -677500 + 10000,
      ymin =  704500, ymax =  704500 + 10000,
      crs  = "EPSG:3979"
    )
  )

  expect_equal(
    data.table::setkey(
      distEvents100m[, .(count = .N), by = c("year", "eventID")],
      year, eventID),
    validEvents[["resample100m"]]
  )
})





