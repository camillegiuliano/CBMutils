
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("ReticulateFindPython", {

  pyenvRoot <- file.path(testDirs$temp$outputs, "ReticulateFindPython")
  dir.create(pyenvRoot)

  # Check that Python can be installed
  pyEXE <- ReticulateFindPython(
    version = "3.10", pyenvRoot = pyenvRoot,
    pyenvOnly = TRUE, useGit = FALSE, prompt = FALSE)

  expect_equal(length(pyEXE), 1)
  expect_true(file.exists(pyEXE))
  expect_true(tolower(tools::file_ext(pyEXE)) == "exe")

  # Check that Python was installed at pyeenvRoot
  expect_true("ReticulateFindPython" %in% strsplit(normalizePath(pyEXE, winslash = "/"), "/")[[1]])

  # Check that re-running returns the same path
  pyEXE2 <- ReticulateFindPython(
    version = "3.10", pyenvRoot = pyenvRoot,
    pyenvOnly = TRUE, useGit = FALSE, prompt = FALSE)

  expect_identical(pyEXE, pyEXE2)

})

