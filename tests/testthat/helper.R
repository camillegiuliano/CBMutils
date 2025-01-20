
# Get a list of test directory paths
.testDirectorySetUp <- function(
    tempDir     = tempdir(),
    teardownEnv = if (testthat::is_testing()) testthat::teardown_env()){

  # Set testing package name
  testPackage <- if (testthat::is_testing()){
    testthat::testing_package()
  }else basename(getwd())

  # Get a list of test directory paths
  testDirs <- list()

  # Set input data path
  testDirs$testdata <- testthat::test_path("testdata")

  # Set temporary directory paths
  testDirs$temp <- list(
    root = file.path(tempDir, paste0("testthat-", testPackage))
  )
  testDirs$temp$inputs   <- file.path(testDirs$temp$root, "inputs")
  testDirs$temp$outputs  <- file.path(testDirs$temp$root, "outputs")

  # Create temporary directories
  for (d in testDirs$temp) dir.create(d)
  if (!is.null(teardownEnv)) withr::defer({
    unlink(testDirs$temp$root, recursive = TRUE)
    if (file.exists(testDirs$temp$root)) warning(
      "Temporary test directory could not be removed: ", testDirs$temp$root, call. = FALSE)
  }, envir = teardownEnv, priority = "last")

  # Return test directory paths
  testDirs
}

