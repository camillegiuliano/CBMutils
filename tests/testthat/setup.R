
if (!testthat::is_testing()){
  library(testthat)
  devtools::load_all()
}

# Set up test directories
testDirs <- .testDirectorySetUp()
