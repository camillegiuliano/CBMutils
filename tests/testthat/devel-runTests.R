
# Run all tests
testthat::test_local()

# Run a subset of tests
testthat::test_local(filter = "Boudewyn")
testthat::test_local(filter = "CBM-DB")
testthat::test_local(filter = "CBM-plots")
testthat::test_local(filter = "CBM-tools")
testthat::test_local(filter = "DataPrep")
testthat::test_local(filter = "Python")

# Run individual tests
testthat::test_local(filter = "Boudewyn_cumPoolsCreate")
testthat::test_local(filter = "Boudewyn_cumPoolsCreateAGB")
testthat::test_local(filter = "Boudewyn_cumPoolsSmooth")
testthat::test_local(filter = "Boudewyn_gcidsCreate")
testthat::test_local(filter = "Boudewyn_growthCurves")
testthat::test_local(filter = "Boudewyn_m3ToBiomPlots")
testthat::test_local(filter = "CBM-DB_disturbances")
testthat::test_local(filter = "CBM-plots")
testthat::test_local(filter = "CBM-tools_calcC")
testthat::test_local(filter = "DataPrep_disturbanceRasters")
testthat::test_local(filter = "Python_ReticulateFindPython")

