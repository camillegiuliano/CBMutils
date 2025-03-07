
# Run all tests
testthat::test_local()

# Run subsets of tests
testthat::test_local(filter = "DB-ListDisturbances")

