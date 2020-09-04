if (interactive()) library(testthat)

test_that("StepPools works", {
  nPixGrp <- 3 ## aka nstands

  pools <- matrix(c(1.0, 10.0, 0.0, 1.0, 20.0, 0.0, 1.0, 5.0, 0.0),
                  ncol = 3, nrow = nPixGrp, byrow = TRUE)
  colnames(pools) <- c("input", "pool1", "pool2")
  #> pools
  ##      input pool1 pool2
  ## [1,]     1    10     0
  ## [2,]     1    20     0
  ## [3,]     1     5     0

  ## these are the indices into the flow matrices
  #op <- matrix(rep(c(10, 1, 2), nPixGrp), ncol = 3, nrow = nPixGrp, byrow = TRUE)
  op <- matrix(rep(c(10, 1), nPixGrp), ncol = 2, nrow = nPixGrp, byrow = TRUE)
  #colnames(op) <- c("disturbance", "growth", "other")
  colnames(op) <- c("disturbance", "growth")

  stopifnot(nrow(pools) == nrow(op))

  cnames <- c("row", "col", "value")
  dist <- matrix(c(2, 3, 1, 1, 1, 1), ncol = 3, nrow = 2, byrow = TRUE)
  colnames(dist) <- cnames
  grow <- matrix(c(1, 2, 0.1, 1, 3, 0.2, 2, 3, 0.3, 3, 3, 1.0), ncol = 3, nrow = 4, byrow = TRUE)
  colnames(grow) <- cnames
  #other <- matrix(c(1, 1, 1, 2, 2, 1, 3, 3, 1), ncol = 3, nrow = 3, byrow = TRUE)
  #colnames(grow) <- cnames

  distenv <- new.env(parent = emptyenv())
  distenv$`10` <- dist
  #flow <- list(Disturbance = distenv, Growth = list(grow), Other = list(list(), other))
  flow <- list(Disturbance = distenv, Growth = list(grow))

  stopifnot(length(flow) == ncol(op))

  new_pools <- StepPools(pools, op, flow)
  #> new_pools
  ##      input pool1 pool2
  ## [1,]     1   0.1  10.2
  ## [2,]     1   0.1  20.2
  ## [3,]     1   0.1   5.2

  exp_pools <- structure(c(1, 1, 1, 0.1, 0.1, 0.1, 10.2, 20.2, 5.2),
                         .Dim = c(3L, 3L),
                         .Dimnames = list(NULL, c("input", "pool1", "pool2")))

  expect_identical(new_pools, exp_pools)
})
