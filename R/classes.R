#' Dataset class
#'
#' DESCRIPTION NEEDED
#'
#' @export
setClass("dataset", slots = list(
  turnoverRates = "matrix",
  rootParameters = "matrix",
  decayParameters = "matrix",
  spinupParameters = "matrix",
  classifierValues = "matrix",
  climate = "matrix",
  spatialUnitIds = "matrix",
  slowAGtoBGTransferRate = "matrix",
  biomassToCarbonRate = "matrix",
  ecoIndices = "matrix",
  spuIndices = "matrix",
  stumpParameters = "matrix",
  overmatureDeclineParameters = "matrix",
  disturbanceMatrix = "matrix",
  disturbanceMatrixAssociation = "matrix",
  disturbanceMatrixValues = "matrix",
  disturbanceMatrixIndices = "matrix",
  disturbanceEvents = "matrix",
  landclasses = "matrix",
  pools = "matrix",
  domPools = "matrix"
))
