# functions used in assertions for checking proportional matrices
### Function to make a data.table out of the carbon transfer matrices

## these need to be used in an assertion after matrices are loaded
matrixDT <- function(matricesIn,  indicesIn){
  matListDT <- lapply(matricesIn,as.data.table)
  matDT <- rbindlist(matListDT)
  matDTnrow <- lapply(matricesIn,nrow)
  matNrow <- do.call("rbind",matDTnrow)
  nameVec <- NULL
  for(i in 1:length(indicesIn)){
    thisIndex <- rep(indicesIn[i], times = matNrow[i])
    nameVec <- c(nameVec,thisIndex)
  }
  matDT$name  <-  nameVec
  return(matDT)
}

### Function to check that the proportions add up to 1.
checkProp <- function(DTofMatrices, tarCols){
  cols <- c("row","name")
  checkProp <- DTofMatrices[,noLoss := sum(value), by = cols][,.(name, row, noLoss)]
  return(checkProp)
}
