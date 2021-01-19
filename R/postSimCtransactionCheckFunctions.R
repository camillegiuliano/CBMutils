utils::globalVariables(c("colC", "disturbance", "noLoss", "type", "value"))

#' Verify and check transactions
#'
#' Post-simulations, this function takes the last two years of simulations,
#' and verifies that the C transfers are as expected.
#' It randomly selects three `pixelGroups`, calculated the expected transactions from time
#' `end(sim)` and time `end(sim) - 1`, and compares them to the last year of simulations.
#' Cases of disturbed, non disturbed, sw, hw, young and old stands were tested.
#'
#' @section Structure:
#' 1. Read-in data
#' 2. randomly select 3 pixelGroups
#' 3. read-in carbon pools at time(sim) and time(sim) - 1 for the three pixelGroup
#' 4. pull-out matrices applied to these three pixelGroups between t0 and t1
#' 5. matrices into a data table
#' 6. prep pools at t0
#' 7. merge three pools at t0 with their respective transaction matrices ids
#' 8. apply all matrices (8.1:8.9)
#'
#' TODO: I am passing a whole sim list here for simplification, but only four
#' objects are needed from the simList: $cbmPools, $pixelKeep, $opMatrixCBM,
#' $allProcesses, and $pooldef.
#'
#' TODO: there are still a bunch of checks that need to be changed to assertions (if statements?)
#'
#' @param sim A simulation (`simList`) object.
#'
#' @export
#' @importFrom data.table rbindlist setorderv
#' @importFrom stats runif time
#' @importFrom stringr str_replace_all
checkTransactions <- function(sim) {
  ### 1. Sim list and packages ################################
  # pick the two last years of the simulation
  poolsIn <- sim$cbmPools[simYear == time(sim) - 1, ]
  poolsOut <- sim$cbmPools[simYear == time(sim), ]

  ### 2. Randomly pick 3 pixelGroup for checking ###############################
  # this line for three randomly selected stands
  checkPg <- round(runif(3, min = min(poolsOut$pixelGroup), max = max(poolsOut$pixelGroup)), digits = 0)
  # need to check one that was disturbed, add this
  # checkPg[3] <- max(poolsIn$pixelGroup)+20
  # checking hardwood pixelGroup processing
  # hwGc <- sim$gcMeta[forest_type_id == 3, growth_curve_component_id]
  # hwPg <- sim$pixelGroupC[growth_curve_component_id %in% hwGc,pixelGroup]
  # checkPg <-  round(runif(3, min = min(hwPg), max = max(hwPg)),digits = 0)
  checkPg <- sort(checkPg)

  ### 3. Get correct carbon for pre-carbon transfers for the three randomly
  # the carbon for the three randomly selected pixelGroups after transactions###################
  outCcheck <- poolsOut[pixelGroup %in% checkPg]
  # the carbon for the three randomly selected pixelGroups before transactions
  # if not disturbed
  inCcheck <- poolsIn[pixelGroup %in% checkPg[which(checkPg %in% poolsIn$pixelGroup)], ]
  # if disturbed
  # disturbed pixelGroups need carbon from previous year in old pixelGroup
  # need to match the carbon for the disturbed pools with IN ORDER
  # figure out the matching pixelGroup for disturbed pixels to get the right initial carbon values
  # are there any disturbed pixelGroups?
  if (dim(inCcheck)[1] != 3) {
    pKeep <- unique(sim$pixelKeep[, (dim(sim$pixelKeep)[2] - 1):(dim(sim$pixelKeep)[2])])
    names(pKeep) <- c("inC", "outC")
    distCcheck <- poolsIn[pixelGroup %in%
      pKeep$inC[which(pKeep$outC %in%
        checkPg[which(!checkPg %in% poolsIn$pixelGroup)])], ]
    # match the order of the pixelGroups matches
    pKeep <- pKeep[match(checkPg[which(!checkPg %in% poolsIn$pixelGroup)], pKeep$outC), ]
    setnames(pKeep, "inC", "pixelGroup")
    distPg <- pKeep[distCcheck, on = "pixelGroup"]
    # cols <- c("pixelGroup", "outC")
    distPg[, pixelGroup := NULL]
    setnames(distPg, "outC", "pixelGroup")
    distCcheck$pixelGroup <- checkPg[which(!checkPg %in% poolsIn$pixelGroup)]
    # put all the before-transaction carbon together
    inCcheck <- rbind(inCcheck, distPg)
  }

  # assertion
  if (!identical(dim(inCcheck), dim(outCcheck))) {
    stop("There is a problem with the pixelGroup section for verification of the carbon transfers.")
  }

  ### 4. Pull out all the matrices applied for each randomly selected pixelGroup #########

  # $opMatrixCBM matrix of identification numbers for the last carbon operations
  # that happened in the sim. This is the same length as the number of pixelGroups
  # in the last year of sim. Each line is a vector of matrix ids used on the each
  # pixelGroup. Number of lines equals number of pixel groups in pools.
  lastOpMatrices <- sim$opMatrixCBM
  theseOpMatrices <- lastOpMatrices[which(poolsOut$pixelGroup %in% checkPg), ]
  # $allProcesses read in as processes, is list of length nine: one for each type
  # of carbon transaction. This is a list, with some being a pointer an
  # environment (1,3,4,7:9) where matrices are stored, while others are a list of
  # matrices (2,5,6). The matrix identifications in lastOpMatrices (one column per
  # each of the nine lists in processes), specify the matrix identification id to
  # be used in the processes for each of the transactions. For example, if
  # lastOpMatrices$disturbance says '91' for a pixelGroup (order of rows in
  # lastOpMatrices == order of pixelGroups), the transaction matrix for the
  # disturbance for this pixelGroup is processes$Disturbance$'91'. Each
  # transaction matrix has three columns (row, col, value), where value is the
  # proportion of carbon from "row" to "col". The rows in lastOpMatrices are the
  # same order as the pixelGroup in poolsOut. For the lists in processes that are a
  # list of matrices (2,5,6), the matrices are in the same order as the pixelGroup.
  # This means that pixelGroup 890, for example, in row 889 in the poolsOut data
  # table will have carbon transactions described by the 889th matrix in the
  # processes$Growth1 and processes$Growth2.
  processes <- sim$allProcesses

  ### 5. Matrices into a data.table #############
  transactionsDT <- list()
  ## TODO: this could be simplified
  ##### Eliot Attempt
  # if (FALSE) {
  #   indices <- unique(theseOpMatrices[["disturbance"]])
  #   indices <- indices[indices > 0]
  #   makeDT2 <- mget(as.character(indices), processes[["Disturbance"]])
  #   makeDT2 <- do.call(rbind, makeDT2)
  #   set(makeDT2, NULL, "type", "disturbance")
  # }
  #####

  cNames <- colnames(theseOpMatrices)
  theseOpMatrices <- as.data.table(theseOpMatrices)
  theseOpMatrices <- cbind(checkPg, theseOpMatrices)
  # take out the transaction matrices that are not references to by an id
  rmNames <- c("Growth1", "OvermatureDecline", "Growth2")
  colnames(theseOpMatrices) <- str_replace_all(colnames(theseOpMatrices), c(" " = ""))
  colnames(theseOpMatrices)[grep("overmature", colnames(theseOpMatrices))] <- "overmaturedecline"
  for (i in setdiff(names(processes), rmNames)) {
    makeDT <- matrixDT(matricesIn = processes[[i]], indicesIn = names(processes[[i]]))
    colNum <- grep(i, colnames(theseOpMatrices), ignore.case = TRUE)
    theNames <- unique(theseOpMatrices[[colNum]])
    makeDT <- makeDT[name %in% theNames, ]
    makeDT$type <- rep(colnames(theseOpMatrices)[colNum], dim(makeDT)[1])
    transactionsDT[[i]] <- makeDT
  }
  # deal with the matrices who's indicies == pixelGroups
  for (i in setdiff(names(processes), setdiff(names(processes), rmNames))) {
    makeDT <- matrixDT(matricesIn = processes[[i]], indicesIn = 1:(length(poolsOut$pixelGroup)))
    indices <- theseOpMatrices[[tolower(i)]]
    makeDT <- makeDT[name %in% indices, ]
    makeDT$type <- rep(grep(i, colnames(theseOpMatrices), ignore.case = TRUE, value = TRUE), dim(makeDT)[1])
    transactionsDT[[i]] <- makeDT
  }
  # names(processes[[i]]) <- as.character(poolsOut$pixelGroup)
  transDT <- rbindlist(transactionsDT)

  ### 6. Prep pools at time 0 with only the randomly selected pixelGroups ############
  # get the long form of the c pools at the start with "rows" column matching the
  # pool names
  InputColNum <- grep("Input", colnames(inCcheck))
  ProductsColNum <- grep("Products", colnames(inCcheck))
  pgIN <- melt(inCcheck, measure.vars = InputColNum:ProductsColNum, id.vars = c("pixelGroup"))
  # change the Inputs:Products to 1:26 using this
  poolsToRows <- as.data.frame(cbind(pools = sim$pooldef, row = c(1:26))) ## sim$
  names(poolsToRows) <- c("variable", "row")
  pgIN1 <- pgIN[poolsToRows, on = "variable"]
  pgIN1$row <- as.numeric(pgIN1$row)
  names(pgIN1) <- c("pixelGroup", "variable", "carbon", "row")
  keycol <- c("pixelGroup", "row")
  setorderv(pgIN1, keycol)

  ### 7. Link initial carbon with transaction matrices. Remember that the
  ### pixelGroups in the inCcheck (carbon coming in) is already matched with the
  ### final pixelGroup number (in the carbon coming out outCcheck) #################
  cNames <- colnames(theseOpMatrices)
  theseOpMatrices <- as.data.table(theseOpMatrices)
  pgIN1$pixelGroup <- as.numeric(pgIN1$pixelGroup)
  setnames(theseOpMatrices, "checkPg", "pixelGroup")
  cTransaction <- pgIN1[theseOpMatrices, on = "pixelGroup", ]
  names(cTransaction)[2] <- "pool"
  cStartLong <- melt(cTransaction,
    measure.vars = 5:13,
    id.vars = c("pixelGroup", "pool", "carbon", "row")
  )
  names(cStartLong)[5:6] <- c("type", "name")
  cStartLong$name <- as.character(cStartLong$name)

  ### 8. Apply matrices ############################################
  # all the matrices are here transDT
  # the correct carbon coming in with the row, type of disturbance, and name are
  # in here cStartLong
  # carbon coming in merged with the all matrices
  inCols <- c("row", "name", "type")
  inCmats <- merge(cStartLong, transDT, by = inCols)
  # the transfers have to happen in order, and they are either proportional
  # transfers or additive transfers
  # setting up some useful vectors
  byType <- unique(inCmats$type)
  propCols <- c("pixelGroup", "row")
  keepCols <- c("row", "name", "type", "pixelGroup", "pool", "carbon", "col", "value")
  sumCols <- c("pixelGroup", "col")
  # eventually can makes this shorter by making internal functions and running through the matrices:
  # thisOrder <- c("disturbance", "growth1", "domturnover", "bioturnover",
  #                "overmaturedecline", "growth2", "domDecay", "slowdecay", "slowmixing")
  # byType <- byType[order(match(byType,thisOrder))]

  ## NOTE: until all issues with CO2, emissions, and products are solved, keep each
  ## applied matrix separate
  ## the structure is the same: first calculate the new carbon for proportional or
  ## create the carbon to add for additive matrices, second, add the carbon for
  ## the additive matrices, and modify the $carbon column

  # 8.1########################### disturbance first ########
  # create the new carbon vector
  if (sum(theseOpMatrices$disturbance) > 0) {
    inCmats[type == byType[grep("disturbance", byType)],
      newC := (carbon * value),
      by = propCols
    ]

    inCmats[type == byType[grep("disturbance", byType)], colC := sum(newC), by = sumCols]
    newC <- unique(inCmats[type == byType[grep("disturbance", byType)], .(pixelGroup, col, colC)])

    # get rid of extra columns
    outCols <- setdiff(names(inCmats), keepCols)
    inCmats[, (outCols) := NULL]

    # note: not all proportional disturbances affect/keep all 26 pools. For the
    # check below to work and the following carbon update, all 26 pools need to be in
    # the newC data.
    if (dim(newC[, unique(col), by = "pixelGroup"])[1] !=
      length(sim$pooldef) * length(unique(newC$pixelGroup))) {
      # by pixelGroup because it could happen that you have 3 disturbed pixelGroups
      # with different disturbance
      newc <- newC[, unique(col), by = "pixelGroup"]
      for (i in 1:length(unique(newc$pixelGroup))) {
        newc <- newc[pixelGroup == unique(newc$pixelGroup)[i], ]
        # these are the rows missing
        rowsNeeded <- which(!(c(1:length(sim$pooldef)) %in% newc$V1))
        add0rows <- as.data.table(cbind(
          pixelGroup = rep(pixelGroup[i], length(rowsNeeded)), col = rowsNeeded,
          colC = rep(0, length(rowsNeeded))
        ))
        newC <- rbind(newC, add0rows)
      }
    }

    # assertion: there should be the same number of rows as there are pools with proportional matrices
    if (dim(newC)[1] != (length(sim$pooldef) * length(unique(newC$pixelGroup)))) {
      stop("Something failed in the checking processes: there should be one row per carbon pool and there is not.")
    }
    # replace the carbon pools with the resulting carbon
    setnames(newC, "col", "row")
    distCmats <- merge(inCmats, newC, by = c("pixelGroup", "row"), all.x = TRUE)

    distCmats$carbon[which(!is.na(distCmats$colC))] <- distCmats$colC[which(!is.na(distCmats$colC))]
    outCols <- setdiff(names(distCmats), keepCols)
    distCmats[, (outCols) := NULL]
    # check
    checkMats <- unique(distCmats[, c(1, 2, 6)])
    newC[order(pixelGroup, row), ] == checkMats[pixelGroup %in%
                                                  theseOpMatrices[disturbance != 0, pixelGroup] &
                                                  order(pixelGroup, row), ]
  } else {
    distCmats <- inCmats
  }
  ########################## disturbances done

  # 8.2###################### half the growth second #################
  # growth only comes from "input" (row == 1)
  addC <- distCmats[type == byType[grep("growth1", byType)] &
    row == 1, ][, .(pixelGroup, col, value)][col != 1, ]
  # it gets directly added into the pools
  setnames(addC, c("col", "value"), c("row", "addC"))
  g1Cmats <- merge(distCmats, addC, by = propCols, all.x = TRUE)
  g1Cmats[, newC := (carbon + addC)]
  g1Cmats$carbon[which(!is.na(g1Cmats$newC))] <- g1Cmats$newC[which(!is.na(g1Cmats$newC))]
  outCols <- setdiff(names(g1Cmats), keepCols)
  g1Cmats[, (outCols) := NULL]
  # # check
  # checkMats <- unique(g1Cmats[,c(1,2,6)])
  # beforeMats <- unique(distCmats[,c(1,2,6)])
  # diffC <- beforeMats - checkMats
  ###################### half the growth DONE

  # 8.3################### domturnover ####################
  g1Cmats[type == byType[grep("domturnover", byType)],
    newC := (carbon * value),
    by = propCols
  ]
  g1Cmats[type == byType[grep("domturnover", byType)], colC := sum(newC), by = sumCols]
  newC <- unique(g1Cmats[type == byType[grep("domturnover", byType)], .(pixelGroup, col, colC)])
  outCols <- setdiff(names(g1Cmats), keepCols)
  g1Cmats[, (outCols) := NULL]

  # assertion: there should be the same number of rows as there are pools with proportional matrices
  if (dim(newC)[1] != (length(sim$pooldef) * length(unique(newC$pixelGroup)))) {
    stop("Something failed in the checking processes: there should be one row per carbon pool and there is not.")
  }
  # replace carbon column
  # replace the carbon pools with the resulting carbon
  setnames(newC, "col", "row")
  domtCmats <- merge(g1Cmats, newC, by = c("pixelGroup", "row"), all.x = TRUE)
  domtCmats$carbon[which(!is.na(domtCmats$colC))] <- domtCmats$colC[which(!is.na(domtCmats$colC))]
  outCols <- setdiff(names(domtCmats), keepCols)
  domtCmats[, (outCols) := NULL]

  # check
  checkMats <- unique(domtCmats[, c(1, 2, 6)])
  newC$colC == checkMats$carbon
  #################### domturnover done

  # 8.4 ################################# bioturnover #######################
  domtCmats[type == byType[grep("bioturnover", byType)],
    newC := (carbon * value),
    by = propCols
  ]
  domtCmats[type == byType[grep("bioturnover", byType)], colC := sum(newC), by = sumCols]
  newC <- unique(domtCmats[type == byType[grep("bioturnover", byType)], .(pixelGroup, col, colC)])
  outCols <- setdiff(names(domtCmats), keepCols)
  domtCmats[, (outCols) := NULL]

  # assertion: there should be the same number of rows as there are pools with proportional matrices
  if (dim(newC)[1] != (length(sim$pooldef) * length(unique(newC$pixelGroup)))) {
    stop("Something failed in the checking processes: there should be one row per carbon pool and there is not.")
  }
  # replace carbon column
  # replace the carbon pools with the resulting carbon
  setnames(newC, "col", "row")
  biotCmats <- merge(domtCmats, newC, by = c("pixelGroup", "row"), all.x = TRUE)
  biotCmats$carbon[which(!is.na(biotCmats$colC))] <- biotCmats$colC[which(!is.na(biotCmats$colC))]
  outCols <- setdiff(names(biotCmats), keepCols)
  biotCmats[, (outCols) := NULL]
  # check
  checkMats <- unique(biotCmats[, c(1, 2, 6)])
  which(newC[order(pixelGroup, row), ] != checkMats[order(pixelGroup, row), ])
  ################################# bioturnover DONE

  # 8.5 ############################ overmaturedecline ###################
  ## TODO CAREFUL I **THINK** THESE ARE SUBSTRACTION MATRICES ALTHOUGH THEY ARE NOT
  ## FUNCTIONAL RIGHT NOW (SEE ISSUE #7 in github)
  ## I will run through the matrices here as if they are ok so we can correctly
  ## test the carbon transactions assuming it is a substraction, WITHOUT changing
  ## the "from" and "to" pools so we can check the c-pools differences between t0
  ## and t1
  # overmatDT[value != 1,]
  #     row col      value name
  # 1:   1  14 0.002914955  841
  # 2:   1  15 0.002914955  841
  # 3:   1  12 0.084506420  841
  # 4:   1  13 0.084506420  841
  minusC <- biotCmats[type == byType[grep("overmaturedecline", byType)], ]
  # check is any values are different then 1, if not there is no overmature decline
  if (mean(minusC$value) != 1) {
    minusC <- minusC[, .(pixelGroup, col, value)][value != 1, ]
    ## for checking only skip these 4 lines, they replace the one above
    # minusC <- overmatDT[value != 1,]
    # minusC[,row := NULL]
    # setnames(minusC,c("col","value","name"),c("row","minusC","pixelGroup"))
    # minusC$pixelGroup <- 893

    # skip
    setnames(minusC, c("col", "value"), c("row", "minusC"))

    overmatCmats <- merge(biotCmats, minusC, by = propCols, all.x = TRUE)
    overmatCmats[, newC := (carbon + minusC)]
    overmatCmats$carbon[which(!is.na(overmatCmats$newC))] <- overmatCmats$newC[which(!is.na(overmatCmats$newC))]
    outCols <- setdiff(names(overmatCmats), keepCols)
    overmatCmats[, (outCols) := NULL]
  } else {
    overmatCmats <- biotCmats
  }
  ############################ overmaturedecline DONE

  # 8.6 ########################################growth2, 2nd half of the growth ##########
  # growth only comes from "input" (row == 1)
  addC <- overmatCmats[type == byType[grep("growth2", byType)] & row == 1, ][, .(pixelGroup, col, value)][col != 1, ]
  # it gets directly added into the pools
  setnames(addC, c("col", "value"), c("row", "addC"))
  g2Cmats <- merge(overmatCmats, addC, by = propCols, all.x = TRUE)
  g2Cmats[, newC := (carbon + addC)]
  g2Cmats$carbon[which(!is.na(g2Cmats$newC))] <- g2Cmats$newC[which(!is.na(g2Cmats$newC))]
  outCols <- setdiff(names(g2Cmats), keepCols)
  g2Cmats[, (outCols) := NULL]
  # # check
  # checkMats <- unique(g2Cmats[,c(1,2,6)])
  # beforeMats <- unique(overmatCmats[,c(1,2,6)])
  # diffC <- beforeMats - checkMats
  ###################### half the growth DONE

  # 8.7 ################################# domdecay #######################
  g2Cmats[type == byType[grep("domDecay", byType)],
    newC := (carbon * value),
    by = propCols
  ]
  g2Cmats[type == byType[grep("domDecay", byType)], colC := sum(newC), by = sumCols]
  newC <- unique(g2Cmats[type == byType[grep("domDecay", byType)], .(pixelGroup, col, colC)])
  outCols <- setdiff(names(g2Cmats), keepCols)
  g2Cmats[, (outCols) := NULL]

  # assertion: there should be the same number of rows as there are pools with proportional matrices
  if (dim(newC)[1] != (length(sim$pooldef) * length(unique(newC$pixelGroup)))) {
    stop("Something failed in the checking processes: there should be one row per carbon pool and there is not.")
  }
  # replace carbon column
  # replace the carbon pools with the resulting carbon
  setnames(newC, "col", "row")
  domdCmats <- merge(g2Cmats, newC, by = c("pixelGroup", "row"), all.x = TRUE)
  domdCmats$carbon[which(!is.na(domdCmats$colC))] <- domdCmats$colC[which(!is.na(domdCmats$colC))]
  outCols <- setdiff(names(domdCmats), keepCols)
  domdCmats[, (outCols) := NULL]
  # check
  checkMats <- unique(domdCmats[, c(1, 2, 6)])
  which(newC[order(pixelGroup, row), ] != checkMats[order(pixelGroup, row), ])
  ################################# domdecay DONE

  # 8.8 ####################### slowdecay #######################
  domdCmats[type == byType[grep("slowdecay", byType)],
    newC := (carbon * value),
    by = propCols
  ]
  domdCmats[type == byType[grep("slowdecay", byType)], colC := sum(newC), by = sumCols]
  newC <- unique(domdCmats[type == byType[grep("slowdecay", byType)], .(pixelGroup, col, colC)])
  outCols <- setdiff(names(domdCmats), keepCols)
  domdCmats[, (outCols) := NULL]

  # assertion: there should be the same number of rows as there are pools with proportional matrices
  if (dim(newC)[1] != (length(sim$pooldef) * length(unique(newC$pixelGroup)))) {
    stop("Something failed in the checking processes: there should be one row per carbon pool and there is not.")
  }
  # replace carbon column
  # replace the carbon pools with the resulting carbon
  setnames(newC, "col", "row")
  slowdCmats <- merge(domdCmats, newC, by = c("pixelGroup", "row"), all.x = TRUE)
  slowdCmats$carbon[which(!is.na(slowdCmats$colC))] <- slowdCmats$colC[which(!is.na(slowdCmats$colC))]
  outCols <- setdiff(names(slowdCmats), keepCols)
  slowdCmats[, (outCols) := NULL]
  # check
  checkMats <- unique(slowdCmats[, c(1, 2, 6)])
  which(newC[order(pixelGroup, row), ] != checkMats[order(pixelGroup, row), ])
  ############################ slowdecay DONE

  # 8.9 ############# slowmixing #######################
  slowdCmats[type == byType[grep("slowmixing", byType)],
    newC := (carbon * value),
    by = propCols
  ]
  slowdCmats[type == byType[grep("slowmixing", byType)], colC := sum(newC), by = sumCols]
  newC <- unique(slowdCmats[type == byType[grep("slowmixing", byType)], .(pixelGroup, col, colC)])
  outCols <- setdiff(names(slowdCmats), keepCols)
  slowdCmats[, (outCols) := NULL]

  # assertion: there should be the same number of rows as there are pools with proportional matrices
  if (dim(newC)[1] != (length(sim$pooldef) * length(unique(newC$pixelGroup)))) {
    stop("Something failed in the checking processes: there should be one row per carbon pool and there is not.")
  }
  # replace carbon column
  # replace the carbon pools with the resulting carbon
  setnames(newC, "col", "row")
  slowmixCmats <- merge(slowdCmats, newC, by = c("pixelGroup", "row"), all.x = TRUE)
  slowmixCmats$carbon[which(!is.na(slowmixCmats$colC))] <- slowmixCmats$colC[which(!is.na(slowmixCmats$colC))]
  outCols <- setdiff(names(slowmixCmats), keepCols)
  slowmixCmats[, (outCols) := NULL]
  # check
  checkMats <- unique(slowmixCmats[, c(1, 2, 6)])
  which(newC[order(pixelGroup, row), ] != checkMats[order(pixelGroup, row), ])
  ################################# slowmixing DONE

  # 9. change the format of outCcheck to match pgIN1 ####################################
  # get the long form of the c pools at the start with "rows" column matching the
  # pool names
  pgOUT <- melt(outCcheck,
    measure.vars = InputColNum:ProductsColNum,
    id.vars = c("pixelGroup")
  )
  ## add the names to help follow
  pgOUT1 <- pgOUT[poolsToRows, on = "variable"]
  pgOUT1$row <- as.numeric(pgOUT1$row)
  keepCols <- c("pixelGroup", "pool", "carbon", "row")
  names(pgOUT1) <- keepCols
  pgOUT1 <- pgOUT1[order(pixelGroup, row), c(1, 4, 2, 3)]
  setnames(pgOUT1, "carbon", "simC")


  # 10.0 Compare carbon at t1 with calculated carbon from transactions
  outCols <- setdiff(names(slowmixCmats), keepCols)
  endCcalc <- copy(slowmixCmats)
  calcCout <- endCcalc[, (outCols) := NULL]
  calcCout <- calcCout[order(pixelGroup, row), ]
  calcCout <- unique(calcCout)
  setnames(calcCout, "carbon", "calcC")
  # compare
  cols <- c("pixelGroup", "row", "pool")
  compareC <- merge(calcCout, pgOUT1, by = cols)
  compareC$match <- round(compareC$simC, 5) == round(compareC$calcC, 5)

  return(compareC[match == "FALSE", ])
}
