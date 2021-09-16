#' Smooth the cumPools data.table
#'
#' This uses the Chapman Richards equation to smooth the curves that are
#' in \code{colsToUse}.
#' @param cumPoolsRaw Data.table with a numeric column called \code{age} and
#'   columns called \code{colsToUse}.
#' @param colsToUse A character vector of columns to smooth.
#' @param colsToUseNew A character vector of column names for the new smoothed columns
#' @export
#' @author Celine Boisvenue and Eliot McIntire
#' @return
#' A data.table with original columns plus new columns named \code{colsToUseNew}.
cumPoolsSmooth <- function(cumPoolsRaw, colsToUse = c("totMerch", "fol", "other"),
                           colsToUseNew = paste0(colsToUse, "_New"))  {
  message(crayon::red("The translation of m3/ha using the Boudewyn et al. stand level translation, ",
          "often results in some curves having peaks and/or swiggles. ",
          "We have built-in an automatic smoothing algorithm that uses a Chapman-Richards ",
          "function form to smooth the resulting curves. There are four attempts at ",
          "smoothing with a Chapman Richards in this algorithm. At the end of the fourth attempt, ",
          " the original curve is used. It is the users' responsibility to inspect curves. ",
          "This process is highly likely to require user intervention."))
  cpr <- cumPoolsRaw # no copy -- just convenience
  cpr[, (colsToUse) := lapply(.SD, as.numeric), .SDcols = colsToUse]

  outInd <- character()

  outerInd <- 0
  ## debugging tools
  #cpr <- cpr[gcids %in% unique(cpr$gcids)[(198)]]
  #message("REMOVE PREVIOUS LINE TO GET BACK ALL GCIDS")
  lenUniqueID_ecozone <- length(unique(cpr[["gcids"]]))
  cpr[, (colsToUseNew) := {
    outerInd <<- outerInd + 1
    outInd <<- .BY
    print(paste0(outerInd, " of ", lenUniqueID_ecozone, ": ", outInd))

    N <- .N
    ind <- seq(N)
    wts <- rep(1L, N)

    SD <- copy(.SD)

    newVals <- lapply(colsToUse, function(c2u) {

      # Find blip, first minimum after that blip, and real maximum
      # blipInd -- calculate 2nd derivative (using diff(diff())), pad with 2 zeros (b/c diff removes 1 value each time)
      blipInd <- which.max(abs(c(0, 0, diff(diff(get(c2u))))))
      # firstMin is the lowest value to the right of blipInd -- if it is next index, fine, or if well right, also fine
      firstMin <- which.min(get(c2u)[seq(blipInd+1, N - 40)]) + blipInd
      # realMax is the first maximum after the first minimum after the blipInd
      realMax <- which.max(get(c2u)[seq(firstMin+1, N)]) + firstMin
      firstInflection <- tail(which(0.12 * get(c2u)[realMax] > get(c2u)), 1)
      quantile20pct <- get(c2u)[realMax] * 0.2
      indQuantile20pct <- which(get(c2u) <= quantile20pct)

      ## set weights -- right points near realmax are 100,
      #     and left points up to 20% of the value at readMax get 100
      newWtsLowerVal <- max(10, round((realMax - firstMin) * 0.3))
      wts[unique(pmin(N, realMax + -newWtsLowerVal:10))] <- 100

      # wts[unique(pmin(N, realMax + -10:10))] <- 100
      wts[ind > (realMax + 40) ] <- 0
      wts[indQuantile20pct] <- 100


      Astart <- get(c2u)[realMax]

      for (chopitoff in 1:4) {
        # Gets rid of wiggles and huge peak
        #if (outInd > 17) browser()
        if (length(firstInflection) > 0) {
          SD[, override := ind > firstInflection & ind < firstMin]
          SD[override == TRUE, (c2u) := NA]
          SD[is.na(get(c2u)),
             (c2u) := list(
               approx(SD$age, SD[[c2u]], xout = SD$age[is.na(SD[[c2u]])])$y#,
               #approx(SD$age, SD$fol, xout = SD$age[is.na(SD$fol)])$y,
               #approx(SD$age, SD$other, xout = SD$age[is.na(SD$other)])$y
             )
          ]
        }
        ## debugging tools
        # Visualize wts with following
        # plot(.SD$age, .SD[[c2u]], pch = 19)
        # points(SD$age, SD[[c2u]], col = wts + 1, pch = 19)
        # message("CHANGED THIS TO ONLY 200 not 2000")
        for(ii in 1:700) {
          # Chapman Richards
          nlsout <- try(nlrob(as.formula(paste(c2u, "~ A * (1 - exp(-k * age))^p")),
                              data = SD, #maxit = 200,
                              weights = wts,
                              maxit = 200,
                              start = list(A = Astart,
                                           k = runif(1, 0.0001, 0.13),
                                           p = runif(1, 1, 80)),
                              trace = FALSE), silent = TRUE)
          if (!is(nlsout, "try-error"))
            break
        }

        if (!is(nlsout, "try-error")) {
          break
        } else {

          if ( chopitoff == 1 ) {
            firstInflectionOrig <- firstInflection
            firstMinOrig <- firstMin

            tooBig <- which(.SD[[c2u]] > (Astart) )
            wtsOrig <- wts
            if (length(tooBig) > 0) {
              SD[tooBig, (c2u) := NA]
              SD <- na.omit(SD)
              wts <- wts[-tooBig]
              ind <- ind[-tooBig]
            } else {
              newWtsLowerVal <- round((realMax - firstMin) * 0.6)
              wts[unique(pmin(N, realMax + -newWtsLowerVal:10))] <- 100

            }
          } else if (chopitoff == 2) {

            if (length(firstInflection) > 0) {
              # smooth out the sharpness
              firstInflection <- max(10, firstInflection - 10)
              firstMin <- min(N, firstMin + 10)
            }
            # wts <- rep(1L, N)

          } else if (chopitoff == 3) {
            firstInflection <- firstInflectionOrig
            firstMin <- firstMinOrig

            wts <- wtsOrig
            SD <- copy(.SD)
            ind <- seq(N)
            SD[get(c2u) > 0, (c2u) := pmax(0, jitter(get(c2u), factor = 150))]

          }
        }

      }
      #browser()
      ind <- seq(N)
      if (is(nlsout, "try-error")) {
      #   stop("This gcid ", .SD$gcids, " failed to converge while estimating Chapman Richards smoothing")
        warning(c2u, " of gcid ", as.character(gcids), " (item ",outerInd,") failed to converge while estimating Chapman Richards smoothing; ",
                "Using original curve")
        #if (outInd > 17) browser()
        newVals <- .SD[[c2u]]
      } else {
        fittedNew <- predict(nlsout, newdata = .SD)
        # Because Chapman Richards is monotonic increasing, it can't have decreasing at right tail
        #  This next check looks for cases where the data are below the fitted Chapman Richards
        #   at the right tail (after the last point that the data cross below the Chapman Richards line)
        #  Then it replaces values with original data values, instead of Chapman Richards,
        #  allowing declines at the right tail
        diffr <- c(diff(ifelse(get(c2u) > fittedNew, 1, -1)), 0)
        lastCrossing <- tail(which(diffr != 0), 1)
        newVals <- ifelse(ind >= lastCrossing, get(c2u), fittedNew)
        if (FALSE) {
          devNum <- match(c2u, colsToUse) + 3
          dev(devNum)
          plot(.SD$age, .SD[[c2u]], type = "p", pch = 19, col = wts + 1,
               ylab = "", xlab = "", axes = FALSE,
               main = gcids,
               cex = 0.8)
          axis(1); axis(2)
          lines(SD$age, newVals)
          mtext(side = 1, line = -1, text = c2u, outer = TRUE)
        }
        newVals

      }
      ## debuggin tools
      # plot(SD$age, SD[[c2u]], type = "p")
      # with(list(A = coef(nlsout)[["A"]], k = coef(nlsout)[["k"]], p = coef(nlsout)[["p"]]), lines(age, A * (1 - exp(-k * age))^p))
      #
    })

    newVals
  }, by = "gcids"]
}
