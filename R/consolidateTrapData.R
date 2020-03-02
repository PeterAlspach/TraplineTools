#' @title Consolidates Trap Data by Month
#'
#' @description
#'
#' @aliases consolidateTrapData
#'
#' @usage consolidateTrapData(trapData=tl, mthInt=obsMth, currMth=NULL, mammals=allMammals,
#'                            status=stdStat, nTraps=nTrap)
#'
#' @param trapData A data.frame containing the trap data (typically as captured by \code{\link{getTrapData}}.
#'
#' @param mthInt A factor vector giving the monthly interval names.  This should have the same number of
#' elements as there are rows in \code{trapData}.
#'
#' @param currMth Either \code{NULL} or an atomic integer.  If \code{NULL} (the default) the entire
#' \code{trapData} dataframe is summarised; otherwise the \code{trapData} is first restricted to only the
#' monthly interval specified by this argument.
#'
#' @param mammals A character vector containing the names of all the mammals of interest.
#'
#' @param status A character vector containing the columns to retain in the summary.  All the rest will be
#' lumped into 'Other'.
#'
#' @param nTraps A number vector containing the number of traps per line (irrespective of whether they are
#' missing or non-functional).
#'
#' @return catch A data.frame with the consolidated data for each line both as absolute values and relative
#' to the number of traps per line.
#'
#' @author Peter Alspach (paalspach@gmail.com)
#'
#' @examples
#' consolidateTrapData()
#'
#' @details Blah.
#'
#' @export

consolidateTrapData <- function(trapData=tl, mthInt=obsMth, currMth=NULL, mammals=allMammals,
                                status=stdStat, nTraps=nTrap)
{
  # Subset trapData to the current month if currMth is non-NULL
  if (!is.null(currMth))
  {
    if (currMth==1) currMth <- 13
    trapData <- trapData[mthInt==levels(mthInt)[currMth-1],]
    mthInt <- mthInt[mthInt==levels(mthInt)[currMth-1]]

    # Replace nTraps with the number of functioning traps for the month
    cM1 <- trapData[!(trapData$Status %in% c('Missing', 'Maintenance required not functioning')),]
    nTraps <- table(cM1$Line, gsub('([[:upper:]]+)([[:digit:]]+)', '\\2', cM1$Tag.No))
    nTraps <- apply(nTraps, 1, function(x) length(x[x!=0]))
  }

  # Tabluate the catches (Trap.1 and Trap.2) by Line
  trap1 <- table(trapData$Line, trapData$Trap.1)
  trap2 <- table(trapData$Line, trapData$Trap.2)
  trap2 <- trap2[, colnames(trap2)!='N/A']
  tt <- unique(c(colnames(trap1), colnames(trap2)))

  # Combine the two traps and subset lump some animals together
  trap1 <- cbind(trap1, matrix(0, nrow(trap1), sum(!(tt %in% colnames(trap1))),
                               dimnames=list(dimnames(trap1)[[1]], tt[!(tt %in% colnames(trap1))])))
  trap2 <- cbind(trap2, matrix(0, nrow(trap2), sum(!(tt %in% colnames(trap2))),
                               dimnames=list(dimnames(trap2)[[1]], tt[!(tt %in% colnames(trap2))])))
  catch <- trap1[, tt] + trap2[, tt]
  catch <- catch[, match(unique(c(status, mammals)), colnames(catch))]
  catch <- cbind(catch[, 1:length(status)],
                 Other=apply(catch[, -(1:length(status))], 1, sum, na.rm=TRUE))
  catch <- cbind(catch, Total=apply(catch[, -1], 1, sum, na.rm=TRUE))

  # Add in the number of traps per line (irrespective of whether some are missing)
  catch <- merge(nTraps, catch, by='row.names', all=TRUE)
  colnames(catch)[2:3] <- c('nTrap','Sprung')

  rownames(catch) <- catch$Row.names
  catch <- catch[,-1]
  catch <- rbind(catch, apply(catch, 2, sum, na.rm=TRUE))
  rownames(catch)[nrow(catch)] <- 'TOTAL'

  # Add numbers caught per trap per month (x 100)
  # catch <- cbind(catch, round(apply(catch[,-1], 2, function(x) 100*x/catch[,1]), 1))
  nMth <- length(unique(mthInt))
  catch <- cbind(catch, round(apply(catch[,-1], 2, function(x) 100*x/(catch[,1]*nMth)), 1))

  catch[nrow(catch), ncol(catch)] <- round(100*catch[nrow(catch), 'Total']/
                            (sum(catch[-nrow(catch), 'nTrap'][!is.na(catch[,'Total'])], na.rm=TRUE)*nMth), 1)

  return(catch)
}