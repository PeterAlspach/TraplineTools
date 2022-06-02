#' @title Consolidates Trap Data by Month
#'
#' @description Consolidates the trap catch data for each line for a particular month, or the entire year
#' (i.e., over the entire trap data.frame as imported by \code{\link{getTrapNZData}}).
#'
#' @aliases consolidateTrapNZData
#'
#' @usage consolidateTrapNzData(trapData=tl, mthInt=obsMth, currMth=NULL, mammals=allMammals,
#'                            stat=stdStat, nBoxes=nBox)
#'
#' @param trapData A data.frame containing the trap data (typically as captured by \code{\link{getTrapNZData}}).
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
#' @param stat A character vector containing the columns to retain in the summary.  All the rest will be
#' lumped into 'Other'.
#'
#' @param nBoxes A number vector containing the number of trap boxess per line (irrespective of whether they are
#' missing or non-functional).
#'
#' @param lineNames A character vector containing the names of the lines.  If \code{NULL}, the default, then
#' the \code{unique(trapData$line)} is used.
#'
#' @return catch A data.frame with the consolidated data for each line both as absolute values and relative
#' to the number of traps per line.
#'
#' @author Peter Alspach (paalspach@gmail.com)
#'
#' @examples
#' consolidateTrapNzData()
#'
#' @export

consolidateTrapNZData <- function(trapData=tl, mthInt=obsMth, currMth=NULL, mammals=allMammals,
                                  stat=stdStat, nBoxes=nBox, lineNames=NULL)
{
  if (is.null(lineNames)) lineNames <- unique(trapData$line)

  # Subset trapData to the current month if currMth is non-NULL
  if (!is.null(currMth))
  {
    trapData <- trapData[mthInt==levels(mthInt)[currMth],]
    mthInt <- mthInt[mthInt==levels(mthInt)[currMth]]

    # Recompute nBox (i.e., number of boxes with functioning traps) for the month
    cM1 <- trapData[!(trapData$status=='Trap gone' | regexpr('miss', trapData$notes, ignore.case=TRUE)>0),]
    nBoxes <- with(cM1[-grep(' trap2', cM1$code),], table(line, gsub('([[:upper:]]+)([[:digit:]]+)', '\\2', code)))
    nBoxes <- apply(nBoxes, 1, function(x) length(x[x!=0]))
  }

  # Tabluate the catches (species.caught) by line, lump some species, and get total
  tt <- trapData[,c('line','species.caught','status')]
  tt[tt$species.caught=='None' & tt$status=='Sprung', 'species.caught'] <- 'Sprung'
  catch <- table(tt$line, tt$species.caught)

  catch <- catch[, match(unique(c(stat, mammals)), colnames(catch))]
  catch <- cbind(catch[, 1:length(stat)],
                 Other=apply(catch[, -(1:length(stat))], 1, sum, na.rm=TRUE))
  catch <- cbind(catch, Total=apply(catch[, -1], 1, sum, na.rm=TRUE))

  # Add in the number of traps per line (irrespective of whether some are missing)
  catch <- merge(nBoxes, catch, by='row.names', all=TRUE)
  colnames(catch)[2:3] <- c('nTrap','Sprung')

  rownames(catch) <- catch$Row.names
  catch <- catch[,-1]
  # Replace missing values with 0 (necessary as missing values are put if the pest wasn't trapped in the period)
  catch[is.na(catch)] <- 0

  # Ensure all lines are represented (necessary in some months lines are missed)
  catch <- catch[lineNames,]
  rownames(catch) <- lineNames

  # Add total line
  catch <- rbind(catch, apply(catch, 2, sum, na.rm=TRUE))
  rownames(catch)[nrow(catch)] <- 'TOTAL'

  # Add numbers caught per trap per month (x 100)
  nMth <- length(unique(mthInt))
  catch <- cbind(catch, round(apply(catch[,-1], 2, function(x) 100*x/(catch[,1]*nMth)), 1))

  catch[nrow(catch), ncol(catch)] <- round(100*catch[nrow(catch), 'Total']/
                                             (sum(catch[-nrow(catch), 'nTrap'][!is.na(catch[,'Total'])], na.rm=TRUE)*nMth), 1)

  # Put lines in alphabetical order
  catch <- catch[order(rownames(catch)),]

  return(catch)
}
