#' @title Consolidate Trap Data by Date
#'
#' @description Consolidates the trap catch data (as imported by \code{\link{getTrapNZData}}) for each date,
#'  optionally grouped by line and/or trap type.
#'
#' @aliases consolidateByDate
#'
#' @usage consolidateByDate(trapData=tl, groupBy=c('line', 'trap.type'), stat=stdStat)
#'
#' @param trapData A data.frame containing the trap data (typically as captured by \code{\link{getTrapNZData}}).
#'
#' @param groupBy A character vector of the columns by which to group the data when consolidating
#'  (defaults to \code{c('line', 'trap.type')}).
#'
#' @param stat A character vector containing the columns to retain in the summary.  All the rest will be
#' lumped into 'Other'.
#'
#' @return catch A matrix (or list of matrices if \code{groupBy} is non-null) with the consolidated data,
#'  optionally grouped by line and/or trap type.
#'
#' @author Peter Alspach (paalspach@gmail.com)
#'
#' @examples
#' consolidateByDate()
#'
#' @export

consolidateByDate <- function(trapData=tl, groupBy=c('line', 'trap.type'), stat=stdStat)
{
  # Where appropriate, put replace 'None' by 'Sprung' in species.caught
  trapData[trapData$species.caught=='None' & trapData$status=='Sprung', 'species.caught'] <- 'Sprung'

  # Tabulate the catches (species.caught)

  # First the process if there is no grouping (i.e., groupBy=NULL)
  if(is.null(groupBy))
  {
   catch <- table(trapData$date, trapData$species.caught)
   catch <- cbind(catch[, colnames(catch) %in% c('None', stat)],
                  Other=apply(as.matrix(catch[,!(colnames(catch) %in% c('None', stat))]), 1, sum))
   # Add in the number of traps per line
   catch[, 'None'] <- apply(catch, 1, sum)
   colnames(catch)[colnames(catch)=='None'] <- 'nTrap'
   # Add total line
   catch <- rbind(catch, apply(catch, 2, sum))
   rownames(catch)[nrow(catch)] <- 'Total'
   # Re-order columns
   catch <- catch[, c('nTrap', stat[stat %in% colnames(catch)], 'Other')]
  } else
  {
    if(length(groupBy)>1) grp <- apply(trapData[, groupBy], 1, paste, collapse='; ') else
                          grp <- trapData[, groupBy] # create grouping variable grp

    # Make list ready to hold the catch matrices
    ttNames <- unique(grp)
    catch <- as.list(rep(NA, length(ttNames)))
    names(catch) <- ttNames[order(ttNames)]

    # Fill in the list with the appropriate matrices
    for(i in ttNames)
    {
      tt <- trapData[grp==i, c('date','species.caught')]
      ttCatch <- table(tt$date, tt$species.caught)
      ttCatch <- cbind(ttCatch[, colnames(ttCatch) %in% c('None', stat)],
                     Other=apply(as.matrix(ttCatch[,!(colnames(ttCatch) %in% c('None', stat))]), 1, sum))
      # Add in the number of traps per line
      ttCatch[, 'None'] <- apply(ttCatch, 1, sum)
      colnames(ttCatch)[colnames(ttCatch)=='None'] <- 'nTrap'
      # Add total line
      ttCatch <- rbind(ttCatch, apply(ttCatch, 2, sum))
      rownames(ttCatch)[nrow(ttCatch)] <- 'Total'
      # Re-order columns
      ttCatch <- ttCatch[, c('nTrap', stat[stat %in% colnames(ttCatch)], 'Other')]
      catch[[i]] <- ttCatch
    }
  }

  return(catch)
}

