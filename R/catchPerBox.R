#' @title Catch for Each Trap Box by Month
#'
#' @description Consolidates the trap catch data for each line for a particular month, or the entire year
#' (i.e., over the entire trap data.frame as imported by \code{\link{getTrapData}}).
#'
#' @aliases catchPerBox
#'
#' @usage catchPerBox(trapData=tl, mthInt=obsMth, relCols=c('Tag.No','Trap.1','Trap.2'), mammals=allMammals)
#'
#' @param trapData A data.frame containing the trap data (typically as captured by \code{\link{getTrapData}}).
#'
#' @param mthInt A factor vector giving the monthly interval names.  This should have the same number of
#' elements as there are rows in \code{trapData}.
#'
#' @param relCols A character vector giving the names of the relevant columns; i.e., those containing the unique
#' trap box number, and the catch for each trap.  Defaults to \code{c('Tag.No','Trap.1','Trap.2')}.
#'
#' @param mammals A character vector containing the names of all the mammals of interest.
#'
#' @return catch A matrix with the total mammalian catch for each trap for each month.
#'
#' @author Peter Alspach (paalspach@gmail.com)
#'
#' @examples
#' catchPerBox()
#'
#' @export

catchPerBox <- function(trapData=tl, mthInt=obsMth, relCols=c('Tag.No','Trap.1','Trap.2'), mammals=allMammals)
{
  # First tabulate the number of pests caught
  trap1 <- table(mthInt, trapData[, relCols[1]], trapData[, relCols[2]])
  trap2 <- table(mthInt, trapData[, relCols[1]], trapData[, relCols[3]])
  trap2 <- trap2[, , dimnames(trap2)[[3]]!='N/A']
  tt <- unique(c(dimnames(trap1)[[3]], dimnames(trap2)[[3]]))

  # Append to trap1 if necessary
  appd <- array(0, replace(dim(trap1), 3, length(tt)-dim(trap1)[3]),
                dimnames=list(dimnames(trap1)[[1]], dimnames(trap1)[[2]],
                              tt[!(tt %in% dimnames(trap1)[[3]])]))
  trap1 <- abind::abind(trap1, appd, along=3)

  # Append to trap2
  appd <- array(0, replace(dim(trap2), 3, length(tt)-dim(trap2)[3]),
                dimnames=list(dimnames(trap2)[[1]], dimnames(trap2)[[2]],
                              tt[!(tt %in% dimnames(trap2)[[3]])]))
  trap2 <- abind::abind(trap2, appd, along=3)

  # Only interested in mammalian catch
  mams <- mammals[mammals %in% dimnames(trap1)[[3]]]
  trap1 <- apply(trap1[,, mams], 1:2, sum)
  trap2 <- apply(trap2[,, mams], 1:2, sum)

  catch <- trap1 + trap2
  return(catch)
}
