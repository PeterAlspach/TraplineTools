#' @title Catch for Each Trap Box by Month
#'
#' @description Consolidates the trap catch data for each line for a particular month, or the entire year
#' (i.e., over the entire trap data.frame as imported by \code{\link{getTrapNZData}}).
#'
#' @aliases catchNZPerBox
#'
#' @usage catchPerNZBox(trapData=tl, mthInt=obsMth, mammals=allMammals)
#'
#' @param trapData A data.frame containing the trap data (typically as captured by \code{\link{getTrapNZData}}, but
#' only the first list element).
#'
#' @param mthInt A factor vector giving the monthly interval names.  This should have the same number of
#' elements as there are rows in \code{trapData}.
#'
#' @param mammals A character vector containing the names of all the mammals of interest.
#'
#' @return catch A matrix with the total mammalian catch for each trap for each month.
#'
#' @author Peter Alspach (paalspach@gmail.com)
#'
#' @examples
#' catchNZPerBox()
#'
#' @export

catchNZPerBox <- function(trapData=tl, mthInt=obsMth, mammals=allMammals)
{
  # First remove ' trap2' from the trap numbers for the doubles
  trapData$code <- gsub(' trap2', '', trapData$code)

  # Now tabulate number of relevant mammals caught
  tt <- trapData[, c('code','species.caught')]
  tt$obsMth <- mthInt
  tt$mammalsCaught <- rep(0, nrow(tt))
  tt[tt$species.caught %in% mammals, 'mammalsCaught'] <- 1
  catch <- tapply(tt[, 'mammalsCaught'], tt[, c('obsMth','code')], sum, na.rm=TRUE)
  return(catch)
}
