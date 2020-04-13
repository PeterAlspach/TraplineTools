#' @title Function to Set the Cut Dates for the Monthly Intervals

#'
#' @description This function cuts a vector of Julian days into monthly intervals, starting at the
#' day-in-the-month specified by \code{firstDay}.
#'
#' @aliases setCutDates
#'
#' @usage setCutDates(firstDay, yr, jDays)
#'
#' @param firstDay An integer giving the day for the beginning of each monthly interval.  This would be 1 to
#' begin at the start of each month, and 16 to begin (roughly) in the middle of each month.
#'
#' @param yr An integer giving the year.  This is necessary so that leap years can be accommodated
#'
#' @param jDays An integer vector giving the Julian days to be assigned monthly intervals.
#'
#' @return obsMth A factor vector of the same length as \code{jDays} with 12 levels labelled according to
#' the monthly interval into which the corresponding Julain day falls.  Days outside the interval (i.e.,
#' either before the start day in January or after the end of the final day in the December interval) are
#' returned as missing values
#'
#' @author Peter Alspach (paalspach@gmail.com)
#'
#' @examples
#' setCutDates(16, 2019, 1:400)
#'
#' @export

setCutDates <- function(firstDay=16, yr=yr, jDays=tl$jDay)
{
  cutDays <- paste(yr, c(paste0('0', 1:9), 10:12), firstDay, sep='-') # cut at the middle of the month
  cutDays <- julian.Date(as.Date(cutDays), origin=as.Date(paste0(yr-1, '-12-31')))
  mthInt <- paste(firstDay, month.abb, sep='-')
  obsMth <- cut(jDays, c(cutDays, 365+firstDay+floor((4.001-yr%%4)/4)), labels=mthInt, right=FALSE)
  return(obsMth=obsMth)
}

