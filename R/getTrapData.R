#' @title Reads Trapline Data from csv
#'
#' @description By default \code{getTrapData} expects the entire year's data, but it will accept
#' just the most recent month.  In the latter case, it is optional to append this to the year's csv
#' file and thus update it. The time stamp is removed from the date, and a column containing the Julian day is
#' added.
#'
#' @aliases getTrapData
#'
#' @usage getTrapData('TrapCatches.csv', recentMth='TrapCatchesRecent', append=FALSE,
#'  omitLines='Possum A12', dstOffset=FALSE, ...)
#'
#' @param file The primary file with the trap catch data (default \code{'TrapCatches.csv'}).
#' This is a csv file as downloaded from the DOC database, or created by this function if
#' \code{append=TRUE} (see later).
#'
#' @param recentMth The most recent month's data.  It can take a long time to download the entire year's data
#' from the website (so long that the process sometimes times out).  Thus it is often desirable to just get
#' the most recent month's data and then append this to the year's data as downloaded the previous month. Defaults
#' to \code{NULL}; i.e., the entire year's data if assumed to be in the primary file.
#'
#' @param append A logical.  If \code{FALSE} (the default) then the recent month's data is read and used in the current
#' code session, but the data is not appended to the csv holding the primary data;
#' if \code{TRUE} then the csv holding the primary data is updated with the most recent month's data.
#'
#' @param omitLines A character vector giving the names of the lines to exclude when processing the data.
#' Defaults to \code{'Possum A12'}.
#'
#' @param dstOffset A logical.  If \code{FALSE} (the default) no special action is taken; if \code{TRUE} then all dates
#' with 23:00:00 as the time are changed to 24:00:00, thereby increment the date by one day.
#'
#' @param ... Other arguments to pass to \code{read.csv}.
#'
#' @return A list is return: the first element (\code{tl}) is a dataframe with the processed data;
#' the second (\code{yr}) is an atomic numeric giving the year.
#'
#' @author Peter Alspach (paalspach@gmail.com)
#'
#' @examples
#' getTrapData('TrapCatches.csv', recentMth='TrapCatchesRecent', append=FALSE)
#'
#' @export

getTrapData <- function(file='TrapCatches.csv', recentMth=NULL, append=FALSE, omitLines='Possum A12',
                        dstOffset=FALSE, ...)
{
  tl <- read.csv(file, header=TRUE, sep=',', stringsAsFactors=FALSE, ...)

  if (!is.null(recentMth)) # get recent month's data is not null
  {
    tlXtra <- read.csv(recentMth, header=TRUE, sep=',', stringsAsFactors=FALSE, ...)
    tl <- rbind(tl, tlXtra)
  }

  tl <- tl[!(tl$Line %in% omitLines),] # omit irrelevant lines

  # Append recent month's data to current year's data if requested
  if (append) write.csv(tl, 'TrapCatches.csv', row.names=FALSE)

  # Rename traps with only a single digit 'd' to '0d'
  relNo <- tl[grep('[[:upper:]][[:digit:]]{1}$', tl$Tag.No), 'Tag.No']
  relNo <- paste0(substring(relNo, 1, nchar(relNo)-1), '0', substring(relNo, nchar(relNo)))
  tl[grep('[[:upper:]][[:digit:]]{1}$', tl$Tag.No), 'Tag.No'] <- relNo

  # Handle anomalous dates when daylight saving is in place.
  if (dstOffset) tl$Date.Checked <- sub('23:00:00', '24:00:00', tl$Date.Checked)

  # Remove time from date stamp
  tl$Date.Checked <- as.character(as.POSIXct(tl$Date.Checked, format="%Y-%m-%dT%H:%M"))
  yr <- as.integer(substring(tl[substring(tl$Date.Checked, 6)>='01-16', 'Date.Checked'][1], 1, 4))
  tl$jDay <- julian.Date(as.Date(tl$Date.Checked), origin=as.Date(paste0(yr-1, '-12-31')))
  tlOld <- tl[tl$jDay <= 15,]
  tl <- tl[tl$jDay > 15,]
  return(list(tl=tl, yr=yr))
}
