#' @title Reads Trapline Data from csv generated from trap.nz
#'
#' @description By default \code{getTrapNZData} expects the entire year-to-date data, but it will accept
#' just the most recent month.  In the latter case, it is optional to append this to the year-to-date csv
#' file and thus update it. A column containing the Julian day is added.
#'
#' @aliases getTrapNZData
#'
#' @usage getTrapNZData('manage_trap_records.csv', recentMth='TrapCatchesRecent', append=FALSE,
#'  trapType='DOC 200', dstOffset=FALSE, ...)
#'
#' @param file The primary file with the trap catch data (default \code{'manage_trap_records.csv'}).
#' This is a csv file as downloaded from the trap.nz database, or created by this function if
#' \code{append=TRUE} (see later).
#'
#' @param recentMth The most recent month's data.  It may take a long time to download the entire year-to-date data
#' from the website (perhaps risking a process time-out).  Thus it is often desirable to just get
#' the most recent month's data and then append this to the year-to-date data as downloaded the previous month. Defaults
#' to \code{NULL}; i.e., the entire year-to-date data is assumed to be in the primary file.
#'
#' @param append A logical.  If \code{FALSE} (the default) then the recent month's data is read and used in the current
#' code session, but the data is not appended to the csv holding the primary data;
#' if \code{TRUE} then the csv holding the primary data is updated with the most recent month's data.
#'
#' @param trapType A character vector giving the names of the trap types to include when processing the data.
#' Defaults to \code{'DOC 200'}.
#'
#' @param dstOffset A logical.  If \code{FALSE} (the default) no special action is taken; if \code{TRUE} then all dates
#' with 23:00:00 as the time are changed to 24:00:00, thereby increment the date by one day.  This was necessary with the
#' old DOC database but probably not trap.nz
#'
#' @param ... Other arguments to pass to \code{read.csv}.
#'
#' @return A list is return: the first element (\code{tl}) is a dataframe with the processed data;
#' the second (\code{yr}) is an atomic numeric giving the year.
#'
#' @author Peter Alspach (paalspach@gmail.com)
#'
#' @examples
#' getTrapNZData('manage_trap_records.csv', recentMth='trap_records_recent', append=FALSE)
#'
#' @export

getTrapNZData <- function(file='manage_trap_records.csv', recentMth=NULL, append=FALSE, trapType='DOC 200',
                          dstOffset=FALSE, ...)
{
  tl <- read.csv(file, header=TRUE, sep=',', stringsAsFactors=FALSE, ...)

  if (!is.null(recentMth)) # get recent month's data is not null
  {
    tlXtra <- read.csv(recentMth, header=TRUE, sep=',', stringsAsFactors=FALSE, ...)
    tl <- rbind(tl, tlXtra)
    tl <- tl[!duplicated(apply(tl, 1, paste, collapse=':')),] # remove duplicated rows, if they exist
  }

  tl <- tl[tl$trap.type %in% trapType,] # keep only data for the relevant traps

  # Append recent month's data to current year's data if requested
  if (append) write.csv(tl, 'manage_trap_records.csv', row.names=FALSE)

  # Rename traps with only a single digit 'd' to '0d'
  relNo <- tl[grep('[[:upper:]][[:digit:]]{1}$', tl$code), 'code']
  relNo <- paste0(substring(relNo, 1, nchar(relNo)-1), '0', substring(relNo, nchar(relNo)))
  tl[grep('[[:upper:]][[:digit:]]{1}$', tl$code), 'code'] <- relNo
  relNo <- tl[grep('[[:upper:]][[:digit:]]{1} trap2', tl$code), 'code']
  relNo <- paste0(substring(relNo, 1, nchar(relNo)-7), '0', substring(relNo, nchar(relNo)-6))
  tl[grep('[[:upper:]][[:digit:]]{1} trap2', tl$code), 'code'] <- relNo

  # Handle anomalous dates when daylight saving is in place.
  if (dstOffset) tl$date <- sub('23:00:00', '24:00:00', tl$date)

  # Remove time from date stamp
  tl$date <- substring(tl$date, 1, 10)
  yr <- as.integer(substring(tl[substring(tl$date, 6)>='01-16', 'date'][1], 1, 4))
  tl$jDay <- julian.Date(as.Date(tl$date), origin=as.Date(paste0(yr-1, '-12-31')))
  tlOld <- tl[tl$jDay <= 15,]
  tl <- tl[tl$jDay > 15,]
  return(list(tl=tl, yr=yr))
}
