#' @title Combine Catch Records
#'
#' @description Catch records taken only a few days apart are added together, and assigned to the first date.
#' The catch records are taken to be in a form created by \code{\link{consolidateByDate}} but without the
#' final \code{TOTAL} row.  Note: the number of traps (\code{nTrap}) will also be summed, and thus will
#' likely need adjusting after calling this function.
#'
#' @aliases combineRecords
#'
#' @usage combineRecords(catchMat, minDiff=5)
#'
#' @param catchMat A numeric matrix, typically created by \code{\link{consolidateByDate}} but without the final
#' \code{TOTAL} row, containing the catch records.  The row.names of the \code{catchMat} should be able to be
#'  parsed into dates.
#'
#' @param minDiff A numeric giving the number of days (defaults to 5) within which records should be combined
#' (i.e., if the number of days is less than or equal to \code{minDiff} then the records are combined).
#'
#' @return catchMat A numeric matrix with the same number of columns are the original input matrix, but
#' (potentially) less rows.
#'
#' @author Peter Alspach (paalspach@gmail.com)
#'
#' @examples
#' combineRecords(catchBoth)

#' @export

combineRecords <- function(catchMat, minDiff=5)
{
  flag <- TRUE
  while (flag)
  {
    flag <- FALSE
    for (i in 1:(nrow(catchMat)-1))
    {
      ttDate <- as.Date(rownames(catchMat))
      if ((ttDate[i+1]-ttDate[i]) <= minDiff)
      {
        catchMat[i,] <- apply(catchMat[c(i, i+1),], 2, sum, na.rm=TRUE)
        catchMat[i+1,] <- 0
        flag <- TRUE
      }
    }
    catchMat <- catchMat[apply(catchMat, 1, sum, na.rm=TRUE)!=0,]
  }
  return(catchMat)
}
