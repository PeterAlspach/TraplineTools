#' @title Function to Facilitate Reporting Miscellaneous Species Numbers
#'
#' @description Given a table with the numbers of various species, this function outputs a character variable
#' to express the number of a particular species in standard language.
#'
#' @aliases mSppFn
#'
#' @usage mSppFn(sppTab, spp, sppPlural, zero=TRUE)
#'
#' @param sppTab A table giving the numbers of various species, the names of which are contained in the
#' dimnames attribute.
#'
#' @param spp An atomic character, which must correspond exactly (i.e., it is case sensitive) to one
#' of the species in \code{sppTab}.
#'
#' @param sppPlural An atomic character giving the English language plural of the species given by \code{spp}.
#'
#' @param zero A logical.  If \code{TRUE} (the default) then, should there be no occurrences of the species,
#' a character string of the form '0 cats' is return; if \code{FALSE} then \code{NULL} is returned in such
#' an event.
#'
#' @return retStr A character string containing an English language phrase expressing the number of the
#' particular species (as specified by \code{spp}) found in the \code{sppTab} table.
#'
#' @author Peter Alspach (paalspach@gmail.com)
#'
#' @examples
#' mSppFn(miscSppY, 'Cat', 'cats')
#'
#' @export

mSppFn <- function(sppTab, spp, sppPlural, zero=TRUE)
{
  if (spp %in% names(sppTab))
  {
    if (sppTab[names(sppTab)==spp]==1) retStr <- paste(1, tolower(spp)) else
      retStr <- paste(sppTab[names(sppTab)==spp], sppPlural)
  } else
  {
    if (zero) retStr <- paste(0, sppPlural) else NULL
  }
  return(retStr)
}
