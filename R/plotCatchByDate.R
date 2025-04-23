#' @title Plot Catches by Date
#'
#' @description Given a matrix of catches (typically as created by \code{\link{consolidateByDate}} after
#' combining records using \code{\link{combineRecords}}), plots the catches per trap for each date
#' for the current and previous years.
#'
#' @aliases plotCatchByDate
#'
#' @usage plotCatchByDate(Catch, stat=stdStat[stdStat!='Sprung'], combine=c('Rodent','Mustelid'),
#'                        total=TRUE, ...)
#'
#' @param Catch A numeric matrix containing the catch data (typically as created by applying
#'  \code{\link{combineRecords}} to the output from \code{\link{consolidateByDate}}).
#'
#' @param stat A character vector containing the columns to plot separately.  All the rest will be
#' lumped into 'Other'.
#'
#' @param combine A character vector specifying whether to combine certain groups of mammals.  Valid options
#' are \code{'Rodent'} which combines rats and mice together, \code{'Mustelid'} which combines stoats,
#' weasels and ferrets together, and \code{c('Rodent','Mustelid')} (the default) which does both.
#'
#' @param total A logical.  If \code{TRUE} then a line giving the total catch is included.
#'
#' @param ... Other paraments to pass to \code{plot}.
#'
#' @return A plot is drawn, but no objects are returned
#'
#' @author Peter Alspach (paalspach@gmail.com)
#'
#' @examples
#' plotCatchByDate(catch)
#'
#' @export

plotCatchByDate <- function(Catch, stat=stdStat[stdStat!='Sprung'], combine=c('Rodent','Mustelid'),
                            total=TRUE, ...)
{
  if ((('Rodent' %in% combine) & !('Rat' %in% stat)) | (('Mustelid' %in% combine) & !('Stoat' %in% stat)))
  {
    stop('combine has category for species not included in stat
         (i.e., Rodent without Rat or Mustelid without Stoat)')
  }
  currentYr <- as.numeric(substring(date(), 21, 24))
  tt <- apply(Catch[,-1], 2, function(x) x <- 100*x/Catch[,1]) # modify to catch per trap
  tt <- as.matrix(tt[, colnames(tt) %in% stat]) # subset columns to only those in stat
  if(is.null(colnames(tt))) colnames(tt) <- stat
  if(total) tt <- cbind(tt, Total=apply(as.matrix(tt), 1, sum, na.rm=TRUE)) # create Total column if requested
  # Add column(s) of 0 for those in stat, but not currently in Catch
  addCol <- stat[!(stat %in% colnames(tt))]
  addCol <- matrix(0, nrow=nrow(tt), ncol=length(addCol), dimnames=list(rownames(tt), addCol))
  tt <- cbind(nTrap=Catch[,'nTrap'], tt, addCol)
  tt <- tt[rownames(tt)!='TOTAL',] # omit TOTAL row (if it exists)

  # If requested, combine rodents
  if ('Rodent' %in% combine)
  {
    tt[,'Rat'] <- apply(as.matrix(tt[, colnames(tt) %in% c('Rat','Mouse')]), 1, sum, na.rm=TRUE)
    tt <- tt[, colnames(tt)!='Mouse']
    colnames(tt)[colnames(tt)=='Rat'] <- 'Rodent'
  }

  # If requested, combine mustelids
  if ('Mustelid' %in% combine)
  {
    tt[,'Stoat'] <- apply(as.matrix(tt[, colnames(tt) %in% c('Stoat','Weasel','Ferret')]), 1, sum, na.rm=TRUE)
    tt <- tt[, !(colnames(tt) %in% c('Weasel','Ferret'))]
    colnames(tt)[colnames(tt)=='Stoat'] <- 'Mustelid'
  }

  yLim <- c(0, max(tt[, -1], na.rm=TRUE)) # set y limits for plots
  ttCol <- c(Rodent='gray40', Rat='gray40', Mouse='lightblue',
             Mustelid='brown2', Stoat='brown2', Weasel='brown1', Ferret='brown4',
             Possum='navy', Other='orange', Total='black')

  ttPY <- tt[substring(rownames(tt), 1, 4)==currentYr-1,]
  tt <- tt[substring(rownames(tt), 1, 4)==currentYr,]
  jd <- julian.Date(as.Date(rownames(tt)), origin=as.Date(paste(currentYr-1, '12-31', sep='-')))
  jdPY <- julian.Date(as.Date(rownames(ttPY)), origin=as.Date(paste(currentYr-2, '12-31', sep='-')))

  # Previous year
  par(mar=c(3,3,1.5,2))
  plot(jdPY, rep(0, length(jdPY)), xlab='', ylab='Catch (% traps)',
       xlim=c(0,366), ylim=yLim, xaxt='n', type='n', bty='n', lty='dashed', ...)
  axis(1, c(1,365), c('1 Jan','31 Dec'))
  axis(2, yLim, c('',''), tck=0)
  for(i in colnames(ttPY)[-1])
  {
    lines(jdPY, ttPY[, i], col=ttCol[i], lty='dashed')
  }

  # Current year
  for(i in colnames(tt)[-1])
  {
    lines(jd, tt[, i], col=ttCol[i])
    text(max(jd)*1.02, tt[nrow(tt), i], i, col=ttCol[i], adj=0, cex=0.75) # may need some jittering
  }
  text(jdPY, par()$usr[4]*1.05, ttPY[,'nTrap'], cex=0.75, col='gray50')
  text(jd, par()$usr[4], tt[,'nTrap'], cex=0.75)
}
