#' Summary
#'

describe <- function(x, subset = NULL, signif = 6) {
  if (!missing(subset)) x <- x[subset]
  x <- na.omit(x)
  print(paste0('Promedio: ', signif(mean(x), signif)))
  print(paste0('DesEstan: ', signif(sd(x), signif)))
  print(paste0('CoefVari: ', signif(100 * sd(x)/mean(x), signif), ' %'))
  print(paste0('n.woutNA: ', length(x)))
  print(paste0('SD_media: ', signif(sd(x)/sqrt(length(x)), signif)))
  print(paste0('RSDmedia: ', signif(100 * sd(x)/mean(x)/sqrt(length(x)), signif), ' %'))
}
