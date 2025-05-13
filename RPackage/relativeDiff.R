#' Summary
#'

relativeDiff <- function(x, y) {
  if (length(x) > 1) x <- mean(na.omit(x))
  if (length(y) > 1) y <- mean(na.omit(y))
  RD <- abs(x - y)/mean(c(x, y))
  print(paste0(round(RD * 100, 4), ' %'))
  return(RD)
}
