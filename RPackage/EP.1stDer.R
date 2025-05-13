#' Potentiometric equivalence point using first derivate method
#'

EP.1stDer <- function(curve, length = 10000, sub = FALSE,
                      subregion = c(0.985, 1.015), plot = FALSE,
                      upwards = FALSE, vabline = TRUE, ...) {
  if (sub) {
    l1 <- which.min(abs(curve$aproxTitFrac - subregion[1]))
    l2 <- which.min(abs(curve$aproxTitFrac - subregion[2]))
    oldcurve <- curve
    curve <- curve[l1:l2, ]
    i = 1
    up = TRUE
    while (nrow(curve) < min(c(8, nrow(oldcurve) - 1))) {
      if (up) {
        l2 <- l2 + i
        up <- FALSE
      } else {
        l1 <- l1 - i
        up <- TRUE
        i <-  i + 1
      }
      curve <- oldcurve[l1:l2, ]
    }
  }
  smoothed <- curveDeriv(curve, length = length)
  #mn <- ifelse(upwards, smoothed$Titrant[which.max(smoothed$firstDeriv)],
  #             smoothed$Titrant[which.min(smoothed$firstDeriv)])

  mn <- smoothed$Titrant[which.max(abs(smoothed$firstDeriv))]

  if (plot) {
    plotCurve(curve = curve, length = length, vabline = vabline, mn = mn, ...)
  }
  #  print(mn)
  return(mn)
}
