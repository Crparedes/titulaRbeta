#' Numeric final point in potentiometric indication
#'

EP.numGrad <- function(curve, length = 10000, plot = FALSE,
                       upwards = FALSE, vabline = TRUE,
                       inter = FALSE, ...) {
  n <- nrow(curve)
  numGrad <- (curve$Signal[2:n] - curve$Signal[1:(n - 1)]) /
    (curve$Titrant[2:n] - curve$Titrant[1:(n - 1)])
  epZ1 <- ifelse(upwards, which.max(numGrad), which.min(numGrad))

  interV <- curve$Titrant[epZ1:(epZ1 + 1)]
  mn <- mean(interV)
  if (plot) {
    plotCurve(curve = curve, length = length, vabline = vabline, mn = mn, smoothDer1 = FALSE, ...)
  }
  #  print(mn)
  if (inter) mn <- list(mn, interV)
  return(mn)
}
