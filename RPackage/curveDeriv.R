curveDeriv <- function(curve, length = 10000) {
  newX <- seq(curve$Titrant[1], curve$Titrant[length(curve$Titrant)], length = length)
  curveSmooth <- predict(smooth.spline(curve$Titrant, curve$Signal), newX, 0)$y
  curveSmDer1 <- predict(smooth.spline(curve$Titrant, curve$Signal), newX, 1)$y
  curveSmDer2 <- predict(smooth.spline(curve$Titrant, curve$Signal), newX, 2)$y

  df <- data.frame(Titrant = newX, Signal = curveSmooth,
                   firstDeriv = curveSmDer1, secondDeriv = curveSmDer2)

  return(df)
}
