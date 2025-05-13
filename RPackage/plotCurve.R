#' Plots titration curves
#'

plotCurve <- function(curve, xlab = 'Titulante (g)', ylab = 'Potencial (mV)', smoothLi = TRUE,
                      smoothDer1 = TRUE, smoothDer2 = FALSE, length = 10000, main = NULL,
                      spar = 0.2, plot = TRUE, vabline = TRUE, mn = NULL) {
  p <- ggplot(data = curve, mapping = aes(x = Titrant, y = Signal)) +
    geom_point(alpha = 0.4, shape = 1) + theme_bw() + labs(x = xlab, y = ylab, title = main)

  if (smoothLi) {
    smoothed <- curveDeriv(curve, length = length)
    smoothed$firstDeriv <- scales::rescale(smoothed$firstDeriv,
                                           to = c(min(curve$Signal), max(curve$Signal)))
    p <- p + geom_line(data = smoothed)
  }
  if (smoothDer1) p <- p + geom_line(data = smoothed, mapping = aes(x = Titrant, y = firstDeriv), color = 2)
  if (smoothDer2) p <- p + geom_line(data = smoothed, mapping = aes(x = Titrant, y = derScaling * secondDeriv), color = 4)
  if (vabline && !missing(mn)) p <- p + geom_vline(xintercept = mn)
  if (plot) print(p)
  return(p)
}
