#' Converts a dataframe to


df2t.curve <- function(df, colTit = 1, colSignal = 2,
                       plot = TRUE, smoothLi = TRUE, smoothDer1 = TRUE, derScaling = 0.6, length = 10000,
                       main = NULL, upwards = FALSE) {
  df <- data.frame(Titrant = df[, colTit], Signal = df[, colSignal])
  df$aproxTitFrac <- df$Titrant / EP.1stDer(curve = df, length = length, sub = FALSE, upwards = upwards)

  if(plot) plotCurve(df, smoothLi = smoothLi, smoothDer1 = smoothDer1,
                     length = length, main = main)
  return(df)
}
