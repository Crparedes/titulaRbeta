#' Converts LIMS reports to data frames
#'

lims2t.curve <- function(file, skip = 25, nrows = 101, header = FALSE, colTit = 2, colSignal = 3,
                         plot = TRUE, smoothLi = TRUE, smoothDer1 = TRUE, derScaling = 0.6, length = 10000,
                         main = NULL) {
  df <- read.delim(file = file, skip = skip, header = header, nrows = nrows)
  df <- data.frame(Titrant = df[, colTit], Signal = df[, colSignal])
  df$aproxTitFrac <- df$Titrant / EP.1stDer(curve = df, length = length, sub = FALSE)

  if(plot) plotCurve(df, smoothLi = smoothLi, smoothDer1 = smoothDer1,
                     length = length, main = main)
  return(df)
}
