massSTD  <- c(0.01, 0.5, 1, 10, 20, 50, 100, 150, 200, 210, 220)  ## [g]
indError <- c(0.00, 0.00, 0.00, 0.00, -0.01, -0.01, 0.01, 0.00, 0.08, 0.08, 0.07) ## [mg]
uncert   <- c(0.04, 0.04, 0.05, 0.08, 0.10, 0.12, 0.19, 0.31, 0.38, 0.45, 0.47) ## [mg]
d <- 0.01 ## [mg]
traceability <- 'Set of weights class E2. Certificate number 4781 INM, 2021-05-12.'

library(masscor)
MT.XPE.205 <- calibCert(balanceID = 'MT XPE 205', serial = 'B7438484111', certificate = 5366,
                        d = d, d.units = 'mg',
                        indError = data.frame(massSTD, indError, uncert),
                        indError.units = c('g', 'mg', 'mg'),
                        rep = data.frame(load = c(0.01, 100, 220), sd = c(0.01, 0.02, 0.01)),
                        rep.units = c('g', 'mg'),
                        eccen = c(100, 0.07), eccen.units = c('g', 'mg'),
                        classSTD = 'E2', traceability = traceability,
                        Temp = c(20.3, 21.1), ## [deg.C]
                        p = c(752.6, 753.0), ## [hPa]
                        h = c(46.0, 48.8), ## [%]
                        unitsENV = c('deg.C', 'hPa', '%'),
                        institution = 'Instituto Nacional de Metrologia de Colombia',
                        date = '2021-08-11')

saveRDS(MT.XPE.205, file = 'calibCert/Mettler Toledo XPE 205 (2021-08-11).rds')
