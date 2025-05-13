source('Disoluciones.R')

modoTit <- c(rep('Trazable', 3), rep('Estandarizacion', 2), rep('Trazable', 2), rep('Estandarizacion', 3))
masasPb <- c(9.5246, 9.5999, 9.5192, 9.5619, 9.4896, 9.4665, 9.4973, 9.4983, 9.5128, 9.5054)

# Archivos raw
path <- './dat.2021.08.27.Pb-StandarizationEDTA/'
(files <- paste0(path, list.files(path = path)))
n1 <- 10

rawCurves <- list()

Meq.void <- c_Pb.Std <- c_Pb.Std.tr <- c_Pb.MeqNum <- rep(NA, n1)
MeqComplete <- data.frame(Meq1stDer10000 = Meq.void, Meq1stDer10000trim = Meq.void,
                          MeqNum = Meq.void, rangNum = Meq.void)

#pdf('TitPb220721.pdf', width = 7, height = 7)
for (i in 1:n1) {
  upwards <- FALSE #ifelse(modoTit[i] == 'Retroceso', TRUE, FALSE)
  rawCurves[[i]]   <- df2t.curve(df = read.csv(file = files[i], header = FALSE, skip = 4,
                                               sep = ifelse(TRUE, ';', ','),
                                               dec = ifelse(TRUE, ',', '.')),
                                 plot = FALSE, upwards = upwards)
  (MeqComplete$Meq1stDer10000[i]   <- EP.1stDer(curve = rawCurves[[i]], length = 10000, plot = TRUE,
                                                upwards = upwards, main = paste0(i, ', ', modoTit[i])))
  #(MeqComplete$Meq1stDer10000trim[i]   <- EP.1stDer(curve = rawCurves[[i]], length = 10000,
  #                                                  sub = TRUE, subregion = c(0.95, 1.05), plot = TRUE,
  #                                                  upwards = upwards, main = paste0(i, ', ', modoTit[i])))
  numGrad <- EP.numGrad(curve = rawCurves[[i]], upwards = upwards, main = paste0(i, ', ', modoTit[i]),
                        inter = TRUE, plot = TRUE)
  (MeqComplete$MeqNum[i] <-  numGrad[[1]])
  (MeqComplete$rangNum[i] <-  diff(numGrad[[2]]))

  if (modoTit[i] == 'Directa') {

  }
  if (modoTit[i] == 'Retroceso') {
    c_Pb.Std[i] <- ((masEDTA[i] * B_dis_EDTA[1] * c_Na2EDTA_1) - (masasCu[i] * B_muestra[1] * c_Cu) -
                      (MeqComplete$Meq1stDer10000[i] * c_Zn * B_muestra[1])) /
      (masasPb[i] * B_muestra[1]) * PbMW
    #c_Pb.Std.tr[i] <- ((masEDTA[i] * B_dis_EDTA[1] * c_Na2EDTA_1) - (masasCu[i] * c_Cu) - 
    # (MeqComplete$Meq1stDer10000trim[i] * c_Zn)) /
    #(masasPb[i] * B_muestra[1]) * PbMW
    c_Pb.MeqNum[i] <- ((masEDTA[i] * B_dis_EDTA[1] * c_Na2EDTA_1) - (masasCu[i] * B_muestra[1] * c_Cu) -
                         (MeqComplete$MeqNum[i] * B_muestra[1] * c_Zn)) /
      (masasPb[i] * B_muestra[1]) * PbMW
  }
}

# Estandarizaci'on disoluci'on de plomo
(c_Pb.Std <- ((MeqComplete$Meq1stDer10000[modoTit == 'Trazable'] * c_Na2EDTA_1)) /
  (masasPb[modoTit == 'Trazable'])) #mmol / kg
describe(c_Pb.Std, signif = 6)
plot(c_Pb.Std)
grubbs.test(c_Pb.Std)
c_Pb.Std[4] <- NA
describe(c_Pb.Std, signif = 6)
plot(c_Pb.Std)

(c_Pb.MeqNum <- ((MeqComplete$MeqNum[modoTit == 'Trazable'] * c_Na2EDTA_1)) /
    (masasPb[modoTit == 'Trazable']))
c_Pb.MeqNum[4] <- NA
describe(c_Pb.MeqNum, signif = 6)
plot(c_Pb.MeqNum)

relativeDiff(c_Pb.Std, c_Pb.MeqNum)

# Estandarizaci'on disoluci'on de EDTA
(c_EDTABlended <- ((masasPb[modoTit == 'Estandarizacion'] * mean(na.omit(c_Pb.Std)))) /
    (MeqComplete$Meq1stDer10000[modoTit == 'Estandarizacion'])) #mmol / kg
describe(c_EDTABlended, signif = 6)
plot(c_EDTABlended)
grubbs.test(c_EDTABlended)
c_EDTABlended[3] <- NA
describe(c_EDTABlended, signif = 6)
plot(c_EDTABlended)


(c_EDTABlended.MeqNum <- ((masasPb[modoTit == 'Estandarizacion'] * mean(na.omit(c_Pb.MeqNum)))) /
    (MeqComplete$MeqNum[modoTit == 'Estandarizacion'])) #mmol / kg
describe(c_EDTABlended.MeqNum, signif = 6)
plot(c_EDTABlended.MeqNum)
grubbs.test(c_EDTABlended.MeqNum)
c_EDTABlended.MeqNum[3] <- NA
describe(c_EDTABlended.MeqNum, signif = 6)
plot(c_EDTABlended.MeqNum)

relativeDiff(c_EDTABlended, c_EDTABlended.MeqNum)
#dev.off()

(MeqComplete$Meq1stDer10000 - MeqComplete$MeqNum) * 1000
(c_Pb.Std - c_Pb.MeqNum)


# Incertidumbre
library(propagate)
C_Pb.BlendA <- propagate(expr = expression(), 
                         data = , 
                         do.sim = FALSE )