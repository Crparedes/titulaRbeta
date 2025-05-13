library(ggplot2)
library(outliers)
library(propagate)
# devtools::install_github('https://github.com/crparedes/titRation')
library(titRation)
# devtools::install_github('https://github.com/sergmonic/AtomicWeights')
library(AtomicWeights)
# devtools::install_github('https://github.com/crparedes/masscor')
library(masscor)
load(file = 'BalSMQB.RData')
attach(BalSMQB)

# Densidad del aire
(rho_air <- airDensity(Temp = 18, p = 750, h = 45)) # [kg/m3]
(u_rho_air <- uncertAirDensity(Temp = 18, p = 750, h = 45, u_Temp = 1.8, u_p = 2, u_h = 3)) # [kg/m3]

# Salidas App
round(rho_air - 0.000893463, 8)
round(u_rho_air - 6.06e-06)

# Preparaci'on EDTA disódico
(MW_Na2EDTA <- as.numeric(getMolarMass(atomsType = c('C', 'H', 'N', 'Na', 'O'), atomsNumber = c(10, 18, 2, 2, 10)))) # [g/mol]
# Pureza Na2EDTA: Certificado UNIIM GSO 2960-84
x_Na2EDTA <- c(0.9986, 0.0003/2) # [g/g]
# Masa EDTA (MRC)
(m_Na2EDTA <- c(convMass(calibCert = MT.XPE.205, reading = 374.19, units = 'mg'),
                uncertConvMass(calibCert = MT.XPE.205, reading = 374.19, units = 'mg'))) # [mg]
3.25476 + 0.37419 - 3.62900
# Flotabilidad EDTA
(B_EDTA <- c(MABC(rho = 0.860, rho_air = rho_air),
             uncertMABC(rho = 0.860, rho_air = rho_air, u_rho = 0.005/sqrt(3), u_rho_air = u_rho_air)))
# Masa EDTA Disoluci'on
(mf_Na2EDTA <- c(convMass(calibCert = MT.XPE.205, reading = 100.0823),
                 uncertConvMass(calibCert = MT.XPE.205, reading = 100.0823))) # [g]
13.8678 + 100.0824 - 113.9503

# Flotabilidad disoluci'on EDTA
rhoEDTA <- c(1.000970, 1.000060, 0.998889)
(B_dis_EDTA <- c(MABC(rho = mean(rhoEDTA), rho_air = rho_air),
                 uncertMABC(rho = mean(rhoEDTA), rho_air = rho_air,
                            u_rho = diff(range(rhoEDTA))/(2 * sqrt(3)), u_rho_air = u_rho_air)))
# Densidad disoluci'on
# 0.999973 +- 0.000600733

(c_Na2EDTA.Prop <- propagate(expr = expression(m_Na2EDTA * x_Na2EDTA * B_EDTA / (MW_Na2EDTA * mf_Na2EDTA * B_dis_EDTA) * 1000),
                             data = cbind(m_Na2EDTA, x_Na2EDTA, B_EDTA, MW_Na2EDTA, mf_Na2EDTA, B_dis_EDTA),
                             do.sim = FALSE)) # [mg] / ([mg/mmol] * [g]) * 1000[g/kg] = [mmol/kg]
c_Na2EDTA.Prop$prop[3] / c_Na2EDTA.Prop$prop[1] * 100
barplot(c_Na2EDTA.Prop$rel.contr)

# Concentraci'on EDTA
(c_Na2EDTA_1 <- c_Na2EDTA.Prop$prop[1]) # [mmol/kg]
# Preparado 11 de agosto

# Salidas APP
#$`Concentracion [mmol/kg]`
#[1] 10.0316

#$`Incertidumbre [mmol/kg]`
#[1] 0.00167

PbMW <- 207.209
signif(0.005/sqrt(3), 2)
# Pureza Pb(NO3)2 MRC SRM 928 NIST
W_PbNO3.2 <- c(1, 0.0003/2) # [mg/mg]
# Fraccion de plomo en el reactivo
(relPb_PbNO3.2 <- as.numeric(getAtomicWeight('Pb')[1] /
                               getMolarMass(atomsType = c('Pb', 'N', 'O'), atomsNumber = c(1, 2, 6))[1])) # [mg/mg]
(nu_Pb.PbNO3.2 <- c(relPb_PbNO3.2,
                    relPb_PbNO3.2 * sqrt(as.numeric(getAtomicWeight('Pb')[2]/getAtomicWeight('Pb')[1])^2 +
                                           as.numeric(getMolarMass(atomsType = c('Pb', 'N', 'O'), atomsNumber = c(1, 2, 6))[2]/
                                                        getMolarMass(atomsType = c('Pb', 'N', 'O'), atomsNumber = c(1, 2, 6))[1])^2)))
(nu_Pb.PbNO3.2MRC <- c(207.209 / 331.219,
                       (207.209 / 331.219) * sqrt((0.005/sqrt(3)/207.209)^2 + (0.005/sqrt(3)/331.219)^2)))
nu_Pb.PbNO3.2[2] / nu_Pb.PbNO3.2[1] * 100
nu_Pb.PbNO3.2MRC[2] / nu_Pb.PbNO3.2MRC[1] * 100
# Masa de reactivo
(m_PbNO3.2 <- c(convMass(calibCert = MT.XPE.205, reading = 212.22, units = 'mg'),
                uncertConvMass(calibCert = MT.XPE.205, reading = 212.22, units = 'mg'))) # [mg]
3.975592 - 3.256291 - 0.719301
# Flotabilidad reactivo
(B_PbNO3.2 <- c(MABC(rho = 4.53, rho_air = rho_air),
                uncertMABC(rho = 4.53, rho_air = rho_air, u_rho = 0.05/sqrt(3), u_rho_air = u_rho_air)))
503.3662 - 450.3686 - 52.9974
# Masa final disoluci'on
(mf_PbNO3.2 <- c(convMass(calibCert = MT.XPE.205, reading = 133.0615),
                 uncertConvMass(calibCert = MT.XPE.205, reading = 133.0615))) # [g]

rhoDisPb <- c(1.010464, 1.009394, 1.008077, 1.008756, 1.007712, 1.006422, 1.001186, 1.000280, 0.999058)
(B_muestra <- B_DisPb <- c(MABC(rho = mean(rhoDisPb), rho_air = rho_air),
                           uncertMABC(rho = mean(rhoDisPb), rho_air = rho_air,
                                      u_rho = diff(range(rhoDisPb))/(2 * sqrt(3)), u_rho_air = u_rho_air)))
# Densidad disoluci'on
# 1.005705 +- 0.003292629

(v_Pb.teor <- propagate(expr = expression(m_PbNO3.2 * B_PbNO3.2 * W_PbNO3.2 * nu_Pb.PbNO3.2MRC / (mf_PbNO3.2 * B_DisPb) * 1000),
                        data = cbind(m_PbNO3.2, B_PbNO3.2, W_PbNO3.2, nu_Pb.PbNO3.2MRC, mf_PbNO3.2, B_DisPb),
                        do.sim = FALSE)) # [mg] / [g] * 1000[g/kg] = [mg/kg]
(v_Pb.te <- v_Pb.teor$prop[1]) # [mg/kg]
barplot(v_Pb.teor$rel.contr)
v_Pb.teor$prop[3] / v_Pb.teor$prop[1] * 100
PbMW <- 207.209
RR <- c_Na2EDTA_1/(v_Pb.te/PbMW)