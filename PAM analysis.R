PAM <- read.csv("data/PAM/PAM_preflood.csv", header=TRUE)
PAM$glasshouse <- as.factor(PAM$glasshouse)
PAM$CO2 <- as.factor(PAM$CO2)

# acacia

PAMacacia_GH.aov <- aov(acacia ~ glasshouse, data = PAM)
summary(PAMacacia_GH.aov)
TukeyHSD(PAMacacia_GH.aov)

PAMacacia_CO2.aov <- aov(acacia ~ CO2, data = PAM)
summary(PAMacacia_CO2.aov)

# casuarina

PAMcasuarina_GH.aov <- aov(casuarina ~ glasshouse, data = PAM)
summary(PAMcasuarina_GH.aov)
TukeyHSD(PAMcasuarina_GH.aov)

PAMcasuarina_CO2.aov <- aov(casuarina ~ CO2, data = PAM)
summary(PAMcasuarina_CO2.aov)

# eucalyptus

PAMeucalyptus_GH.aov <- aov(eucalyptus ~ glasshouse, data = PAM)
summary(PAMeucalyptus_GH.aov)
TukeyHSD(PAMeucalyptus_GH.aov)

PAMeucalyptus_CO2.aov <- aov(eucalyptus ~ CO2, data = PAM)
summary(PAMeucalyptus_CO2.aov)
