library(nlme)
library(lme4)
library(ez)
library(car)
library(plyr)
library(ggplot2)
library(reshape2)

source("scripts/functions.R")

licor <- read.csv("data/licor/licor_all.csv", header=TRUE)

licor$species <- as.factor(licor$species)
licor$time <- as.factor(licor$time)
licor$treatment <- as.factor(licor$treatment)
licor$CO2 <- as.factor(licor$CO2)
licor$plant <- as.factor(licor$plant)

licor <- merge(licor, traits)
licor$photoMass <- licor$SLA * licor$photo
licor$condMass <- licor$SLA * licor$cond

licor.A <- subset(licor, species == "acacia")
licor.C <- subset(licor, species == "cas")
licor.E <- subset(licor, species == "euc")

# plot some graphs 

licor.A_photo <- getSummary(licor.A, photo, acacia)
licor.A_cond <- getSummary(licor.A, cond, acacia)

licor.C_photo <- getSummary(licor.C, photo, acacia)
licor.C_cond <- getSummary(licor.C, cond, acacia)

licor.E_photo <- getSummary(licor.E, photo, acacia)
licor.E_cond <- getSummary(licor.E, cond, acacia)

facetPlot(licor.A_photo, acacia, photosyntheticRate)
facetPlot(licor.C_photo, casuarina, photosyntheticRate)
facetPlot(licor.E_photo, eucalyptus, photosyntheticRate)

facetPlot(licor.A_cond, acacia, stomatalConductance)
facetPlot(licor.C_cond, casuarina, stomatalConductance)
facetPlot(licor.E_cond, eucalyptus, stomatalConductance)



licor.A_photoMass <- getSummary(licor.A, photoMass, acacia)
licor.C_photoMass <- getSummary(licor.C, photoMass, acacia)
licor.E_photoMass <- getSummary(licor.E, photoMass, acacia)

facetPlot(licor.A_photoMass, acacia, photosyntheticRateMass)
facetPlot(licor.C_photoMass, casuarina, photosyntheticRateMass)
facetPlot(licor.E_photoMass, eucalyptus, photosyntheticRateMass)





licor.A_condMass <- getSummary(licor.A, condMass, acacia)
licor.C_condMass <- getSummary(licor.C, condMass, acacia)
licor.E_condMass <- getSummary(licor.E, condMass, acacia)

facetPlot(licor.A_condMass, acacia, condsyntheticRateMass)
facetPlot(licor.C_condMass, casuarina, condsyntheticRateMass)
facetPlot(licor.E_condMass, eucalyptus, condsyntheticRateMass)


# now do some anovas

licor.A <- subset(licor, species == "acacia")
licor.C <- subset(licor, species == "cas")
licor.E <- subset(licor, species == "euc")

aov.A <- aov(photo ~ treatment * CO2, data = licor.A)
#summary(aov.A)

Anova.A2 <- lm(photo ~ CO2 * treatment, data = licor.A)
Anova(Anova.A2, type="2")

summary(aov.A)
TukeyHSD(aov.A, conf.level = 0.90)    
replications(photo ~ treatment * CO2, data = licor.A)

aov.C <- aov(photo ~ treatment * CO2, data = licor.C)
summary(aov.C)
TukeyHSD(aov.C, conf.level = 0.90)    
replications(photo ~ treatment * CO2, data = licor.C)

aov.E <- aov(photo ~ treatment * CO2, data = licor.E)
summary(aov.E)
TukeyHSD(aov.E, conf.level = 0.90) 
replications(photo ~ treatment * CO2, data = licor.E)




require(agricolae)

A.interaction <- with(licor.A, interaction(CO2, treatment))
aov.A <- aov(photo ~ A.interaction, data = licor.A)
HSD.test(aov.A, "A.interaction", alpha = 0.1, group=TRUE, console=TRUE)

C.interaction <- with(licor.C, interaction(CO2, treatment))
aov.C <- aov(photo ~ C.interaction, data = licor.C)
HSD.test(aov.C, "C.interaction", alpha = 0.1, group=TRUE, console=TRUE)

E.interaction <- with(licor.E, interaction(CO2, treatment))
aov.E <- aov(photo ~ E.interaction, data = licor.E)
HSD.test(aov.E, "E.interaction", alpha = 0.1, group=TRUE, console=TRUE)





