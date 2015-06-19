library(nlme)
library(lme4)
library(ez)
library(plyr)
library(ggplot2)
library(reshape2)
library(car)
library(effsize)
library(heplots)
library(MuMIn)
library(lsr)

source("scripts/functions.R")

## LICOR ##


licor <- read.csv("data/licor/licor_all1.csv", header=TRUE)
#licor <- subset(licor, treatment != "waterlogged")
traits <- read.csv("data/harvest/traits1.csv", header=TRUE)

licor$species <- as.factor(licor$species)
licor$time <- as.factor(licor$time)
licor$treatment <- as.factor(licor$treatment)
licor$CO2 <- as.factor(licor$CO2)
licor$plant <- as.factor(licor$plant)

licor <- merge(licor, traits)
licor$photoMass <- licor$SLA * licor$photo
licor$condMass <- licor$SLA * licor$cond

licor$WUE <- licor$photo/licor$Trmmol

licor$treatment <- factor(licor$treatment, levels = c("control", "waterlogged", "recovery"))

licor.A <- subset(licor, species == "acacia")
licor.C <- subset(licor, species == "cas")
licor.E <- subset(licor, species == "euc")


# plot some graphs 

licor.A_photo <- getSummary(licor.A, photo, acacia)
licor.A_cond <- getSummary(subset(licor.A, cond < 0.8), cond, acacia)
licor.A_WUE <- getSummary(licor.A, WUE, acacia)
licor.A_tr <- getSummary(licor.A, Trmmol, acacia)


licor.C_photo <- getSummary(licor.C, photo, cas)
licor.C_cond <- getSummary(licor.C, cond, cas)
licor.C_WUE <- getSummary(licor.C, WUE, cas)
licor.C_tr <- getSummary(licor.C, Trmmol, cas)


licor.E_photo <- getSummary(licor.E, photo, euc)
licor.E_cond <- getSummary(licor.E, cond, euc)
licor.E_WUE <- getSummary(licor.E, WUE, euc)
licor.E_tr <- getSummary(licor.E, Trmmol, euc)


facetPlot(licor.A_photo, acacia, photosyntheticRate)
facetPlot(licor.C_photo, casuarina, photosyntheticRate)
facetPlot(licor.E_photo, eucalyptus, photosyntheticRate)

facetPlot(licor.A_cond, acacia, stomatalConductance)
facetPlot(licor.C_cond, casuarina, stomatalConductance)
facetPlot(licor.E_cond, eucalyptus, stomatalConductance)

facetPlot(licor.A_WUE, acacia, WUE)
facetPlot(licor.C_WUE, casuarina, WUE)
facetPlot(licor.E_WUE, eucalyptus, WUE)

facetPlot(licor.A_tr, acacia, Trmmol)
facetPlot(licor.C_tr, casuarina, Trmmol)
facetPlot(licor.E_tr, eucalyptus, Trmmol)

#licor.A_photoMass <- getSummary(licor.A, photoMass, acacia)
#licor.C_photoMass <- getSummary(licor.C, photoMass, acacia)
#licor.E_photoMass <- getSummary(licor.E, photoMass, acacia)

#facetPlot(licor.A_photoMass, acacia, photosyntheticRateMass)
#facetPlot(licor.C_photoMass, casuarina, photosyntheticRateMass)
#facetPlot(licor.E_photoMass, eucalyptus, photosyntheticRateMass)

#licor.A_condMass <- getSummary(licor.A, condMass, acacia)
#licor.C_condMass <- getSummary(licor.C, condMass, acacia)
#licor.E_condMass <- getSummary(licor.E, condMass, acacia)

#facetPlot(licor.A_condMass, acacia, condsyntheticRateMass)
#facetPlot(licor.C_condMass, casuarina, condsyntheticRateMass)
#facetPlot(licor.E_condMass, eucalyptus, condsyntheticRateMass)


# photosynthetic rate


photo.A <- aov(photo ~ CO2 * treatment, data = licor.A)
etaSquared(photo.A, anova = TRUE)

photo.C <- aov(photo ~ CO2 * treatment, data = licor.C)
etaSquared(photo.C, anova = TRUE)

photo.E <- aov(photo ~ CO2 * treatment, data = licor.E)
etaSquared(photo.E, anova = TRUE)


# transpiration

Trmmol.A <- aov(Trmmol ~ CO2 * treatment, data = licor.A)
etaSquared(Trmmol.A, anova = TRUE)


Trmmol.C <- aov(Trmmol ~ CO2 * treatment, data = licor.C)
etaSquared(Trmmol.C, anova = TRUE)

Trmmol.E <- aov(Trmmol ~ CO2 * treatment, data = licor.E)
etaSquared(Trmmol.E, anova = TRUE)



# water use efficiency

WUE.A <- aov(WUE ~ CO2 * treatment, data = licor.A)
etaSquared(WUE.A, anova = TRUE)

WUE.C <- aov(WUE ~ CO2 * treatment, data = licor.C)
etaSquared(WUE.C, anova = TRUE)

WUE.E <- aov(WUE ~ CO2 * treatment, data = licor.E)
etaSquared(WUE.E, anova = TRUE)


# stomatal conductance

cond.A <- aov(cond ~ CO2 * treatment, data = licor.A)
etaSquared(cond.A, anova = TRUE)

cond.C <- aov(cond ~ CO2 * treatment, data = licor.C)
etaSquared(cond.C, anova = TRUE)

cond.E <- aov(cond ~ CO2 * treatment, data = licor.E)
etaSquared(cond.E, anova = TRUE)


## TRAITS ##

traits <- read.csv("data/harvest/traits1.csv", header=T)
#traits <- subset(traits, treatment != "waterlogged")
traits$dryBiomass <- traits$dryRootBiomass + traits$dryShootBiomass

traits <- data.frame(cbind(
  traits["species"],
  traits["plant"],
  traits["CO2"],
  traits["treatment"],
  traits["dryBiomass"],
  traits["dryRootBiomass"], 
  traits["dryFineRootMass"],
  traits["dryShootBiomass"],
  traits["RMF"],
  traits["rootFineProportion"],
  traits["fineRootDMC"],
  traits["coarseRootDMC"],
  traits["SLA"], 
  traits["stemDensity"]))

traits$treatment <- factor(traits$treatment, levels = c("control", "waterlogged", "recovery"))

#traits <- subset(traits, treatment != "waterlogged")

traits.A <- subset(traits, species == "acacia")
traits.C <- subset(traits, species == "cas")
traits.E <- subset(traits, species == "euc")

plot.means(traits.A, acacia)
plot.means(traits.C, casuarina)
plot.means(traits.E, eucalyptus)

rootFineProportion.A <- getSummary_traits(traits.A, rootFineProportion, acacia)
rootFineProportion.C <- getSummary_traits(traits.C, rootFineProportion, cas)
rootFineProportion.E <- getSummary_traits(traits.E, rootFineProportion, euc)

SLA.A <- getSummary_traits(traits.A, SLA, acacia)
SLA.C <- getSummary_traits(traits.C, SLA, cas)
SLA.E <- getSummary_traits(traits.E, SLA, euc)

RMF.A <- getSummary_traits(traits.A, RMF, acacia)
RMF.C <- getSummary_traits(traits.C, RMF, cas)
RMF.E <- getSummary_traits(traits.E, RMF, euc)

fineRootDMC.A <- getSummary_traits(traits.A, fineRootDMC, acacia)
fineRootDMC.C <- getSummary_traits(traits.C, fineRootDMC, cas)
fineRootDMC.E <- getSummary_traits(traits.E, fineRootDMC, euc)

stemDensity.A <- getSummary_traits(na.omit(traits.A), stemDensity, acacia)
stemDensity.C <- getSummary_traits(na.omit(subset(traits.C,, stemDensity > 0.15)), stemDensity, cas) # remove outlier
stemDensity.E <- getSummary_traits(traits.E, stemDensity, euc)

traits1 <- traits

traits.A1 <- traits.A
traits.C1 <- traits.C
traits.E1 <- traits.E


traits <- read.csv("data/harvest/traits1.csv", header=T)
traits$dryBiomass <- traits$dryRootBiomass + traits$dryShootBiomass

traits <- data.frame(cbind(
  traits["species"],
  traits["plant"],
  traits["CO2"],
  traits["treatment"],
  sqrt(traits["dryBiomass"]),
  sqrt(traits["dryRootBiomass"]), 
  sqrt(traits["dryFineRootMass"]),
  sqrt(traits["dryShootBiomass"]),
  log10(traits["RMF"]),
  traits["rootFineProportion"],
  traits["fineRootDMC"],
  log10(traits["SLA"]), 
  traits["stemDensity"]))

#traits <- subset(traits, stemDMC > 0.15)


# find outliers
#plot(traits$dryRootBiomass)
#plot(traits$rootFineProportion)
#plot(traits$dryFineRootMass)
#plot(traits$dryShootBiomass)
#plot(traits$SLA)
#plot(traits$LDMC)
#plot(traits$stemDMC)
#pot(traits$RMF)
#plot(traits$fineRootDMC)


traits.A <- subset(traits, species == "acacia")
traits.C <- subset(traits, species == "cas")
traits.E <- subset(traits, species == "euc")


rootFineProportion.A.aov <- aov(rootFineProportion ~ CO2 * treatment, data = traits.A)
etaSquared(rootFineProportion.A.aov, anova = TRUE)
rootFineProportion.C.aov <- aov(rootFineProportion ~ CO2 * treatment, data = traits.C)
etaSquared(rootFineProportion.C.aov, anova = TRUE)
TukeyHSD(rootFineProportion.C.aov)
rootFineProportion.E.aov <- aov(rootFineProportion ~ CO2 * treatment, data = traits.E)
etaSquared(rootFineProportion.E.aov, anova = TRUE)


SLA.A.aov <- aov(SLA ~ CO2 * treatment, data = traits.A)
etaSquared(SLA.A.aov, anova = TRUE)
SLA.C.aov <- aov(SLA ~ CO2 * treatment, data = traits.C)
etaSquared(SLA.C.aov, anova = TRUE)
traits.E <- subset(traits.E, SLA < 1.65)
SLA.E.aov <- aov(SLA ~ CO2 * treatment, data = traits.E)
etaSquared(SLA.E.aov, anova = TRUE)
traits.E <- subset(traits.E, species == "euc")


RMF.A.aov <- aov(RMF ~ CO2 * treatment, data = traits.A)
etaSquared(RMF.A.aov, anova = TRUE)
TukeyHSD(RMF.A.aov)
RMF.C.aov <- aov(RMF ~ CO2 * treatment, data = traits.C)
etaSquared(RMF.C.aov, anova = TRUE)
RMF.E.aov <- aov(RMF ~ CO2 * treatment, data = traits.E)
etaSquared(RMF.E.aov, anova = TRUE)


fineRootDMC.A.aov <- aov(fineRootDMC ~ CO2 * treatment, data = traits.A)
etaSquared(fineRootDMC.A.aov, anova = TRUE)
TukeyHSD(fineRootDMC.A.aov)
fineRootDMC.C.aov <- aov(fineRootDMC ~ CO2 * treatment, data = traits.C)
etaSquared(fineRootDMC.C.aov, anova = TRUE)
fineRootDMC.E.aov <- aov(fineRootDMC  ~ CO2 * treatment, data = traits.E)
etaSquared(fineRootDMC.E.aov, anova = TRUE)


stemDensity.A.aov <- aov(stemDensity ~ CO2 * treatment, data = traits.A)
etaSquared(stemDensity.A.aov, anova = TRUE)
traits.C <- subset(traits.C, stemDensity < 1)
stemDensity.C.aov <- aov(stemDensity ~ CO2 * treatment, data = traits.C)
etaSquared(stemDensity.C.aov, anova = TRUE)
stemDensity.E.aov <- aov(stemDensity ~ CO2 * treatment, data = traits.E)
etaSquared(stemDensity.E.aov, anova = TRUE)


RMF.A.aov <- aov(RMF ~ CO2 * treatment, data = traits.A)
etaSquared(RMF.A.aov, anova = TRUE)
TukeyHSD(RMF.A.aov)
RMF.C.aov <- aov(RMF ~ CO2 * treatment, data = traits.C)
etaSquared(RMF.C.aov, anova = TRUE)
RMF.E.aov <- aov(RMF ~ CO2 * treatment, data = traits.E)
etaSquared(RMF.E.aov, anova = TRUE)



# removing waterlogged treatment for biomass comparisons


traits1 <- subset(traits1, treatment != "waterlogged")

traits.A1 <- subset(traits1, species == "acacia")
traits.C1 <- subset(traits1, species == "cas")
traits.E1 <- subset(traits1, species == "euc")

dryBiomass.A <- getSummary_traits(traits.A1, dryBiomass, acacia)
dryBiomass.C <- getSummary_traits(traits.C1, dryBiomass, cas)
dryBiomass.E <- getSummary_traits(traits.E1, dryBiomass, euc)

dryRootBiomass.A <- getSummary_traits(traits.A1, dryRootBiomass, acacia)
dryRootBiomass.C <- getSummary_traits(traits.C1, dryRootBiomass, cas)
dryRootBiomass.E <- getSummary_traits(traits.E1, dryRootBiomass, euc)

dryFineRootMass.A <- getSummary_traits(traits.A1, dryFineRootMass, acacia)
dryFineRootMass.C <- getSummary_traits(traits.C1, dryFineRootMass, cas)
dryFineRootMass.E <- getSummary_traits(traits.E1, dryFineRootMass, euc)

dryShootBiomass.A <- getSummary_traits(traits.A1, dryShootBiomass, acacia)
dryShootBiomass.C <- getSummary_traits(traits.C1, dryShootBiomass, cas)
dryShootBiomass.E <- getSummary_traits(traits.E1, dryShootBiomass, euc)


traits <- subset(traits, treatment != "waterlogged")
traits.A <- subset(traits, species == "acacia")
traits.C <- subset(traits, species == "cas")
traits.E <- subset(traits, species == "euc")

dryBiomass.A.aov <- aov(dryBiomass ~ CO2 * treatment, data = traits.A)
etaSquared(dryBiomass.A.aov, anova = TRUE)
dryBiomass.C.aov <- aov(dryBiomass ~ CO2 * treatment, data = traits.C)
etaSquared(dryBiomass.C.aov, anova = TRUE)
TukeyHSD(dryBiomass.C.aov)
dryBiomass.E.aov <- aov(dryBiomass ~ CO2 * treatment, data = traits.E)
etaSquared(dryBiomass.E.aov, anova = TRUE)


dryFineRootMass.A.aov <- aov(dryFineRootMass ~ CO2 * treatment, data = traits.A)
etaSquared(dryFineRootMass.A.aov, anova = TRUE)
dryFineRootMass.C.aov <- aov(dryFineRootMass ~ CO2 * treatment, data = traits.C)
etaSquared(dryFineRootMass.C.aov, anova = TRUE)
dryFineRootMass.E.aov <- aov(dryFineRootMass ~ CO2 * treatment, data = traits.E)
etaSquared(dryFineRootMass.E.aov, anova = TRUE)


dryRootBiomass.A.aov <- aov(dryRootBiomass ~ CO2 * treatment, data = traits.A)
etaSquared(dryRootBiomass.A.aov, anova = TRUE)
dryRootBiomass.C.aov <- aov(dryRootBiomass ~ CO2 * treatment, data = traits.C)
etaSquared(dryRootBiomass.C.aov, anova = TRUE)
TukeyHSD(dryRootBiomass.C.aov)
dryRootBiomass.E.aov <- aov(dryRootBiomass ~ CO2 * treatment, data = traits.E)
etaSquared(dryRootBiomass.E.aov, anova = TRUE)


dryShootBiomass.A.aov <- aov(dryShootBiomass ~ CO2 * treatment, data = traits.A)
etaSquared(dryShootBiomass.A.aov, anova = TRUE)
dryShootBiomass.C.aov <- aov(dryShootBiomass ~ CO2 * treatment, data = traits.C)
etaSquared(dryShootBiomass.C.aov, anova = TRUE)
TukeyHSD(dryShootBiomass.C.aov)
dryShootBiomass.E.aov <- aov(dryShootBiomass ~ CO2 * treatment, data = traits.E)
etaSquared(dryShootBiomass.E.aov, anova = TRUE)

RMF.A.aov <- aov(RMF ~ CO2 * treatment, data = traits.A)
etaSquared(RMF.A.aov, anova = TRUE)
TukeyHSD(RMF.A.aov)
RMF.C.aov <- aov(RMF ~ CO2 * treatment, data = traits.C)
etaSquared(RMF.C.aov, anova = TRUE)
RMF.E.aov <- aov(RMF ~ CO2 * treatment, data = traits.E)
etaSquared(RMF.E.aov, anova = TRUE)





# adonis

library(vegan)

traits.A_ <- traits.A[6:12]
traits.A_ <- na.omit(traits.A_)
traits.A.dist <- vegdist(traits.A_)
traits.A.adonis <- adonis(traits.A.dist ~ CO2 * treatment, data = traits.A)
print(traits.A.adonis)

traits.C_ <- traits.C[6:12]
traits.C_ <- na.omit(traits.C_)
traits.C.dist <- vegdist(traits.C_)
traits.C.adonis <- adonis(traits.C.dist ~ CO2 * treatment, data = traits.C)
print(traits.C.adonis)

traits.E_ <- traits.E[6:12]
traits.E_ <- na.omit(traits.E_)
traits.E.dist <- vegdist(traits.E_)
traits.E.adonis <- adonis(traits.E.dist ~ CO2 * treatment, data = traits.E)
print(traits.E.adonis)

# pca 

A.pca <- prcomp(traits.A_, scale.=TRUE, centre = TRUE)
C.pca <- prcomp(traits.C_, scale.=TRUE, centre = TRUE)
E.pca <- prcomp(traits.E_, scale.=TRUE, centre = TRUE)

summary(A.pca)
summary(C.pca)
summary(E.pca)

biplot(A.pca)


# trait correlations

cor(traits.A_)
cor(traits.C_)
cor(traits.E_)

plot(traits.A$rootFineProportion, traits.A$fineRootDMC)
rootsA.A.aov <- aov(fineRootDMC ~ rootFineProportion, data = subset(traits.A, CO2 == "E"))
rootsA.E.aov <- aov(fineRootDMC ~ rootFineProportion, data = subset(traits.A, CO2 == "A"))
abline(rootsA.A.aov)
abline(rootsA.E.aov)
summary(rootsA.A.aov)
summary(rootsA.E.aov)

plot(traits.C$rootFineProportion, traits.C$fineRootDMC)
rootsC.A.aov <- aov(fineRootDMC ~ rootFineProportion, data = subset(traits.C, CO2 == "E"))
rootsC.E.aov <- aov(fineRootDMC ~ rootFineProportion, data = subset(traits.C, CO2 == "A"))
abline(rootsC.A.aov)
abline(rootsC.E.aov)
summary(rootsC.A.aov)
summary(rootsC.E.aov)

plot(traits.E$rootFineProportion, traits.E$fineRootDMC)
rootsE.A.aov <- aov(fineRootDMC ~ rootFineProportion, data = subset(traits.E, CO2 == "E"))
rootsE.E.aov <- aov(fineRootDMC ~ rootFineProportion, data = subset(traits.E, CO2 == "A"))
abline(rootsE.A.aov)
abline(rootsE.E.aov)
summary(rootsE.A.aov)
summary(rootsE.E.aov)







