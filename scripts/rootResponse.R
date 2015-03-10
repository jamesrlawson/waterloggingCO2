traits <- read.csv("data/harvest/traits1.csv", header=T)

traits <- data.frame(cbind(
  traits["species"],
  traits["plant"],
  traits["CO2"],
  traits["treatment"],
  sqrt(traits["dryRootBiomass"]), 
  sqrt(traits["rootDensity"]),
  sqrt(traits["dryShootBiomass"]),
  log10(traits["RMF"]),
  log10(traits["SLA"]), 
  traits["LDMC"],
  traits["dryBiomass"],
  traits["rootDensity"]))

traits$dryBiomass <- sqrt(traits$dryRootBiomass + traits$dryShootBiomass)

licor <- read.csv("data/licor/licor_all.csv", header=TRUE)
traits <- merge(licor, traits)

#traits <- subset(traits, treatment != "flooded")

traits.A <- subset(traits, species == "acacia")
traits.C <- subset(traits, species == "cas")
traits.E <- subset(traits, species == "euc")


## EUCAYPTUS ##

traits.E__ <- traits.E
#traits.E__ <- subset(traits.E, dryBiomass < 0.38) #remove outlier
#traits.E__ <- subset(traits.E__, plant != "E1") #remove outlier

plot(traits.E__$rootDensity, traits.E__$dryBiomass, col = traits.E__$treatment, pch = as.numeric(traits.E__$CO2))
rootsE.control.lm <- lm(dryBiomass ~ rootDensity, data = subset(traits.E__, treatment == "control"))
rootsE.recovery.lm <- lm(dryBiomass ~ rootDensity, data = subset(traits.E__, treatment == "recovery"))
rootsE.flooded.lm <- lm(dryBiomass ~ rootDensity, data = subset(traits.E__, treatment == "flooded"))

abline(rootsE.control.lm, col = c("black"))
abline(rootsE.recovery.lm, col = c("green"))
abline(rootsE.flooded.lm, col = c("red"))
summary(rootsE.control.lm)
summary(rootsE.recovery.lm)
summary(rootsE.flooded.lm)

plot(traits.E__$rootDensity, traits.E__$dryBiomass, col = traits.E__$treatment, pch = as.numeric(traits.E__$CO2))

rootsE.E.lm <- lm(dryBiomass ~ rootDensity, data = subset(traits.E__, CO2 == "E"))
rootsE.A.lm <- lm(dryBiomass ~ rootDensity, data = subset(traits.E__, CO2 == "A"))

abline(rootsE.E.lm, col = c("black"))
abline(rootsE.A.lm, col = c("green"))
summary(rootsE.E.lm)
summary(rootsE.A.lm)
Anova(rootsE.A.lm, rootsE.E.lm)


rootsE0.lm <- lm(dryBiomass ~ rootDensity, data = traits.E__)
rootsE1.lm <- lm(dryBiomass ~ rootDensity * treatment, data = traits.E__)
rootsE2.lm <- lm(dryBiomass ~ rootDensity * CO2, data = traits.E__)
rootsE3.lm <- lm(dryBiomass ~ rootDensity * treatment * CO2, data = traits.E__)
require(MuMIn)

AICc(rootsE0.lm, rootsE1.lm, rootsE2.lm, rootsE3.lm) # significant treatment interaction

anova(rootsE1.lm)
anova(rootsE3.lm)




### ACACIA ###

traits.A__ <- traits.A
#traits.A__ <- subset(traits.A, dryBiomass < 0.3)
#traits.A__ <- traits.A__[-25,]
                     
plot(traits.A__$rootDensity, traits.A__$dryBiomass, col = traits.A__$treatment, pch = as.numeric(traits.A__$CO2))
rootsA.control.lm <- lm(dryBiomass ~ rootDensity, data = subset(traits.A__, treatment == "control"))
rootsA.recovery.lm <- lm(dryBiomass ~ rootDensity, data = subset(traits.A__, treatment == "recovery"))
rootsA.flooded.lm <- lm(dryBiomass ~ rootDensity, data = subset(traits.A__, treatment == "flooded"))

abline(rootsA.control.lm, col = c("black"))
abline(rootsA.recovery.lm, col = c("green"))
abline(rootsA.flooded.lm, col = c("red"))
summary(rootsA.control.lm)
summary(rootsA.recovery.lm)
summary(rootsA.flooded.lm)

rootsA.E.lm <- lm(dryBiomass ~ rootDensity, data = subset(traits.A__, CO2 == "E"))
rootsA.A.lm <- lm(dryBiomass ~ rootDensity, data = subset(traits.A__, CO2 == "A"))

plot(traits.A__$rootDensity, traits.A__$dryBiomass, col = traits.A__$CO2, pch = as.numeric(traits.A__$CO2))

abline(rootsA.E.lm, col = c("red"))
abline(rootsA.A.lm, col = c("black"))
summary(rootsA.E.lm)
summary(rootsA.A.lm)


rootsA0.lm <- lm(dryBiomass ~ rootDensity, data = traits.A__)
rootsA1.lm <- lm(dryBiomass ~ rootDensity * treatment, data = traits.A__)
rootsA2.lm <- lm(dryBiomass ~ rootDensity * CO2, data = traits.A__)
rootsA3.lm <- lm(dryBiomass ~ rootDensity * treatment * CO2, data = traits.A__)
require(MuMIn)

AICc(rootsA0.lm, rootsA1.lm, rootsA2.lm, rootsA3.lm) # rootsA2 wins!

anova(rootsA3.lm) #  significant treatment and highly significant CO2 interactions
anova(rootsA2.lm) # 
anova(rootsA1.lm) #

## CASUARINA ##

traits.C__ <- traits.C
#traits.C__ <- subset(traits.C, dryBiomass < 0.31)

plot(traits.C__$rootDensity, traits.C__$dryBiomass, col = traits.C__$treatment, pch = as.numeric(traits.C__$CO2))
rootsC.control.lm <- lm(dryBiomass ~ rootDensity, data = subset(traits.C__, treatment == "control"))
rootsC.recovery.lm <- lm(dryBiomass ~ rootDensity, data = subset(traits.C__, treatment == "recovery"))
rootsC.flooded.lm <- lm(dryBiomass ~ rootDensity, data = subset(traits.C__, treatment == "flooded"))

abline(rootsC.control.lm, col = c("black"))
abline(rootsC.recovery.lm, col = c("green"))
abline(rootsC.flooded.lm, col = c("red"))
summary(rootsC.control.lm)
summary(rootsC.recovery.lm)
summary(rootsC.flooded.lm)


rootsC.E.lm <- lm(dryBiomass ~ rootDensity, data = subset(traits.C__, CO2 == "E"))
rootsC.A.lm <- lm(dryBiomass ~ rootDensity, data = subset(traits.C__, CO2 == "A"))

plot(traits.C__$rootDensity, traits.C__$dryBiomass, col = traits.C__$CO2, pch = as.numeric(traits.C__$CO2))

abline(rootsC.E.lm, col = c("red"))
abline(rootsC.A.lm, col = c("black"))
summary(rootsC.E.lm)
summary(rootsC.A.lm)


rootsC0.lm <- lm(dryBiomass ~ rootDensity, data = traits.C__)
rootsC1.lm <- lm(dryBiomass ~ rootDensity * treatment, data = traits.C__)
rootsC2.lm <- lm(dryBiomass ~ rootDensity * CO2, data = traits.C__)
rootsC3.lm <- lm(dryBiomass ~ rootDensity * treatment * CO2, data = traits.C__)
require(MuMIn)

AICc(rootsC0.lm, rootsC1.lm, rootsC2.lm, rootsC3.lm)

anova(rootsC1.lm)
anova(rootsC2.lm) 
anova(rootsC3.lm) # significant treatment and Co2 effects, and significant treatment interaction, 
                  # almost significant three-way interaction!



