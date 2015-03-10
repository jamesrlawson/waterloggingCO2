euc_licor <- read.csv("data/licor/licor_preflood.csv", header=TRUE)

euc_licor$GH <- as.factor(euc_licor$GH)
euc_licor$CO2 <- as.factor(euc_licor$CO2)

# photosynthesis

photo.GH.aov <- aov(Photo ~ GH, data = euc_licor)
summary(photo.GH.aov)
TukeyHSD(photo.GH.aov)

photo.CO2.aov <- aov(Photo ~ CO2, data = euc_licor)
summary(photo.CO2.aov)

# stomatal conductance

Cond.GH.aov <- aov(Cond ~ GH, data = euc_licor)
summary(Cond.GH.aov)
TukeyHSD(Cond.GH.aov)

Cond.CO2.aov <- aov(Cond ~ CO2, data = euc_licor)
summary(Cond.CO2.aov)
