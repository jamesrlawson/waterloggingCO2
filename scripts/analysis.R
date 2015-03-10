library(nlme)
library(lme4)
library(ez)

## LICOR DATA ##

# means and stdevs etc. # 

# repeated measures ANOVA #

licor <- read.csv("data/licor/licor_doubles.csv", header=TRUE)

licor$species <- as.factor(licor$species)
licor$time <- as.factor(licor$time)
licor$treatment <- as.factor(licor$treatment)
licor$CO2 <- as.factor(licor$CO2)
licor$plant <- as.factor(licor$plant)

# take out flooded treatment (as it causes mismatches)

licor <- subset(licor, treatment != "recovery")

licor.A <- subset(licor, species == "acacia")
licor.C <- subset(licor, species == "cas")
licor.E <- subset(licor, species == "euc")

# simple photosynth anovas

aov.C <- aov(photo ~ treatment * CO2 * time + Error(plant / treatment), data=licor.C)
summary(aov.C)
TukeyHSD(aov.C)    



with(licor.C, pairwise.t.test(treatment, treatment * CO2 * time, p.adjust.method="BH", paired=T))




aov.A <- aov(photo ~ treatment * CO2 * time, data=licor.A)
summary(aov.A)
TukeyHSD(aov.A)  

aov.E <- aov(photo ~ treatment * CO2 * time, data=licor.E)
summary(aov.E)
TukeyHSD(aov.E)  

# simple stomatal conductance anovas

aov.C <- aov(cond ~ treatment * CO2 * time, data=licor.C)
summary(aov.C)
TukeyHSD(aov.C)    

aov.A <- aov(cond ~ treatment * CO2 * time, data=licor.A)
summary(aov.A)
TukeyHSD(aov.A)  

aov.E <- aov(cond ~ treatment * CO2 * time, data=licor.E)
summary(aov.E)
TukeyHSD(aov.E)  



# photosynthesis repeated measures AIC?      

anova_C.photo<-lme(data=licor.C, fixed=photo~treatment*CO2*time, random=~1|plant,
                   na.action=na.omit)

anova_C.photo2<-lme(data=licor.C, fixed=photo~treatment*CO2, random=~1|plant,
                   na.action=na.omit)

anova_C.photo3<-lme(data=licor.C, fixed=photo~treatment*time, random=~1|plant,
                    na.action=na.omit)

anova_C.photo4<-lme(data=licor.C, fixed=photo~time*CO2, random=~1|plant,
                    na.action=na.omit)

anova_C.photo5<-lme(data=licor.C, fixed=photo~time, random=~1|plant,
                    na.action=na.omit)

anova_C.photo6<-lme(data=licor.C, fixed=photo~CO2, random=~1|plant,
                    na.action=na.omit)

anova_C.photo7<-lme(data=licor.C, fixed=photo~treatment, random=~1|plant,
                    na.action=na.omit)

anova_C.photo8<-lme(data=licor.C, fixed=photo~treatment*CO2*time, random=~time|plant,
                   na.action=na.omit)



AICc(anova_C.photo, anova_C.photo2, anova_C.photo3, anova_C.photo4, anova_C.photo5, anova_C.photo6, anova_C.photo7, anova_C.photo8)


anova(anova_C.photo,type="marginal") # computes Type III sums of squares (useful for unbalanced?)


anova_C.photo<-lme(data=licor.C, fixed=photo~treatment*CO2*time, random=~1|plant,
                   na.action=na.omit)

anova(anova_C.photo)
anova(anova_C.photo,type="marginal")

summary(anova_C.photo)

#ez#

licor.A_ez_photo = ezANOVA(
  data = licor.A
  , dv = photo
  , wid = plant
  , within = .(time)
  , between = .(treatment, CO2)
  , detailed = TRUE
  , return_aov = TRUE
  , type = 2
)

licor.C_ez_photo = ezANOVA(
  data = licor.C
  , dv = photo
  , wid = plant
  , within = .(time)
  , between = .(treatment, CO2)
  , detailed = TRUE
  , return_aov = TRUE
  , type = 2
)

licor.E_ez_photo = ezANOVA(
  data = licor.E
  , dv = photo
  , wid = plant
  , within = .(time)
  , between = .(treatment, CO2)
  , detailed = TRUE
  , return_aov = TRUE
  , type = 2
)


licor.A_ez_cond = ezANOVA(
  data = licor.A
  , dv = cond
  , wid = plant
  , within = .(time)
  , between = .(treatment, CO2)
  , detailed = TRUE
  , return_aov = TRUE
  , type = 2
)

licor.C_ez_cond = ezANOVA(
  data = licor.C
  , dv = cond
  , wid = plant
  , within = .(time)
  , between = .(treatment, CO2)
  , detailed = TRUE
  , return_aov = TRUE
  , type = 2
)

licor.E_ez_cond = ezANOVA(
  data = licor.E
  , dv = cond
  , wid = plant
  , within = .(time)
  , between = .(treatment, CO2)
  , detailed = TRUE
  , return_aov = TRUE
  , type = 2
)



licor.A_ez_photo$ANOVA
licor.C_ez_photo$ANOVA
licor.E_ez_photo$ANOVA



#######

licor.A_flooded <- subset(licor.A, time != "mid")
#licor.A_flooded <- subset(licor.A_flooded, time != "mid")
licor.A_flooded <- subset(licor.A_flooded, time != "recovered")
licor.A_flooded <- subset(licor.A_flooded, treatment != "flooded")



licor.A_ez_photo_blah = ezANOVA(
  data = licor.A_flooded
  , dv = photo
  , wid = plant
  , within = .(time)
  , between = .(CO2, treatment)
  , detailed = TRUE
  , return_aov = TRUE
  , type = 2
)

licor.A_ez_photo_blah$ANOVA




licor.E_flooded <- subset(licor.A, time != "recovered")
licor.E_flooded <- subset(licor.A_flooded, time != "mid")
licor.E_flooded <- subset(licor.A_flooded, treatment == "recovery")


licor.E_ez_photo_blah = ezANOVA(
  data = licor.E_flooded
  , dv = photo
  , wid = plant
  , within = .(time)
  , between = .(CO2)
  , detailed = TRUE
  , return_aov = TRUE
  , type = 2
)

licor.A_ez_photo_blah$ANOVA



acacia_control <- subset(acacia_pre, treatment == "control")
acacia_flooded <- subset(acacia_pre, treatment == "flooded")
acacia_recovery <- subset(acacia_pre, treatment == "recovery")

acacia1.t <- kruskal.test(photo ~ CO2, data = acacia_control)
acacia2.t <- kruskal.test(photo ~ CO2, data = acacia_flooded)
acacia3.t <- kruskal.test(photo ~ CO2, data = acacia_recovery)

acacia1.t
acacia2.t
acacia3.t
licor.E_ez_cond$ANOVA


library(schoRsch)
anova_out(licor.E_ez_photo)



summary(glht(licor.E_ez_photo$aov,linfct=mcp(CO2="Tukey")))



# afex # - similar to ez but uses glm
require(afex)

licor.E_melted <- melt(licor.E)
licor.E_photo <- subset(licor.E, variable =="photo")
                               

licor.E_ezglm <- ez.glm(dv = "photo",
                        id = "plant",
                        between = c("treatment", "CO2"),
                        within = "time",
                        data = licor.E)
                        

pairwise.t.test(licor.E$photo,interaction(licor.E$CO2, licor.E$treatment),paired=TRUE,p.adjust.method="BH")


euc_pre <- subset(licor.E, time == "pre")
euc_mid <- subset(licor.E, time == "mid")
euc_end <- subset(licor.E, time == "end")
euc_recovered <- subset(licor.E, time == "recovered")

euc_control <- subset(licor.E, treatment == "control")
euc_flooded <- subset(licor.E, treatment == "flooded")
euc_recovery <- subset(euc_end, treatment == "recovery")

euc1.t<- kruskal.test(photo ~ CO2, data = euc_control)
euc2.t <- kruskal.test(photo ~ CO2, data = euc_flooded)
euc3.t <- kruskal.test(photo ~ CO2, data = euc_recovery)

euc1.t
euc2.t
euc3.t




cas_pre <- subset(licor.C, time == "pre")
cas_mid <- subset(licor.C, time == "mid")
cas_end <- subset(licor.C, time == "end")
cas_recovered <- subset(licor.C, time == "recovered")

cas_control <- subset(cas_recovered, treatment == "control")
cas_flooded <- subset(cas_recovered, treatment == "flooded")
cas_recovery <- subset(cas_recovered, treatment == "recovery")

cas1.t<- kruskal.test(photo ~ CO2, data = cas_control)
cas2.t <- kruskal.test(photo ~ CO2, data = cas_flooded)
cas3.t <- kruskal.test(photo ~ CO2, data = cas_recovery)

cas1.t
cas2.t
cas3.t


acacia_pre <- subset(licor.A, time == "pre")
acacia_mid <- subset(licor.A, time == "mid")
acacia_end <- subset(licor.A, time == "end")
acacia_recovered <- subset(licor.A, time == "recovered")

acacia_control <- subset(acacia_pre, treatment == "control")
acacia_flooded <- subset(acacia_pre, treatment == "flooded")
acacia_recovery <- subset(acacia_pre, treatment == "recovery")

acacia1.t <- kruskal.test(photo ~ CO2, data = acacia_control)
acacia2.t <- kruskal.test(photo ~ CO2, data = acacia_flooded)
acacia3.t <- kruskal.test(photo ~ CO2, data = acacia_recovery)
 
acacia1.t
acacia2.t
acacia3.t















