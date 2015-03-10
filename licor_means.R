# licor MEANS #

library(plyr)
library(ggplot2)
library(reshape2)

licor <- read.csv("data/licor/licor_A33 removed.csv", header=TRUE)
PAM <- read.csv("data/PAM/PAM.csv", header=TRUE)

licor$species <- as.factor(licor$species)
licor$time = factor(licor$time, unique(licor$time)) # keeps factor in original order
licor$CO2 <- as.factor(licor$CO2)
licor$plant <- as.factor(licor$plant)

licor.A <- subset(licor, species == "acacia")
licor.C <- subset(licor, species == "cas")
licor.E <- subset(licor, species == "euc")

#PAM$species <- as.factor(PAM$species)
#PAM$time = factor(PAM$time, unique(PAM$time)) # keeps factor in original order
##PAM$CO2 <- as.factor(PAM$CO2)
#PAM$plant <- as.factor(PAM$plant)

PAM.A <- subset(PAM, species == "acacia")
PAM.C <- subset(PAM, species == "cas")
PAM.E <- subset(PAM, species == "euc")

PAM.A <- na.omit(PAM.A)
PAM.C <- na.omit(PAM.C)
PAM.E <- na.omit(PAM.E)



getStats <- function(df, measurement, species) {
  
  measurement <- deparse(substitute(measurement))
  
  df_melted <- melt(df)
  #df_melted <- na.omit(df_melted)
  df_melted <- subset(df_melted, variable == measurement)
  
  stats <- ddply(df_melted, .(CO2, treatment, time), summarise, 
                 mean = mean(value),
                 sem = sd(value)/sqrt(length(value)))
  stats <- transform(stats, lower=mean-sem, upper=mean+sem)
  
}
  
licor.A_photo <- getStats(licor.A, photo, acacia)
licor.A_cond <- getStats(licor.A, cond, acacia)

licor.C_photo <- getStats(licor.C, photo, acacia)
licor.C_cond <- getStats(licor.C, cond, acacia)

licor.E_photo <- getStats(licor.E, photo, acacia)
licor.E_cond <- getStats(licor.E, cond, acacia)



PAM.A_stats <- ddply(PAM.A, .(treatment, CO2, time), summarise, 
               mean = mean(PAM),
               sem = sd(PAM)/sqrt(length(PAM)))
PAM.A_stats <- transform(PAM.A_stats, lower=mean-sem, upper=mean+sem)


PAM.C_stats <- ddply(PAM.C, .(treatment, CO2, time), summarise, 
                     mean = mean(PAM),
                     sem = sd(PAM)/sqrt(length(PAM)))
PAM.C_stats <- transform(PAM.C_stats, lower=mean-sem, upper=mean+sem)


PAM.E_stats <- ddply(PAM.E, .(treatment, CO2, time), summarise, 
                     mean = mean(PAM),
                     sem = sd(PAM)/sqrt(length(PAM)))
PAM.E_stats <- transform(PAM.E_stats, lower=mean-sem, upper=mean+sem)


facetPlot <- function(df, species, measurement) {
 
  measurement <- deparse(substitute(measurement))
  
  figureDir <- "C:/Users/James/Desktop/stuff/glasshouse/glasshouse proj/output/figures/licor"
  species <- deparse(substitute(species))  
  outDir <- sprintf("%s", figureDir)
  dir.create(outDir, recursive=TRUE)
  
  png(sprintf("%s/%s_%s.png", outDir, species, measurement), width = 800, height = 600)

      plot <- ggplot(df, aes(treatment, mean, fill=CO2)) 
      plot <- plot + geom_bar(stat = "identity", position="dodge") 
      plot <- plot + facet_wrap(~ time)
      
      plot <- plot + geom_errorbar(aes(ymax=upper,
                                       ymin=lower),
                                   position=position_dodge(0.9),
                                   data=df)    
      plot <- plot + ggtitle(paste(species, measurement))
    
    print(plot)
  
  dev.off()

}

facetPlot(licor.A_photo, acacia, photosyntheticRate1)
facetPlot(licor.C_photo, casuarina, photosyntheticRate1)
facetPlot(licor.E_photo, eucalyptus, photosyntheticRate1)

facetPlot(licor.A_cond, acacia, stomatalConductance1)
facetPlot(licor.C_cond, casuarina, stomatalConductance1)
facetPlot(licor.E_cond, eucalyptus, stomatalConductance1)


facetPlot(PAM.A_stats, acacia, PAM)
facetPlot(PAM.C_stats, casuarina)
facetPlot(PAM.E_stats, eucalyptus, PAM)















# take out flooded treatment (as it causes mismatches)

#licor <- subset(licor, treatment != "flooded")

licor.A <- subset(licor, species == "acacia")
licor.C <- subset(licor, species == "cas")
licor.E <- subset(licor, species == "euc")

# means over multiple treatments

means.photo.A <- ddply(licor.A, .(treatment, CO2, time), summarise, means = mean(photo), sd = sd(photo))
means.photo.C <- ddply(licor.C, .(treatment, CO2, time), summarise, means = mean(photo), sd = sd(photo))
means.photo.E <- ddply(licor.E, .(treatment, CO2, time), summarise, means = mean(photo), sd = sd(photo))

means.cond.A <- ddply(licor.A, .(treatment, CO2, time), summarise, means = mean(cond), sd = sd(cond))
means.cond.C <- ddply(licor.C, .(treatment, CO2, time), summarise, means = mean(cond), sd = sd(cond))
means.cond.E <- ddply(licor.E, .(treatment, CO2, time), summarise, means = mean(cond), sd = sd(cond))

# time means

plot(means.photo.A$time, means.photo.A$means)
plot(means.photo.C$time, means.photo.C$means)
plot(means.photo.E$time, means.photo.E$means)

plot(means.cond.A$time, means.cond.A$means)
plot(means.cond.C$time, means.cond.C$means)
plot(means.cond.E$time, means.cond.E$means)

# flooding treatment means

plot(means.photo.A$treatment, means.photo.A$means)
plot(means.photo.C$treatment, means.photo.C$means)
plot(means.photo.E$treatment, means.photo.E$means)

plot(means.cond.A$treatment, means.cond.A$means)
plot(means.cond.C$treatment, means.cond.C$means)
plot(means.cond.E$treatment, means.cond.E$means)

# CO2 means

plot(means.photo.A$CO2, means.photo.A$means)
plot(means.photo.C$CO2, means.photo.C$means)
plot(means.photo.E$CO2, means.photo.E$means)

plot(means.cond.A$CO2, means.cond.A$means)
plot(means.cond.C$CO2, means.cond.C$means)
plot(means.cond.E$CO2, means.cond.E$means)


# boxplots #

#cond

licor.A$f1f2f3 <- interaction(licor.A$CO2, licor.A$treatment, licor.A$time)
ggplot(aes(y = cond, x = f1f2f3), data = licor.A) + geom_boxplot(size=0.01)

licor.C$f1f2f3 <- interaction(licor.C$CO2, licor.C$treatment, licor.C$time)
ggplot(aes(y = cond, x = f1f2f3), data = licor.C) + geom_boxplot()

licor.E$f1f2f3 <- interaction(licor.E$CO2, licor.E$treatment, licor.E$time)
ggplot(aes(y = cond, x = f1f2f3), data = licor.E) + geom_boxplot()


# photo #

licor.A$f1f2f3 <- interaction(licor.A$CO2, licor.A$treatment, licor.A$time)
ggplot(aes(y = photo, x = f1f2f3), data = licor.A) + geom_boxplot()

licor.C$f1f2f3 <- interaction(licor.C$CO2, licor.C$treatment, licor.C$time)
ggplot(aes(y = photo, x = f1f2f3), data = licor.C) + geom_boxplot()

licor.E$f1f2f3 <- interaction(licor.E$CO2, licor.E$treatment, licor.E$time)
ggplot(aes(y = photo, x = f1f2f3), data = licor.E) + geom_boxplot()





blah <- pairwise.t.test(licor.C$cond,licor.C$f1f2f3,paired=TRUE,p.adjust.method="BH")




with(licor.A, boxplot(photo ~ CO2 * treatment * time))








means.cond.A_eCO2 <- subset(means.cond.A, CO2 == "E")
means.cond.A_aCO2 <- subset(means.cond.A, CO2 == "A")

plot(means.cond.A_eCO2$time, means.cond.A_eCO2$means)
plot(means.cond.A_aCO2$time, means.cond.A_aCO2$means)



licor.A_control <- subset(licor.A, treatment == "control")
licor.A_flooded <- subset(licor.A, treatment == "flooded")
licor.A_recovery <- subset(licor.A, treatment == "recovery")


plot(licor.A_control$time, licor.A_control$photo)
plot(licor.A_flooded$time, licor.A_control$photo)
plot(licor.A_recovery$time, licor.A_control$photo)







