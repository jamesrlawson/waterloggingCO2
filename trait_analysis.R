# trait MEANS #

library(plyr)
library(ggplot2)
library(reshape2)

traits <- read.csv("data/harvest/traits.csv", header=T)
#traits <- subset(traits, treatment != "flooded")

# subset by species

traits.A <- subset(traits, species == "acacia")
traits.C <- subset(traits, species == "cas")
traits.E <- subset(traits, species == "euc")

# make facet plots for all traits

plot.means_facet <- function(df, species) {
  
  figureDir <- "C:/Users/James/Desktop/stuff/glasshouse/glasshouse proj/output/figures"
  species <- deparse(substitute(species))
  
  outDir <- sprintf("%s/%s", figureDir, species)
  
  dir.create(outDir, recursive=TRUE)
  
  
  df_melted <- melt(df)
  df_melted <- na.omit(df_melted)
  
  stats <- ddply(df_melted, .(CO2, treatment, variable), summarise, 
                 mean = mean(value),
                 sem = sd(value)/sqrt(length(value)))
  stats <- transform(stats, lower=mean-sem, upper=mean+sem)
  
  
  png(sprintf("%s/%s_traitfacet.png", outDir, species), width = 1500, height = 900)
  
  plot <- ggplot(stats, aes(treatment, mean, fill=CO2)) 
  plot <- plot + geom_bar(stat = "identity", position="dodge")    
  plot <- plot + facet_wrap(~ variable, scales = "free") 
  plot <- plot + geom_errorbar(aes(ymax=upper,
                                   ymin=lower),
                               position=position_dodge(0.9),
                               data=stats)    
  plot <- plot + ggtitle(paste(species))  
  
  print(plot)
  
  dev.off()      
  
}

plot.means_facet(traits.A, acacia)
plot.means_facet(traits.C, casuarina)
plot.means_facet(traits.E, eucalyptus)



## individual traits ##

cas_control <- subset(traits.C, treatment == "control")
cas_flooded <- subset(traits.C, treatment == "flooded")
cas_recovery <- subset(traits.C, treatment == "recovery")

cas1.t<- kruskal.test(dryShootBiomass ~ CO2, data = cas_control)
cas2.t <- kruskal.test(dryShootBiomass ~ CO2, data = cas_flooded)
cas3.t <- kruskal.test(dryShootBiomass ~ CO2, data = cas_recovery)

cas1.t
cas2.t
cas3.t


blah <- aov(LDMC ~ CO2 * treatment, data = traits.C)
summary(blah)



TukeyHSD(blah)

