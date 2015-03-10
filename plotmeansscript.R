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


plot.means <- function(df, species) {
  
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
  
  
  for(i in 1:nrow(stats)) {      
    
    trait <- (stats[i,3])
    traitName <- deparse(substitute(trait))
    
    traitStats <- subset(stats, variable == trait)
    traitStats <- traitStats[-3]
    
    png(sprintf("%s/%s.png", outDir, trait), width = 800, height = 600)
    
    plot <- ggplot(traitStats, aes(treatment, mean, fill=CO2)) 
    plot <- plot + geom_bar(stat = "identity", position="dodge") 
    
    plot <- plot + geom_errorbar(aes(ymax=upper,
                      ymin=lower),
                  position=position_dodge(0.9),
                  data=traitStats)    
    plot <- plot + ylab(trait)  
    
    print(plot)
    
    dev.off()

  }

}
    
plot.means(traits.A, acacia)
plot.means(traits.C, casuarina)
plot.means(traits.E, eucalyptus)








