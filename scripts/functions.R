library(nlme)
library(lme4)
library(ez)
library(plyr)
library(ggplot2)
library(reshape2)

getSummary <- function(df, measurement, species) {
  
  measurement <- deparse(substitute(measurement))
  
  df_melted <- melt(df)
  #df_melted <- na.omit(df_melted)
  df_melted <- subset(df_melted, variable == measurement)
  
  stats <- ddply(df_melted, .(CO2, treatment), summarise, 
                 mean = mean(value),
                 sd = sd(value),
                 sem = sd(value)/sqrt(length(value)))
  stats <- transform(stats, lower=mean-sem, upper=mean+sem)
  
}


getSummary_traits <- function(df, measurement, species) {
  
  measurement <- deparse(substitute(measurement))
  
  df_melted <- melt(df)
  #df_melted <- na.omit(df_melted)
  df_melted <- subset(df_melted, variable == measurement)
  
  stats <- ddply(df_melted, .(CO2, treatment), summarise, 
                 mean = mean(value),
                 sd = sd(value),
                 sem = sd(value)/sqrt(length(value)))
  stats <- transform(stats, lower=mean-sem, upper=mean+sem)
  
}

facetPlot <- function(df, species, measurement) {
  
  measurement <- deparse(substitute(measurement))
  
  figureDir <- "C:/Users/James/Desktop/stuff/glasshouse/glasshouse proj/output/figures/licor"
  species <- deparse(substitute(species))  
  outDir <- sprintf("%s", figureDir)
  dir.create(outDir, recursive=TRUE)
  
  svg(sprintf("%s/%s_%s.svg", outDir, species, measurement), width = 8, height = 6, pointsize = 12)
  
  plot <- ggplot(df, aes(treatment, mean, fill=CO2)) 
  plot <- plot + geom_bar(aes(fill = CO2), stat = "identity", position="dodge") 
  plot <- plot + scale_fill_grey()
#  plot <- plot + ylab(bquote('Photosynthetic rate ('*mu~ 'mol' ~CO[2]~ m^-2~s^-1*')'))
#  plot <- plot + ylab(bquote('Stomatal conductance ('*m~ 'mol' ~H[2]~O~ m^-2~s^-1*')'))
  plot <- plot + ylab("WUE (A / Gs)")

  plot <- plot + geom_errorbar(aes(ymax=upper,
                                   ymin=lower),
                               position=position_dodge(0.9),
                               data=df)    
  #plot <- plot + ggtitle(paste(species, measurement))
  plot <- plot + theme_bw()  
  plot <- plot + theme_set(theme_bw(base_size = 18))
  plot <- plot + theme(axis.title.x=element_blank(),
                       panel.border = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.grid.major = element_blank(),
                       axis.line = element_line(size=.2, color = "black")
                       )

  print(plot)
  
  dev.off()
  
}


plot.means_facet <- function(df, species) {
  
  figureDir <- "C:/Users/James/Desktop/stuff/glasshouse/glasshouse proj/output/figures/traits"
  species <- deparse(substitute(species))
  
  outDir <- figureDir
  
  dir.create(outDir, recursive=TRUE)
  
  df_melted <- melt(df)
  df_melted <- na.omit(df_melted)
  
  stats <- ddply(df_melted, .(CO2, treatment, variable), summarise, 
                 mean = mean(value),
                 sem = sd(value)/sqrt(length(value)))
  stats <- transform(stats, lower=mean-sem, upper=mean+sem)
  
  
  svg(sprintf("%s/%s_traitfacet.svg", outDir, species), width = 8, height = 6, pointsize = 12)
  
  plot <- ggplot(stats, aes(treatment, mean, fill=CO2)) 
  plot <- plot + geom_bar(stat = "identity", position="dodge")    
  plot <- plot + scale_fill_grey()
  plot <- plot + facet_wrap(~ variable, scales = "free") 
  plot <- plot + geom_errorbar(aes(ymax=upper,
                                   ymin=lower),
                               position=position_dodge(0.9),
                               data=stats)    
  plot <- plot + ggtitle(paste(species))  
  plot <- plot + theme_bw()  
  plot <- plot + theme(axis.title.x=element_blank(),
                       panel.border = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.grid.major = element_blank(),
                       axis.line = element_line(size=.2, color = "black")
  )
  
  print(plot)
  
  dev.off()      
  
}


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
    
    svg(sprintf("%s/%s.svg", outDir, trait), width = 8, height = 6, pointsize = 12)
    
    plot <- ggplot(traitStats, aes(treatment, mean, fill=CO2)) 
    plot <- plot + geom_bar(stat = "identity", position="dodge") 
    plot <- plot + scale_fill_grey()
    plot <- plot + geom_errorbar(aes(ymax=upper,
                                     ymin=lower),
                                 position=position_dodge(0.9),
                                 data=traitStats)    
    plot <- plot + ylab(trait)  
    plot <- plot + theme_bw()  
    plot <- plot + theme_set(theme_bw(base_size = 18))
    plot <- plot + theme(axis.title.x=element_blank(),
                         panel.border = element_blank(),
                         panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         axis.line = element_line(size=.2, color = "black")
    )
    
    print(plot)
    
    dev.off()
    
  }
  
}



