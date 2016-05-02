require(dplyr)
require(plyr)

load_data <- function(location) { 
  filenames = paste(location, "/", list.files(path = location, pattern = "*.csv", recursive=TRUE), sep = "")
  tables <- lapply(filenames, read.table, header = T, skip = 6, sep=",", stringsAsFactors = FALSE)
  tables <- lapply(tables, "[", c(1:4))
  
  out <- do.call(rbind, tables)
  out[,1] <- as.Date(out[,1])
  out[,c(2:4)] <- lapply(out[,c(2:4)], as.numeric)
  out$month <- months(out[,1])
  
  return(out)
}

temp_GH2 <- load_data("data/GH_logs/GH2")
temp_GH3 <- load_data("data/GH_logs/GH3")
temp_GH4 <- load_data("data/GH_logs/GH4")

temp_GH2 <- cbind(temp_GH2$month, select(temp_GH2, contains("C")))
temp_GH2 <- cbind(temp_GH2$month, select(temp_GH3, contains("C")))
temp_GH2 <- cbind(temp_GH2$month, select(temp_GH4, contains("C")))

colnames(temp_GH2) <- c("temp", "month")
colnames(temp_GH3) <- c("temp", "month")
colnames(temp_GH4) <- c("temp", "month")

temp_GH2 <- temp_GH2 %>% group_by(month) %>% summarise(., 
                                                   mean = mean(temp, na.rm=TRUE),
                                                   min = min(temp, na.rm=TRUE),
                                                   max = max(temp, na.rm=TRUE))
temp_GH3 <- temp_GH3 %>% group_by(month) %>% summarise(., 
                                                       mean = mean(temp, na.rm=TRUE),
                                                       min = min(temp, na.rm=TRUE),
                                                       max = max(temp, na.rm=TRUE))
temp_GH4 <- temp_GH4 %>% group_by(month) %>% summarise(., 
                                                       mean = mean(temp, na.rm=TRUE),
                                                       min = min(temp, na.rm=TRUE),
                                                       max = max(temp, na.rm=TRUE))
temp_GH2$GH <- 2
temp_GH3$GH <- 3
temp_GH4$GH <- 4

temp_GH <- rbind(temp_GH2, temp_GH3, temp_GH4)
temp_GH <- temp_GH[!temp_GH$month %in% c("April", "May"),]

tapply(temp_GH$mean, temp_GH$GH, mean)
tapply(temp_GH$mean, temp_GH$GH, min)
tapply(temp_GH$mean, temp_GH$GH, max)

summary(aov(mean ~ GH, data = temp_GH))
summary(aov(min ~ GH, data = temp_GH))
summary(aov(max ~ GH, data = temp_GH))

