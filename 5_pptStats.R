# PPT Stats by county

library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)

ppt <- read.csv("data/PPT_All_1981_20172.csv")%>%
  select(-X)

counties <- st_read("data/countiesusa/cb_2017_us_county_5m.shp")

seCounties <- counties%>%
  filter(STATEFP %in% c(37,45,13))

seCounties$ID <- paste0("F",seCounties$STATEFP,seCounties$COUNTYFP)

pptFlip <- gather(ppt, County, ppt, -Date)

pptFlip <- pptFlip %>% separate(Date, sep="/", into = c("month", "day", "year"))
pptFlip$month <- as.numeric(pptFlip$month)
pptFlip <- filter(pptFlip, month %in% 5:10)

grpCounty <- group_by(pptFlip, County)

#Calculate Growing Season and Standard Deviation Average High Temps
pptaverage <- pptFlip %>%
  group_by(County, year) %>%
  summarize(sumppt = sum(ppt))

pptaverage1 <- pptaverage %>%
  group_by(County) %>%
  summarize(Average = mean(sumppt))

pptaverage2 <- pptaverage %>%
  group_by(County) %>%
  summarize(sd = sd(sumppt))

meanMerge <- merge(pptaverage, pptaverage1, by="County", all=TRUE)
meanMerge <- merge(meanMerge, pptaverage2, by="County", all=TRUE)
meanMerge$Anomaly <- ((meanMerge$sumppt-meanMerge$Average)/meanMerge$sd)

#Create Monthly Subsets

mayppt  <- pptFlip %>%
  filter(month == "5")

junppt<- pptFlip %>%
  filter(month == "6")

julppt <- pptFlip %>%
  filter(month == "7")

augppt <- pptFlip %>%
  filter(month == "8")

sepppt <- pptFlip %>%
  filter(month == "9")

octppt <- pptFlip %>%
  filter(month == "10")

# Calculate Daily Mean by County for entire time period
#meanPPT <- summarize(grpCounty, meanPPT = mean(ppt))
#seCountyppt <- merge(seCounties, meanPPT, by.x = "ID", by.y="County")

#plot <- ggplot(seCountyppt)+
 # geom_sf(aes(fill = meanPPT))

#plot

#st_write(seCountyppt, "~/Documents/Geog_803/Analysis/dailymean.shp")

# Calculate Extreme ppt (top 10%. 5%, 1% of values)
# trace precip is considered .127 mm
mayData <- data.frame()
counties <- seCounties$ID
for(n in 1:length(counties)){
  county <- mayppt%>%
    filter(County == counties[n] & ppt > 0.127)
  quant90 <- quantile(county$ppt, .90)
  quant95 <- quantile(county$ppt, .95)
  quant99 <- quantile(county$ppt, .99)
  newData <- data.frame("County" = county$County[1], "Quant90" = quant90, "Quant95" = quant95,"Quant99" = quant99)
  mayData <- rbind(mayData, newData)
}

junData <- data.frame()
counties <- seCounties$ID
for(n in 1:length(counties)){
  county <- junppt%>%
    filter(County == counties[n] & ppt > 0.127)
  quant90 <- quantile(county$ppt, .90)
  quant95 <- quantile(county$ppt, .95)
  quant99 <- quantile(county$ppt, .99)
  newData <- data.frame("County" = county$County[1], "Quant90" = quant90, "Quant95" = quant95,"Quant99" = quant99)
  junData <- rbind(junData, newData)
}

julData <- data.frame()
counties <- seCounties$ID
for(n in 1:length(counties)){
  county <- julppt%>%
    filter(County == counties[n] & ppt > 0.127)
  quant90 <- quantile(county$ppt, .90)
  quant95 <- quantile(county$ppt, .95)
  quant99 <- quantile(county$ppt, .99)
  newData <- data.frame("County" = county$County[1], "Quant90" = quant90, "Quant95" = quant95,"Quant99" = quant99)
  julData <- rbind(julData, newData)
}

augData <- data.frame()
counties <- seCounties$ID
for(n in 1:length(counties)){
  county <- augppt%>%
    filter(County == counties[n] & ppt > 0.127)
  quant90 <- quantile(county$ppt, .90)
  quant95 <- quantile(county$ppt, .95)
  quant99 <- quantile(county$ppt, .99)
  newData <- data.frame("County" = county$County[1], "Quant90" = quant90, "Quant95" = quant95,"Quant99" = quant99)
  augData <- rbind(augData, newData)
}

sepData <- data.frame()
counties <- seCounties$ID
for(n in 1:length(counties)){
  county <- sepppt%>%
    filter(County == counties[n] & ppt > 0.127)
  quant90 <- quantile(county$ppt, .90)
  quant95 <- quantile(county$ppt, .95)
  quant99 <- quantile(county$ppt, .99)
  newData <- data.frame("County" = county$County[1], "Quant90" = quant90, "Quant95" = quant95,"Quant99" = quant99)
  sepData <- rbind(sepData, newData)
}

octData <- data.frame()
counties <- seCounties$ID
for(n in 1:length(counties)){
  county <- octppt%>%
    filter(County == counties[n] & ppt > 0.127)
  quant90 <- quantile(county$ppt, .90)
  quant95 <- quantile(county$ppt, .95)
  quant99 <- quantile(county$ppt, .99)
  newData <- data.frame("County" = county$County[1], "Quant90" = quant90, "Quant95" = quant95,"Quant99" = quant99)
  octData <- rbind(octData, newData)
}

write.csv(mayData, "data/mayppt_Extremes.csv")
write.csv(junData, "data/junppt_Extremes.csv")
write.csv(julData, "data/julppt_Extremes.csv")
write.csv(augData, "data/augppt_Extremes.csv")
write.csv(sepData, "data/sepppt_Extremes.csv")
write.csv(octData, "data/octppt_Extremes.csv")


# Count number of days by year that ppt extremes were exceeded

mayMerge <- merge(mayppt, mayData, by="County")
junMerge <- merge(junppt, junData, by="County")
julMerge <- merge(julppt, julData, by="County")
augMerge <- merge(augppt, augData, by="County")
sepMerge <- merge(sepppt, sepData, by="County")
octMerge <- merge(octppt, octData, by="County")

maydf <- data.frame()
for(n in 1981:2017){
  data <- mayMerge%>%
    filter(year == n)
  days90 <- group_by(data, County)%>%
    tally(ppt>Quant90)
  days95 <- group_by(data, County)%>%
    tally(ppt>Quant95)
  days99 <- group_by(data, County)%>%
    tally(ppt>Quant99)
  output <- data.frame("Year" = data$year[1], "extpptDays90" = days90, "extpptDays95" = days95,"extpptDays99" = days99)
  maydf <- rbind(maydf, output)
  
}


jundf <- data.frame()
for(n in 1981:2017){
  data <- junMerge%>%
    filter(year == n)
  days90 <- group_by(data, County)%>%
    tally(ppt>Quant90)
  days95 <- group_by(data, County)%>%
    tally(ppt>Quant95)
  days99 <- group_by(data, County)%>%
    tally(ppt>Quant99)
  output <- data.frame("Year" = data$year[1], "extpptDays90" = days90, "extpptDays95" = days95,"extpptDays99" = days99)
  jundf <- rbind(jundf, output)
  
}

juldf <- data.frame()
for(n in 1981:2017){
  data <- julMerge%>%
    filter(year == n)
  days90 <- group_by(data, County)%>%
    tally(ppt>Quant90)
  days95 <- group_by(data, County)%>%
    tally(ppt>Quant95)
  days99 <- group_by(data, County)%>%
    tally(ppt>Quant99)
  output <- data.frame("Year" = data$year[1], "extpptDays90" = days90, "extpptDays95" = days95,"extpptDays99" = days99)
  juldf <- rbind(juldf, output)
  
}

augdf <- data.frame()
for(n in 1981:2017){
  data <- augMerge%>%
    filter(year == n)
  days90 <- group_by(data, County)%>%
    tally(ppt>Quant90)
  days95 <- group_by(data, County)%>%
    tally(ppt>Quant95)
  days99 <- group_by(data, County)%>%
    tally(ppt>Quant99)
  output <- data.frame("Year" = data$year[1], "extpptDays90" = days90, "extpptDays95" = days95,"extpptDays99" = days99)
  augdf <- rbind(augdf, output)
  
}

sepdf <- data.frame()
for(n in 1981:2017){
  data <- sepMerge%>%
    filter(year == n)
  days90 <- group_by(data, County)%>%
    tally(ppt>Quant90)
  days95 <- group_by(data, County)%>%
    tally(ppt>Quant95)
  days99 <- group_by(data, County)%>%
    tally(ppt>Quant99)
  output <- data.frame("Year" = data$year[1], "extpptDays90" = days90, "extpptDays95" = days95,"extpptDays99" = days99)
  sepdf <- rbind(sepdf, output)
  
}

octdf <- data.frame()
for(n in 1981:2017){
  data <- octMerge%>%
    filter(year == n)
  days90 <- group_by(data, County)%>%
    tally(ppt>Quant90)
  days95 <- group_by(data, County)%>%
    tally(ppt>Quant95)
  days99 <- group_by(data, County)%>%
    tally(ppt>Quant99)
  output <- data.frame("Year" = data$year[1], "extpptDays90" = days90, "extpptDays95" = days95,"extpptDays99" = days99)
  octdf <- rbind(octdf, output)
  
}

colnames(maydf) <- c("Year","County","NumpptDays90", "County1", "NumpptDays95", "County2", "NumpptDays99")
may <- merge(maydf,meanMerge, by.x=c("County", "Year"), by.y=c("County", "year"), all=TRUE)

colnames(jundf) <- c("Year","County","NumpptDays90", "County1", "NumpptDays95", "County2", "NumpptDays99")
jun <- merge(jundf,meanMerge, by.x=c("County", "Year"), by.y=c("County", "year"), all=TRUE)

colnames(juldf) <- c("Year","County","NumpptDays90", "County1", "NumpptDays95", "County2", "NumpptDays99")
jul <- merge(juldf,meanMerge, by.x=c("County", "Year"), by.y=c("County", "year"), all=TRUE)

colnames(augdf) <- c("Year","County","NumpptDays90", "County1", "NumpptDays95", "County2", "NumpptDays99")
aug <- merge(augdf,meanMerge, by.x=c("County", "Year"), by.y=c("County", "year"), all=TRUE)

colnames(sepdf) <- c("Year","County","NumpptDays90", "County1", "NumpptDays95", "County2", "NumpptDays99")
sep <- merge(sepdf,meanMerge, by.x=c("County", "Year"), by.y=c("County", "year"), all=TRUE)

colnames(octdf) <- c("Year","County","NumpptDays90", "County1", "NumpptDays95", "County2", "NumpptDays99")
oct <- merge(octdf,meanMerge, by.x=c("County", "Year"), by.y=c("County", "year"), all=TRUE)

write.csv(may, "data/Num_mayppt_Extreme_Days.csv")
write.csv(jun, "data/Num_junppt_Extreme_Days.csv")
write.csv(jul, "data/Num_julppt_Extreme_Days.csv")
write.csv(aug, "data/Num_augppt_Extreme_Days.csv")
write.csv(sep, "data/Num_sepppt_Extreme_Days.csv")
write.csv(oct, "data/Num_octppt_Extreme_Days.csv")

# Do we need this?
#dfSpread <- spread(outdf, County, NumDays)
#write.csv(dfSpread, "~/Documents/Geog_803/Analysis/Num_ppt_Extreme_Days_Wide.csv")
