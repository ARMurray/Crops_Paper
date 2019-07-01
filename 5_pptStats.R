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
colnames(meanMerge) = c("County", "year", "GrowsingSeasonSumppt", "GrowingSeasonAvgppt", "GrowingSeasonpptsd", "GrowingSeasonAnomaly")


#Create Monthly Subsets

mayppt  <- pptFlip %>%
  filter(month == "5")

pptaveragemay <- mayppt %>%
  group_by(County, year) %>%
  summarize(sumppt = sum(ppt))

pptaverage1may <- pptaveragemay %>%
  group_by(County) %>%
  summarize(Average = mean(sumppt))

pptaverage2may <- pptaveragemay %>%
  group_by(County) %>%
  summarize(sd = sd(sumppt))

meanMergemay <- merge(pptaveragemay, pptaverage1may, by="County", all=TRUE)
meanMergemay <- merge(meanMergemay, pptaverage2may, by="County", all=TRUE)
meanMergemay$Anomaly <- ((meanMergemay$sumppt-meanMergemay$Average)/meanMergemay$sd)
colnames(meanMergemay) = c("County", "year", "maySumppt", "mayAvgppt", "maypptsd", "maypptAnomaly")



junppt<- pptFlip %>%
  filter(month == "6")

pptaveragejun <- junppt %>%
  group_by(County, year) %>%
  summarize(sumppt = sum(ppt))

pptaverage1jun <- pptaveragejun %>%
  group_by(County) %>%
  summarize(Average = mean(sumppt))

pptaverage2jun <- pptaveragejun %>%
  group_by(County) %>%
  summarize(sd = sd(sumppt))

meanMergejun <- merge(pptaveragejun, pptaverage1jun, by="County", all=TRUE)
meanMergejun <- merge(meanMergejun, pptaverage2jun, by="County", all=TRUE)
meanMergejun$Anomaly <- ((meanMergejun$sumppt-meanMergejun$Average)/meanMergejun$sd)
colnames(meanMergejun) = c("County", "year", "junSumppt", "junAvgppt", "junpptsd", "junpptAnomaly")



julppt <- pptFlip %>%
  filter(month == "7")

pptaveragejul <- julppt %>%
  group_by(County, year) %>%
  summarize(sumppt = sum(ppt))

pptaverage1jul <- pptaveragejul %>%
  group_by(County) %>%
  summarize(Average = mean(sumppt))

pptaverage2jul <- pptaveragejul %>%
  group_by(County) %>%
  summarize(sd = sd(sumppt))

meanMergejul <- merge(pptaveragejul, pptaverage1jul, by="County", all=TRUE)
meanMergejul <- merge(meanMergejul, pptaverage2jul, by="County", all=TRUE)
meanMergejul$Anomaly <- ((meanMergejul$sumppt-meanMergejul$Average)/meanMergejul$sd)
colnames(meanMergejul) = c("County", "year", "julSumppt", "julAvgppt", "julpptsd", "julpptAnomaly")



augppt <- pptFlip %>%
  filter(month == "8")

pptaverageaug <- augppt %>%
  group_by(County, year) %>%
  summarize(sumppt = sum(ppt))

pptaverage1aug <- pptaverageaug %>%
  group_by(County) %>%
  summarize(Average = mean(sumppt))

pptaverage2aug <- pptaverageaug %>%
  group_by(County) %>%
  summarize(sd = sd(sumppt))

meanMergeaug <- merge(pptaverageaug, pptaverage1aug, by="County", all=TRUE)
meanMergeaug <- merge(meanMergeaug, pptaverage2aug, by="County", all=TRUE)
meanMergeaug$Anomaly <- ((meanMergeaug$sumppt-meanMergeaug$Average)/meanMergeaug$sd)
colnames(meanMergeaug) = c("County", "year", "augSumppt", "augAvgppt", "augpptsd", "augpptAnomaly")



sepppt <- pptFlip %>%
  filter(month == "9")

pptaveragesep <- sepppt %>%
  group_by(County, year) %>%
  summarize(sumppt = sum(ppt))

pptaverage1sep <- pptaveragesep %>%
  group_by(County) %>%
  summarize(Average = mean(sumppt))

pptaverage2sep <- pptaveragesep %>%
  group_by(County) %>%
  summarize(sd = sd(sumppt))

meanMergesep <- merge(pptaveragesep, pptaverage1sep, by="County", all=TRUE)
meanMergesep <- merge(meanMergesep, pptaverage2sep, by="County", all=TRUE)
meanMergesep$Anomaly <- ((meanMergesep$sumppt-meanMergesep$Average)/meanMergesep$sd)
colnames(meanMergesep) = c("County", "year", "sepSumppt", "sepAvgppt", "seppptsd", "seppptAnomaly")

octppt <- pptFlip %>%
  filter(month == "10")

pptaverageoct <- octppt %>%
  group_by(County, year) %>%
  summarize(sumppt = sum(ppt))

pptaverage1oct <- pptaverageoct %>%
  group_by(County) %>%
  summarize(Average = mean(sumppt))

pptaverage2oct <- pptaverageoct %>%
  group_by(County) %>%
  summarize(sd = sd(sumppt))

meanMergeoct <- merge(pptaverageoct, pptaverage1oct, by="County", all=TRUE)
meanMergeoct <- merge(meanMergeoct, pptaverage2oct, by="County", all=TRUE)
meanMergeoct$Anomaly <- ((meanMergeoct$sumppt-meanMergeoct$Average)/meanMergeoct$sd)
colnames(meanMergeoct) = c("County", "year", "octSumppt", "octAvgppt", "octpptsd", "octpptAnomaly")

monthsums <- merge(meanMergemay,meanMergejun, by.x=c("County", "year"), by.y=c("County", "year"), all=TRUE)
monthsums <- merge(monthsums,meanMergejul, by.x=c("County", "year"), by.y=c("County", "year"), all=TRUE)
monthsums <- merge(monthsums,meanMergeaug, by.x=c("County", "year"), by.y=c("County", "year"), all=TRUE)
monthsums <- merge(monthsums,meanMergesep, by.x=c("County", "year"), by.y=c("County", "year"), all=TRUE)
monthsums <- merge(monthsums,meanMergeoct, by.x=c("County", "year"), by.y=c("County", "year"), all=TRUE)

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

#write.csv(mayData, "data/mayppt_Extremes.csv")
#write.csv(junData, "data/junppt_Extremes.csv")
#write.csv(julData, "data/julppt_Extremes.csv")
#write.csv(augData, "data/augppt_Extremes.csv")
#write.csv(sepData, "data/sepppt_Extremes.csv")
#write.csv(octData, "data/octppt_Extremes.csv")


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


colnames(maydf) <- c("Year","County","maypptDays90", "County1", "maypptDays95", "County2", "maypptDays99")
colnames(jundf) <- c("Year","County","junpptDays90", "County1", "junpptDays95", "County2", "junpptDays99")
colnames(juldf) <- c("Year","County","julpptDays90", "County1", "julpptDays95", "County2", "julpptDays99")
colnames(augdf) <- c("Year","County","augpptDays90", "County1", "augpptDays95", "County2", "augpptDays99")
colnames(sepdf) <- c("Year","County","seppptDays90", "County1", "seppptDays95", "County2", "seppptDays99")
colnames(octdf) <- c("Year","County","octpptDays90", "County1", "octpptDays95", "County2", "octpptDays99")
maydf <- maydf%>%
  select(-County1,-County2)
jundf <- jundf%>%
  select(-County1,-County2)
juldf <- juldf%>%
  select(-County1,-County2)
augdf <- augdf%>%
  select(-County1,-County2)
sepdf <- sepdf%>%
  select(-County1,-County2)
octdf <- octdf%>%
  select(-County1,-County2)

#Combine Into One Document

pptfinal <- merge(maydf,jundf, by.x=c("County", "Year"), by.y=c("County", "Year"), all=TRUE)
pptfinal <- merge(pptfinal,juldf, by.x=c("County", "Year"), by.y=c("County", "Year"), all=TRUE)
pptfinal <- merge(pptfinal,augdf, by.x=c("County", "Year"), by.y=c("County", "Year"), all=TRUE)
pptfinal <- merge(pptfinal,sepdf, by.x=c("County", "Year"), by.y=c("County", "Year"), all=TRUE)
pptfinal <- merge(pptfinal,octdf, by.x=c("County", "Year"), by.y=c("County", "Year"), all=TRUE)
pptfinal <- merge(pptfinal,meanMerge, by.x=c("County", "Year"), by.y=c("County", "year"), all=TRUE)
pptfinal <- merge(pptfinal,monthsums, by.x=c("County", "Year"), by.y=c("County", "year"), all=TRUE)

#write.csv(pptfinal, "data/Num_ppt_Extreme_Days2.csv")

# Do we need this?
#dfSpread <- spread(outdf, County, NumDays)
#write.csv(dfSpread, "~/Documents/Geog_803/Analysis/Num_ppt_Extreme_Days_Wide.csv")
