# PPT Stats by county

library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)

ppt <- read.csv("data/PPT_All_1981_2017.csv")%>%
  select(-X)

counties <- st_read("data/countiesusa/cb_2017_us_county_5m.shp")

seCounties <- counties%>%
  filter(STATEFP %in% c(37,45,13))

seCounties$ID <- paste0("F",seCounties$STATEFP,seCounties$COUNTYFP)

pptFlip <- gather(ppt, County, ppt, -Date)

pptFlip <- pptFlip %>% separate(Date, sep="/", into = c("day", "month", "year"))
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
meanPPT <- summarize(grpCounty, meanPPT = mean(ppt))
seCountyppt <- merge(seCounties, meanPPT, by.x = "ID", by.y="County")

plot <- ggplot(seCountyppt)+
  geom_sf(aes(fill = meanPPT))

plot

#st_write(seCountyppt, "~/Documents/Geog_803/Analysis/dailymean.shp")

# Calculate Extreme ppt (top 10%. 5%, 1% of values)
# trace precip is considered .127 mm
outData <- data.frame()
counties <- seCounties$ID
for(n in 1:length(counties)){
  county <- pptFlip%>%
    filter(County == counties[n] & ppt > 0.127)
  quant90 <- quantile(county$ppt, .90)
  quant95 <- quantile(county$ppt, .95)
  quant99 <- quantile(county$ppt, .99)
  newData <- data.frame("County" = county$County[1], "Quant90" = quant90, "Quant95" = quant95,"Quant99" = quant99)
  outData <- rbind(outData, newData)
}

#write.csv(outData, "data/PPT_Extremes.csv")

# Count number of days by year that ppt extremes were exceeded

pptMerge <- merge(pptFlip, outData, by="County")

outdf <- data.frame()
for(n in 1981:2017){
  data <- pptMerge%>%
    filter(year == n)
  days90 <- group_by(data, County)%>%
    tally(ppt>Quant90)
  days95 <- group_by(data, County)%>%
    tally(ppt>Quant95)
  days99 <- group_by(data, County)%>%
    tally(ppt>Quant99)
  output <- data.frame("Year" = data$year[1], "extpptDays90" = days90, "extpptDays95" = days95,"extpptDays99" = days99)
  outdf <- rbind(outdf, output)
  
}

colnames(outdf) <- c("Year","County","NumpptDays90", "County1", "NumpptDays95", "County2", "NumpptDays99")
c <- merge(outdf,meanMerge, by.x=c("County", "Year"), by.y=c("County", "year"), all=TRUE)
#write.csv(outdf, "data/Num_ppt_Extreme_Days.csv")

# Do we need this?
#dfSpread <- spread(outdf, County, NumDays)
#write.csv(dfSpread, "~/Documents/Geog_803/Analysis/Num_ppt_Extreme_Days_Wide.csv")
