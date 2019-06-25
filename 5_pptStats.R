# PPT Stats by county

library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)

ppt <- read.csv("~/Documents/Geog_803/Analysis/PPT_All_1981_2017.csv")%>%
  select(-X)

counties <- st_read("~/Documents/Geog_803/shapefiles/US_County_2010.shp")

seCounties <- counties%>%
  filter(STATEFP10 %in% c(37,45,13))

seCounties$ID <- paste0("F",seCounties$STATEFP10,seCounties$COUNTYFP10)

pptFlip <- gather(ppt, County, ppt, -Date)

grpCounty <- group_by(pptFlip, County)

# Calculate Daily Mean by County for entire time period
meanPPT <- summarize(grpCounty, meanPPT = mean(ppt))
seCountyppt <- merge(seCounties, meanPPT, by.x = "ID", by.y="County")

plot <- ggplot(seCountyppt)+
  geom_sf(aes(fill = meanPPT))

#plot

#st_write(seCountyppt, "~/Documents/Geog_803/Analysis/dailymean.shp")

# Calculate Extreme ppt (top 10% of values)
# trace precip is considered .127 mm
outData <- data.frame()
counties <- seCounties$ID
for(n in 1:length(counties)){
  county <- pptFlip%>%
    filter(County == counties[n] & ppt > 0.127)
  quant90 <- quantile(county$ppt, .90)
  newData <- data.frame("County" = county$County[1], "Quant90" = quant90)
  outData <- rbind(outData, newData)
}

write.csv(outData, "~/Documents/Geog_803/Analysis/PPT_Extremes.csv")

# Count number of days by year that ppt extremes were exceeded

pptFlip$year <- as.numeric(substr(pptFlip$Date,1,4))
pptMerge <- merge(pptFlip, outData, by="County")

outdf <- data.frame()
for(n in 1981:2017){
  data <- pptMerge%>%
    filter(year == n)
  days <- group_by(data, County)%>%
    tally(ppt>Quant90)
  output <- data.frame("Year" = data$year[1], "extPptDays" = days)
  outdf <- rbind(outdf, output)
  
}

colnames(outdf) <- c("Year","County","NumDays")
dfSpread <- spread(outdf, County, NumDays)

write.csv(outdf, "~/Documents/Geog_803/Analysis/Num_ppt_Extreme_Days.csv")
write.csv(dfSpread, "~/Documents/Geog_803/Analysis/Num_ppt_Extreme_Days_Wide.csv")


# Plot extremes vs time

plot <- ggplot(outdf, aes(x = Year, y = NumDays, group = Year))+
  geom_boxplot()+
  labs(title = "Distribution of Extreme Precipitation Days by County (NC / SC / GA)")

plotly::ggplotly(plot)
