#Here I am. Andrew is a dummy. 
# temp Stats by county

library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)

temp <- read.csv("data/tmax_wide_1981_2017.csv")%>%
  select(-X)

counties <- st_read("data/countiesusa/cb_2017_us_county_5m.shp")

seCounties <- counties%>%
  filter(STATEFP %in% c(37,45,13))

seCounties$ID <- paste0("F",seCounties$STATEFP,seCounties$COUNTYFP)

tempFlip <- gather(temp, County, temp, -Date)
tempFlip$Date <- as.Date(tempFlip$Date)

tempFlip <- tempFlip %>% separate(Date, sep="-", into = c("year", "month", "day"))
tempFlip$month <- as.numeric(tempFlip$month)

tempFlip <- filter(tempFlip, month %in% 5:10)

grpCounty <- group_by(tempFlip, County)

#Calculate Growing Season and Standard Deviation Average High Temps
tempaverage <- tempFlip %>%
  group_by(County, year) %>%
  summarize(meantemp = mean(temp))

tempaverage1 <- tempaverage %>%
  group_by(County) %>%
  summarize(Average = mean(meantemp))

tempaverage2 <- tempaverage %>%
  group_by(County) %>%
  summarize(sd = sd(meantemp))

meanMerge <- merge(tempaverage, tempaverage1, by="County", all=TRUE)
meanMerge <- merge(meanMerge, tempaverage2, by="County", all=TRUE)
meanMerge$Anomaly <- ((meanMerge$meantemp-meanMerge$Average)/meanMerge$sd)

#Create Monthly Subsets

maytemp  <- tempFlip %>%
  filter(month == "5")

juntemp<- tempFlip %>%
  filter(month == "6")

jultemp <- tempFlip %>%
  filter(month == "7")

augtemp <- tempFlip %>%
  filter(month == "8")

septemp <- tempFlip %>%
  filter(month == "9")

octtemp <- tempFlip %>%
  filter(month == "10")

# Calculate Daily Mean by County for entire time period
meantemp <- summarize(grpCounty, meantemp = mean(temp))
seCountytemp <- merge(seCounties, meantemp, by.x = "ID", by.y="County")

plot <- ggplot(seCountytemp)+
  geom_sf(aes(fill = meantemp))

plot

#st_write(seCountytemp, "~/Documents/Geog_803/Analysis/dailymean.shp")

# Calculate Extreme temp (top 10%, 5%, 1% of values)
outData <- data.frame()
counties <- seCounties$ID
for(n in 1:length(counties)){
  county <- tempFlip%>%
    filter(County == counties[n])
  quant90 <- quantile(county$temp, .90)
  quant95 <- quantile(county$temp, .95)
  quant99 <- quantile(county$temp, .99)
  newData <- data.frame("County" = county$County[1], "Quant90" = quant90, "Quant95" = quant95,"Quant99" = quant99)
  outData <- rbind(outData, newData)
}

#write.csv(outData, "data/temp_Extremes.csv")

# Count number of days by year that temp extremes were exceeded
#This needs to be done for each month

tempMerge <- merge(tempFlip, outData, by="County")

outdf <- data.frame()
for(n in 1981:2017){
  data <- tempMerge%>%
    filter(year == n)
  days90 <- group_by(data, County)%>%
    tally(temp>Quant90)
  days95 <- group_by(data, County)%>%
    tally(temp>Quant95)
  days99 <- group_by(data, County)%>%
    tally(temp>Quant99)
  seasontotal <- group_by(data, County)%>%
    tally(temp)
  output <- data.frame("Year" = data$year[1], "exttempDays90" = days90, "exttempDays95" = days95,"exttempDays99" = days99)
  outdf <- rbind(outdf, output)
  
}

colnames(outdf) <- c("Year","County","NumTempDays90", "County1", "NumTempDays95", "County2", "NumTempDays99")

#write.csv(outdf, "data/Num_temp_Extreme_Days.csv")

#Do we really need this?
dfSpread <- spread(outdf, County, NumDays)
#write.csv(dfSpread, "data/Num_temp_Extreme_Days_Wide.csv")


