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
#meantemp <- summarize(grpCounty, meantemp = mean(temp))
#seCountytemp <- merge(seCounties, meantemp, by.x = "ID", by.y="County")

#plot <- ggplot(seCountytemp)+
 # geom_sf(aes(fill = meantemp))

#lot

#st_write(seCountytemp, "~/Documents/Geog_803/Analysis/dailymean.shp")

# Calculate Extreme temp (top 10%, 5%, 1% of values)
mayData <- data.frame()
counties <- seCounties$ID
for(n in 1:length(counties)){
  county <- maytemp%>%
    filter(County == counties[n])
  quant90 <- quantile(county$temp, .90)
  quant95 <- quantile(county$temp, .95)
  quant99 <- quantile(county$temp, .99)
  newData <- data.frame("County" = county$County[1], "Quant90" = quant90, "Quant95" = quant95,"Quant99" = quant99)
  mayData <- rbind(mayData, newData)
}

junData <- data.frame()
counties <- seCounties$ID
for(n in 1:length(counties)){
  county <- juntemp%>%
    filter(County == counties[n])
  quant90 <- quantile(county$temp, .90)
  quant95 <- quantile(county$temp, .95)
  quant99 <- quantile(county$temp, .99)
  newData <- data.frame("County" = county$County[1], "Quant90" = quant90, "Quant95" = quant95,"Quant99" = quant99)
  junData <- rbind(junData, newData)
}

julData <- data.frame()
counties <- seCounties$ID
for(n in 1:length(counties)){
  county <- jultemp%>%
    filter(County == counties[n])
  quant90 <- quantile(county$temp, .90)
  quant95 <- quantile(county$temp, .95)
  quant99 <- quantile(county$temp, .99)
  newData <- data.frame("County" = county$County[1], "Quant90" = quant90, "Quant95" = quant95,"Quant99" = quant99)
  julData <- rbind(julData, newData)
}

augData <- data.frame()
counties <- seCounties$ID
for(n in 1:length(counties)){
  county <- augtemp%>%
    filter(County == counties[n])
  quant90 <- quantile(county$temp, .90)
  quant95 <- quantile(county$temp, .95)
  quant99 <- quantile(county$temp, .99)
  newData <- data.frame("County" = county$County[1], "Quant90" = quant90, "Quant95" = quant95,"Quant99" = quant99)
  augData <- rbind(augData, newData)
}

sepData <- data.frame()
counties <- seCounties$ID
for(n in 1:length(counties)){
  county <- septemp%>%
    filter(County == counties[n])
  quant90 <- quantile(county$temp, .90)
  quant95 <- quantile(county$temp, .95)
  quant99 <- quantile(county$temp, .99)
  newData <- data.frame("County" = county$County[1], "Quant90" = quant90, "Quant95" = quant95,"Quant99" = quant99)
  sepData <- rbind(sepData, newData)
}

octData <- data.frame()
counties <- seCounties$ID
for(n in 1:length(counties)){
  county <- octtemp%>%
    filter(County == counties[n])
  quant90 <- quantile(county$temp, .90)
  quant95 <- quantile(county$temp, .95)
  quant99 <- quantile(county$temp, .99)
  newData <- data.frame("County" = county$County[1], "Quant90" = quant90, "Quant95" = quant95,"Quant99" = quant99)
  octData <- rbind(octData, newData)
}

#write.csv(mayData, "data/maytemp_Extremes.csv")
#write.csv(junData, "data/juntemp_Extremes.csv")
#write.csv(julData, "data/jultemp_Extremes.csv")
#write.csv(augData, "data/augtemp_Extremes.csv")
#write.csv(sepData, "data/septemp_Extremes.csv")
#write.csv(octData, "data/octtemp_Extremes.csv")

# Count number of days by year that temp extremes were exceeded
#This needs to be done for each month

mayMerge <- merge(maytemp, mayData, by="County")
junMerge <- merge(juntemp, junData, by="County")
julMerge <- merge(jultemp, julData, by="County")
augMerge <- merge(augtemp, augData, by="County")
sepMerge <- merge(septemp, sepData, by="County")
octMerge <- merge(octtemp, octData, by="County")

maydf <- data.frame()
for(n in 1981:2017){
  data <- mayMerge%>%
    filter(year == n)
  days90 <- group_by(data, County)%>%
    tally(temp>Quant90)
  days95 <- group_by(data, County)%>%
    tally(temp>Quant95)
  days99 <- group_by(data, County)%>%
    tally(temp>Quant99)
  output <- data.frame("Year" = data$year[1], "exttempDays90" = days90, "exttempDays95" = days95,"exttempDays99" = days99)
  maydf <- rbind(maydf, output)
  
}


jundf <- data.frame()
for(n in 1981:2017){
  data <- junMerge%>%
    filter(year == n)
  days90 <- group_by(data, County)%>%
    tally(temp>Quant90)
  days95 <- group_by(data, County)%>%
    tally(temp>Quant95)
  days99 <- group_by(data, County)%>%
    tally(temp>Quant99)
  output <- data.frame("Year" = data$year[1], "exttempDays90" = days90, "exttempDays95" = days95,"exttempDays99" = days99)
  jundf <- rbind(jundf, output)
  
}

juldf <- data.frame()
for(n in 1981:2017){
  data <- julMerge%>%
    filter(year == n)
  days90 <- group_by(data, County)%>%
    tally(temp>Quant90)
  days95 <- group_by(data, County)%>%
    tally(temp>Quant95)
  days99 <- group_by(data, County)%>%
    tally(temp>Quant99)
  output <- data.frame("Year" = data$year[1], "exttempDays90" = days90, "exttempDays95" = days95,"exttempDays99" = days99)
  juldf <- rbind(juldf, output)
  
}

augdf <- data.frame()
for(n in 1981:2017){
  data <- augMerge%>%
    filter(year == n)
  days90 <- group_by(data, County)%>%
    tally(temp>Quant90)
  days95 <- group_by(data, County)%>%
    tally(temp>Quant95)
  days99 <- group_by(data, County)%>%
    tally(temp>Quant99)
  output <- data.frame("Year" = data$year[1], "exttempDays90" = days90, "exttempDays95" = days95,"exttempDays99" = days99)
  augdf <- rbind(augdf, output)
  
}

sepdf <- data.frame()
for(n in 1981:2017){
  data <- sepMerge%>%
    filter(year == n)
  days90 <- group_by(data, County)%>%
    tally(temp>Quant90)
  days95 <- group_by(data, County)%>%
    tally(temp>Quant95)
  days99 <- group_by(data, County)%>%
    tally(temp>Quant99)
  output <- data.frame("Year" = data$year[1], "exttempDays90" = days90, "exttempDays95" = days95,"exttempDays99" = days99)
  sepdf <- rbind(sepdf, output)
  
}

octdf <- data.frame()
for(n in 1981:2017){
  data <- octMerge%>%
    filter(year == n)
  days90 <- group_by(data, County)%>%
    tally(temp>Quant90)
  days95 <- group_by(data, County)%>%
    tally(temp>Quant95)
  days99 <- group_by(data, County)%>%
    tally(temp>Quant99)
  output <- data.frame("Year" = data$year[1], "exttempDays90" = days90, "exttempDays95" = days95,"exttempDays99" = days99)
  octdf <- rbind(octdf, output)
  
}
colnames(maydf) <- c("Year","County","maytempDays90", "County1", "maytempDays95", "County2", "maytempDays99")
colnames(jundf) <- c("Year","County","juntempDays90", "County1", "juntempDays95", "County2", "juntempDays99")
colnames(juldf) <- c("Year","County","jultempDays90", "County1", "jultempDays95", "County2", "jultempDays99")
colnames(augdf) <- c("Year","County","augtempDays90", "County1", "augtempDays95", "County2", "augtempDays99")
colnames(sepdf) <- c("Year","County","septempDays90", "County1", "septempDays95", "County2", "septempDays99")
colnames(octdf) <- c("Year","County","octtempDays90", "County1", "octtempDays95", "County2", "octtempDays99")
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

tempfinal <- merge(maydf,jundf, by.x=c("County", "Year"), by.y=c("County", "Year"), all=TRUE)
tempfinal <- merge(tempfinal,juldf, by.x=c("County", "Year"), by.y=c("County", "Year"), all=TRUE)
tempfinal <- merge(tempfinal,augdf, by.x=c("County", "Year"), by.y=c("County", "Year"), all=TRUE)
tempfinal <- merge(tempfinal,sepdf, by.x=c("County", "Year"), by.y=c("County", "Year"), all=TRUE)
tempfinal <- merge(tempfinal,octdf, by.x=c("County", "Year"), by.y=c("County", "Year"), all=TRUE)
tempfinal <- merge(tempfinal,meanMerge, by.x=c("County", "Year"), by.y=c("County", "year"), all=TRUE)

#Merge PPT and Temp Final 
COMBO <- merge(tempfinal,pptfinal, by.x=c("County", "Year"), by.y=c("County", "Year"), all=TRUE)

#write.csv(COMBO, "data/ppt_temp_combined.csv")
#write.csv(tempfinal, "data/Num_temp_Extreme_Days.csv")

#Do we really need this?
dfSpread <- spread(outdf, County, NumDays)
#write.csv(dfSpread, "data/Num_temp_Extreme_Days_Wide.csv")


