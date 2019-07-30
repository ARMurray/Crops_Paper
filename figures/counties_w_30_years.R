library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(sf)

crops <- read.csv(here("data","FinalCrops2.csv"))

counties <- st_read("data/countiesusa/cb_2017_us_county_5m.shp")%>%
  filter(STATEFP %in% c(37,45,13))

# Subset Crops
crops$FIPS <- paste0("F",crops$FIPS)
crops$join <- paste0(crops$FIPS,crops$Year)

#crops <- merge(ppt, crops, by = "join")

soybeans <- crops%>%
  filter(Commodity == "SOYBEANS")

corn <-  crops%>%
  filter(Commodity == "CORN")

cotton <-  crops%>%
  filter(Commodity == "COTTON")

peanuts <-  crops%>%
  filter(Commodity == "PEANUTS")

sweetPotatoes <- crops%>%
  filter(Commodity == "SWEET POTATOES")


# Filter to keep only counties with >29 years of records

# Soybeans
counts <- as.data.frame(table(soybeans$FIPS))
GT30 <- counts%>%
  filter(Freq > 29)
countiesGT30 <- GT30$Var1

soybeanCounts <- counts
soybeanCounties <- nrow(counts)
soybeanCountiesGT30 <- nrow(GT30)

soybeans <- soybeans%>%
  filter(FIPS %in% countiesGT30)

# Corn
counts <- as.data.frame(table(corn$FIPS))
GT30 <- counts%>%
  filter(Freq > 29)
countiesGT30 <- GT30$Var1

cornCounts <- counts
cornCounties <- nrow(counts)
cornCountiesGT30 <- nrow(GT30)

corn <- corn%>%
  filter(FIPS %in% countiesGT30)

# Cotton
counts <- as.data.frame(table(cotton$FIPS))
GT30 <- counts%>%
  filter(Freq > 29)
countiesGT30 <- GT30$Var1

cottonCounts <- counts
cottonCounties <- nrow(counts)
cottonCountiesGT30 <- nrow(GT30)

cotton <- cotton%>%
  filter(FIPS %in% countiesGT30)

# Peanuts
counts <- as.data.frame(table(peanuts$FIPS))
GT30 <- counts%>%
  filter(Freq > 29)
countiesGT30 <- GT30$Var1

peanutsCounts <- counts
peanutsCounties <- nrow(counts)
peanutsCountiesGT30 <- nrow(GT30)

peanuts <- peanuts%>%
  filter(FIPS %in% countiesGT30)

# Sweet Potatoes
counts <- as.data.frame(table(sweetPotatoes$FIPS))
GT30 <- counts%>%
  filter(Freq > 29)
countiesGT30 <- GT30$Var1

sweetPotatoCounts <- counts
sweetPotatoesCounties <- nrow(counts)
sweetPotatoesCountiesGT30 <- nrow(GT30)

sweetPotatoes <- sweetPotatoes%>%
  filter(FIPS %in% countiesGT30)



# Crop Maps
sf <- counties%>%
  select(GEOID)
sf$FIPS <- paste0("F",sf$GEOID)

sf <- merge(sf, cornCounts, by.x = "FIPS", by.y = "Var1",all.x = TRUE )
sf <- merge(sf, cottonCounts, by.x = "FIPS", by.y = "Var1",all.x = TRUE)
sf <- merge(sf, peanutsCounts, by.x = "FIPS", by.y = "Var1",all.x = TRUE)
sf <- merge(sf, soybeanCounts, by.x = "FIPS", by.y = "Var1",all.x = TRUE)
sf <- merge(sf, sweetPotatoCounts, by.x = "FIPS", by.y = "Var1",all.x = TRUE)
colnames(sf) <- c("FIPS","GEOID10","Corn","Cotton","Peanuts","Soybeans","SweetPotatoes","geometry")

sf <- sf%>%
  mutate(Corn = ifelse(Corn < 30 , NA ,Corn))%>%
  mutate(Cotton = ifelse(Cotton < 30 , NA ,Cotton))%>%
  mutate(Peanuts = ifelse(Peanuts < 30 , NA ,Peanuts))%>%
  mutate(Soybeans = ifelse(Soybeans < 30 , NA ,Soybeans))%>%
  mutate(SweetPotatoes = ifelse(SweetPotatoes < 30 , NA ,SweetPotatoes))

corn <- ggplot()+
  geom_sf(data = sf, aes(fill = Corn))+
  scale_fill_gradient(low = "yellow", high = "yellow", na.value = 'white')+
  labs(title = "Years of Corn Yield Data between 1981-2017")+
  guides(fill=guide_colorbar(title="Years of Data"))
corn

cotton <- ggplot()+
  geom_sf(data = sf, aes(fill = Cotton))+
  scale_fill_gradient(low = "blue", high = "blue", na.value = 'white')+
  labs(title = "Years of Cotten Yield Data between 1981-2017")+
  guides(fill=guide_colorbar(title="Years of Data"))
cotton

peanuts <- ggplot()+
  geom_sf(data = sf, aes(fill = Peanuts))+
  scale_fill_gradient(low = "#874e00", high = "#874e00", na.value = 'white')+
  labs(title = "Years of Peanut Yield Data between 1981-2017")+
  guides(fill=guide_colorbar(title="Years of Data"))
peanuts

soybeans <- ggplot()+
  geom_sf(data = sf, aes(fill = Soybeans))+
  scale_fill_gradient(low = "#ba14a1", high = "#ba14a1", na.value = 'white')+
  labs(title = "Years of Soybean Yield Data between 1981-2017")+
  guides(fill=guide_colorbar(title="Years of Data"))
soybeans

sf <- sf%>%
  filter(substr(FIPS,1,3)=="F37")

sweetPotatoes <- ggplot()+
  geom_sf(data = sf, aes(fill = SweetPotatoes))+
  scale_fill_gradient(low = "#d67d02", high = "#d67d02", na.value = 'white')+
  labs(title = "Years of Sweet Potato Yield Data between 1981-2017")+
  guides(fill=guide_colorbar(title="Years of Data"))
sweetPotatoes
