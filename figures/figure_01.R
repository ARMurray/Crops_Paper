library(dplyr)
library(sf)
library(tidyverse)
library(here)
library(ggplot2)

# Import table
df <- read.csv(here("data/Crops_Final_Figs/Crops_Final_Figs/Fig1/FilteredCropData.csv"))

# Import shapefile of counties
sf <- st_read(here("data/shapefiles/tl_2015_us_county.shp"))%>%
  dplyr::filter(STATEFP %in% c("37","13","45"))%>%
  st_transform(crs=2958)

## CORN ##

# Filter and count corn records by county
cornCount <- df%>%
  filter(Commodity=="CORN")%>%
  select(FIPS)%>%
  table()%>%
  as.data.frame()
colnames(cornCount) <- c("FIPS","cornYears")

cornSf <- left_join(sf,cornCount, by = c("GEOID"="FIPS"))%>%
  mutate("Over_30"=ifelse(cornYears > 29, "YES","NO"))

# Corn Plot
cornPlot <- ggplot(cornSf)+
  geom_sf(aes(fill = Over_30))+
  scale_fill_manual(values = c("#e5b91c"),na.value="#adaca8")+
  theme(legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


## COTTON ##
# Filter and count corn records by county
cottonCount <- df%>%
  filter(Commodity=="COTTON")%>%
  select(FIPS)%>%
  table()%>%
  as.data.frame()
colnames(cottonCount) <- c("FIPS","cottonYears")

cottonSf <- left_join(sf,cottonCount, by = c("GEOID"="FIPS"))%>%
  mutate("Over_30"=ifelse(cottonYears > 29, "YES","NO"))

# Cotton Plot
cottonPlot <- ggplot(cottonSf)+
  geom_sf(aes(fill = Over_30))+
  scale_fill_manual(values = c("#5fadcc"),na.value="#adaca8")+
  theme(legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

## PEANUTS ##

# Filter and count peanuts records by county
peanutCount <- df%>%
  filter(Commodity=="PEANUTS")%>%
  select(FIPS)%>%
  table()%>%
  as.data.frame()
colnames(peanutCount) <- c("FIPS","peanutYears")

peanutSf <- left_join(sf,peanutCount, by = c("GEOID"="FIPS"))%>%
  mutate("Over_30"=ifelse(peanutYears > 29, "YES","NO"))

# Peanut Plot
peanutPlot <- ggplot(peanutSf)+
  geom_sf(aes(fill = Over_30))+
  scale_fill_manual(values = c("#e30425"),na.value="#adaca8")+
  theme(legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


## SOYBEANS##

# Filter and count peanuts records by county
soybeanCount <- df%>%
  filter(Commodity=="SOYBEANS")%>%
  select(FIPS)%>%
  table()%>%
  as.data.frame()
colnames(soybeanCount) <- c("FIPS","soybeanYears")

soybeanSf <- left_join(sf,soybeanCount, by = c("GEOID"="FIPS"))%>%
  mutate("Over_30"=ifelse(soybeanYears > 29, "YES","NO"))

# soybean Plot
soybeanPlot <- ggplot(soybeanSf)+
  geom_sf(aes(fill = Over_30))+
  scale_fill_manual(values = c("#0a8974"),na.value="#adaca8")+
  theme(legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


## SWEET POTATOES## 


# Filter and count potato records by county
potatoCount <- df%>%
  filter(Commodity=="SWEET POTATOES")%>%
  select(FIPS)%>%
  table()%>%
  as.data.frame()
colnames(potatoCount) <- c("FIPS","potatoYears")

potatoSf <- left_join(sf,potatoCount, by = c("GEOID"="FIPS"))%>%
  mutate("Over_30"=ifelse(potatoYears > 29, "YES","NO"))

# potato Plot
potatoPlot <- ggplot(potatoSf)+
  geom_sf(aes(fill = Over_30))+
  scale_fill_manual(values = c("#e47927"),na.value="#adaca8")+
  theme(legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


plot <- plotly::subplot(cottonPlot,cornPlot,peanutPlot,soybeanPlot, nrows = 2, shareY = TRUE, shareX = TRUE, margin = -.01)
plot
