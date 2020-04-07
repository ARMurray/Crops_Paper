library(dplyr)
library(sf)
library(tidyverse)
library(here)
library(ggplot2)
library(gridExtra)

# Import table
df <- read.csv(here("data/Crops_Final_Figs/Crops_Final_Figs/Fig1/FilteredCropData.csv"))

#layers <- st_layers(here("data/boundaries.gdb"))

# Import county polygons
sf <- st_read(here(dsn="data/boundaries.gdb"),layer = "US_county_2010")%>%
  dplyr::filter(STATEFP10 %in% c("37","13","45"))%>%
  st_transform(crs=2958)%>%
  mutate('GEOID10'=as.integer(as.character(GEOID10)))

# Import state polygons
states <- st_read(here(dsn="data/boundaries.gdb"),layer = "US_state_2010")%>%
  dplyr::filter(STATEFP10 %in% c("37","13","45"))%>%
  st_transform(crs=2958)

## CORN ##

# Filter and count corn records by county
cornCount <- df%>%
  filter(Commodity=="CORN")%>%
  select(FIPS)%>%
  table()%>%
  as.data.frame()
colnames(cornCount) <- c("FIPS","cornYears")

cornCount$FIPS <- as.integer(as.character(cornCount$FIPS))

cornSf <- left_join(sf,cornCount, by = c("GEOID10"="FIPS"))%>%
  mutate("Over_30"=ifelse(cornYears > 29, "YES","NO"))

# Corn Plot
cornPlot <- ggplot(cornSf)+
  geom_sf(aes(fill = Over_30),size=.2)+
  scale_fill_manual(values = c("#e5b91c"),na.value="#c4c4c0")+
  xlim(xmin=79536,xmax=1002211)+
  theme(legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background = element_blank(),
        plot.margin = margin(c(0,0,0,-2), unit = "cm"))+
  geom_text(x=751000, y=3500000, label="Corn", size = 6)

cornStates <- cornPlot+
  geom_sf(data = states,fill=alpha("grey",0),color="black",size=.5)

## COTTON ##
# Filter and count corn records by county
cottonCount <- df%>%
  filter(Commodity=="COTTON")%>%
  select(FIPS)%>%
  table()%>%
  as.data.frame()
colnames(cottonCount) <- c("FIPS","cottonYears")

cottonSf <- left_join(sf,cottonCount, by = c("GEOID10"="FIPS"))%>%
  mutate("Over_30"=ifelse(cottonYears > 29, "YES","NO"))

# Cotton Plot
cottonPlot <- ggplot(cottonSf)+
  geom_sf(aes(fill = Over_30),size=.2)+
  scale_fill_manual(values = c("#5fadcc"),na.value="#c4c4c0")+
  xlim(xmin=79536,xmax=1002211)+
  theme(legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background = element_blank(),
        plot.margin = margin(c(0,-2,0,0), unit = "cm"))+
  geom_text(x=751000, y=3500000, label="Cotton", size = 6)

cottonStates <- cottonPlot+
  geom_sf(data = states,fill=alpha("grey",0),color="black",size=.5)

## PEANUTS ##

# Filter and count peanuts records by county
peanutCount <- df%>%
  filter(Commodity=="PEANUTS")%>%
  select(FIPS)%>%
  table()%>%
  as.data.frame()
colnames(peanutCount) <- c("FIPS","peanutYears")

peanutSf <- left_join(sf,peanutCount, by = c("GEOID10"="FIPS"))%>%
  mutate("Over_30"=ifelse(peanutYears > 29, "YES","NO"))

# Peanut Plot
peanutPlot <- ggplot(peanutSf)+
  geom_sf(aes(fill = Over_30),size=.2)+
  scale_fill_manual(values = c("#e30425"),na.value="#c4c4c0")+
  xlim(xmin=79536,xmax=1002211)+
  theme(legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background = element_blank(),
        plot.margin = margin(c(0,-2,0,0), unit = "cm"))+
  geom_text(x=751000, y=3500000, label="Peanuts", size = 6)

peanutStates <- peanutPlot+
  geom_sf(data = states,fill=alpha("grey",0),color="black",size=.5)


## SOYBEANS##

# Filter and count peanuts records by county
soybeanCount <- df%>%
  filter(Commodity=="SOYBEANS")%>%
  select(FIPS)%>%
  table()%>%
  as.data.frame()
colnames(soybeanCount) <- c("FIPS","soybeanYears")

soybeanSf <- left_join(sf,soybeanCount, by = c("GEOID10"="FIPS"))%>%
  mutate("Over_30"=ifelse(soybeanYears > 29, "YES","NO"))

# soybean Plot
soybeanPlot <- ggplot(soybeanSf)+
  geom_sf(aes(fill = Over_30),size=.2)+
  scale_fill_manual(values = c("#0a8974"),na.value="#c4c4c0")+
  xlim(xmin=79536,xmax=1002211)+
  theme(legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background = element_blank(),
        plot.margin = margin(c(0,0,0,-2), unit = "cm"))+
  geom_text(x=761000, y=3500000, label="Soybeans", size = 6)

soybeanStates <- soybeanPlot+
  geom_sf(data = states,fill=alpha("grey",0),color="black",size=.5)

## SWEET POTATOES## 


# Filter and count potato records by county
potatoCount <- df%>%
  filter(Commodity=="SWEET POTATOES")%>%
  select(FIPS)%>%
  table()%>%
  as.data.frame()
colnames(potatoCount) <- c("FIPS","potatoYears")

potatoSf <- left_join(sf,potatoCount, by = c("GEOID10"="FIPS"))%>%
  mutate("Over_30"=ifelse(potatoYears > 29, "YES","NO"),
         "STATEFP10" = as.character(STATEFP10))%>%
  filter(STATEFP10 == "37")

# potato Plot
potatoPlot <- ggplot(potatoSf)+
  geom_sf(aes(fill = Over_30),size=.2)+
  scale_fill_manual(values = c("#e47927"),na.value="#c4c4c0")+
  xlim(xmin=-500000,xmax=2000000)+
  ylim(ymin=3747000,ymax=4057400)+
  theme(legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background = element_blank(),
        plot.margin = margin(c(-2,0,0,0), unit = "cm"))+
  geom_text(x=1250000, y=3780000, label="Sweet Potatoes", size = 6)
potatoPlot

NC <- states%>%
  filter(STATEFP10 == "37")

potatoStates <- potatoPlot+
  geom_sf(data = NC,fill=alpha("grey",0),color="black",size=.5)


# Create layout for figure
lay <- rbind(c(1,2),
             c(3,4),
             c(5,5))

## Create Plot without state boundaries
tiff(here("figures/figure01/figure01.tiff"), width = 15.24, height = 12.7, units = 'cm', res = 1000)
grid.arrange(cottonPlot,cornPlot,peanutPlot,soybeanPlot,potatoPlot,
             layout_matrix = lay)
dev.off()

## Create Plot with state boundaries
tiff(here("figures/figure01/figure01_boundaries.tiff"), width = 15.24, height = 12.7, units = 'cm', res = 1000)
grid.arrange(cottonStates,cornStates,peanutStates,soybeanStates,potatoStates,
             layout_matrix = lay)
dev.off()