library(dplyr)
library(sf)
library(tidyverse)
library(here)
library(ggplot2)
library(gridExtra)

# Import table
df <- read.csv(here("data/Crops_Final_Figs/Crops_Final_Figs/Fig1/FilteredCropData.csv"))


layers <- st_layers(here("data/boundaries.gdb"))


# Import county polygons
sf <- st_read(here(dsn="data/boundaries.gdb"),layer = "US_county_2010")%>%
  dplyr::filter(STATEFP10 %in% c("37","13","45"))%>%
  st_transform(crs=2958)

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

cornSf <- left_join(sf,cornCount, by = c("GEOID10"="FIPS"))%>%
  mutate("Over_30"=ifelse(cornYears > 29, "YES","NO"))

# Corn Plot
cornPlot <- ggplot(cornSf)+
  geom_sf(aes(fill = Over_30),size=.2)+
  scale_fill_manual(values = c("#e5b91c"),na.value="#c4c4c0")+
  theme(legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background = element_blank(),
        plot.margin = margin(c(0,0,0,-.1), unit = "cm"))

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
  theme(legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background = element_blank(),
        plot.margin = margin(c(0,-.1,0,0), unit = "cm"))

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
  theme(legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background = element_blank(),
        plot.margin = margin(c(0,-.1,0,0), unit = "cm"))

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
  theme(legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background = element_blank(),
        plot.margin = margin(c(0,0,0,-.1), unit = "cm"))

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
  xlim(xmin=-500000,xmax=1500000)+
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
        plot.margin = margin(c(-1,0,0,0), unit = "cm"))
#potatoPlot

NC <- states%>%
  filter(STATEFP10 == "37")

potatoStates <- potatoPlot+
  geom_sf(data = NC,fill=alpha("grey",0),color="black",size=.5)


# Create layout for figure
lay <- rbind(c(1,2),
             c(3,4),
             c(5,5))

## Create Plot without state boundaries
tiff(here("figures/figure01/figure01.tiff"), width = 8, height = 8, units = 'cm', res = 1000)
grid.arrange(cottonPlot,cornPlot,peanutPlot,soybeanPlot,potatoPlot,
             layout_matrix = lay)
dev.off()

## Create Plot with state boundaries
tiff(here("figures/figure01/figure01_boundaries.tiff"), width = 8, height = 8, units = 'cm', res = 1000)
grid.arrange(cottonStates,cornStates,peanutStates,soybeanStates,potatoStates,
             layout_matrix = lay)
dev.off()