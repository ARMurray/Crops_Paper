library(dplyr)
library(sf)
library(tidyverse)
library(here)
library(ggplot2)
library(gridExtra)

## SET VARIABLE (PPT or TMAX or TMIN)
var <- 'PPT'

# Colors for Classification
cols <- c("-0.71 : -0.5" = "#730000",
          "< -0.5" = "#a80000",
          "< -.05" = "#d95d5d",
          "< 0.05" = "#FFFFFF",
          "< 0.25" = "#94f2ab",
          "< 0.50" = "#43c463",
          "0.50 : 0.67" = "#0b992e")

# Import county polygons
sf <- st_read(here(dsn="data/boundaries.gdb"),layer = "US_county_2010")%>%
  dplyr::filter(STATEFP10 %in% c("37","13","45"))%>%
  st_transform(crs=2958)%>%
  mutate('GEOID10'=as.integer(as.character(GEOID10)))

# Import state polygons
states <- st_read(here(dsn="data/boundaries.gdb"),layer = "US_state_2010")%>%
  dplyr::filter(STATEFP10 %in% c("37","13","45"))%>%
  st_transform(crs=2958)

# Import Data
df <- read.csv(paste0(here("data/Crops_Final_Figs/Crops_Final_Figs/Figs6and8"),"/",var,"CountyCorrelation.csv"))%>%
  mutate('Class' = ifelse(corr < -.5, "-0.71 : -0.5",
                          ifelse(corr < -.25,"< -0.5",
                                 ifelse(corr < 0,"< -.05",
                                        ifelse(corr < .25,"< 0.05",
                                               ifelse(corr < .5,"< 0.25",
                                                      ifelse(corr < .5,"< 0.50","0.50 : 0.67")))))))
# Filter to Corn
corn <- df%>%
  filter(Commodity == "CORN")

# Join Corn to county polygons and classify correllations
cornSf <- left_join(sf,corn, by = c('GEOID10'='FIPS'))

cornPlot <- ggplot(cornSf)+
  geom_sf(aes(fill = Class),size=.2)+
  scale_fill_manual(values = cols,
                    na.value="#c4c4c0")+
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

############ COTTON ###############
# Filter df to Cotton
cotton <- df%>%
  filter(Commodity == "COTTON")

# Join to county polygons and classify
cottonSf <- left_join(sf,cotton, by = c('GEOID10'='FIPS'))

# Cotton Plot
cottonPlot <- ggplot(cottonSf)+
  geom_sf(aes(fill = Class),size=.2)+
  scale_fill_manual(values = cols,na.value="#c4c4c0")+
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


############ PEANUTS ###############
# Filter df to Peanuts
peanuts <- df%>%
  filter(Commodity == "PEANUTS")

# Join to county polygons and classify
peanutSf <- left_join(sf,peanuts, by = c('GEOID10'='FIPS'))

# Peanut Plot
peanutPlot <- ggplot(peanutSf)+
  geom_sf(aes(fill = Class),size=.2)+
  scale_fill_manual(values = cols,na.value="#c4c4c0")+
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


############ SOYBEANS ###############
# Filter df to Soybeans
soybeans <- df%>%
  filter(Commodity == "SOYBEANS")

# Join to county polygons and classify
soybeanSf <- left_join(sf,soybeans, by = c('GEOID10'='FIPS'))

# soybean Plot
soybeanPlot <- ggplot(soybeanSf)+
  geom_sf(aes(fill = Class),size=.2)+
  scale_fill_manual(values = cols,na.value="#c4c4c0")+
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


############ SWEET POTATOES ###############
# Filter df to Sweet Potatoes
potatoes <- df%>%
  filter(Commodity == "SWEET POTATOES")

# Filter sf to NC
nc <- sf%>%
  filter(STATEFP10 == '37')

# Join to county polygons and classify
potatoSf <- left_join(nc,potatoes, by = c('GEOID10'='FIPS'))

# potato Plot
potatoPlot <- ggplot(potatoSf)+
  geom_sf(aes(fill = Class),size=.2)+
  scale_fill_manual(values = cols,na.value="#c4c4c0")+
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

NC <- states%>%
  filter(STATEFP10 == "37")

potatoStates <- potatoPlot+
  geom_sf(data = NC,fill=alpha("grey",0),color="black",size=.5)

##------- Create Plot with state boundaries -------##
# Create layout for figure
lay <- rbind(c(1,2),
             c(3,4),
             c(5,5))

tiff(here("figures/figure06_PPT/figure06_PPT.tiff"), width = 15.24, height = 12.7, units = 'cm', res = 1000)
grid.arrange(cottonStates,cornStates,peanutStates,soybeanStates,potatoStates,
             layout_matrix = lay)
dev.off()