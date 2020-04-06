library(dplyr)
library(sf)
library(tidyverse)
library(here)
library(ggplot2)
library(gridExtra)

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
ppt <- read.csv(here("data/Crops_Final_Figs/Crops_Final_Figs/Figs6and8/PPTCountyCorrelation.csv"))
tmax <- read.csv(here("data/Crops_Final_Figs/Crops_Final_Figs/Figs6and8/TMAXCountyCorrelation.csv"))
tmin <- read.csv(here("data/Crops_Final_Figs/Crops_Final_Figs/Figs6and8/TMINCountyCorrelation.csv"))

sfJoin <- sf%>%
  left_join(ppt, by = c('GEOID10'='FIPS'))

ggplot(sfJoin)+
  geom_sf(aes(fill = corr))
