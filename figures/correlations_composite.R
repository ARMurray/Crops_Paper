library(dplyr)
library(sf)
library(tidyverse)
library(here)
library(ggplot2)
library(gridExtra)

## SET VARIABLE (PPT or TMAX or TMIN)
var <- 'PPT'

# Colors for Classification
cols <- c("-0.71 : -0.50" = "#800505",
          "< -0.25" = "#bf1717",
          "< -0.05" = "#d95d5d",
          "< 0.05" = "#e0ce75",
          "< 0.25" = "#43c463",
          "< 0.50" = "#1d7d35",
          "0.50 : 0.67" = "#025216")

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
  mutate('Class' = ifelse(corr < -.5, "-0.71 : -0.50",
                          ifelse(corr < -.25,"< -0.25",
                                 ifelse(corr < -0.05,"< -0.05",
                                        ifelse(corr < 0.05,"< 0.05",
                                               ifelse(corr < .25,"< 0.25",
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

cornPPT <- cornPlot+
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

cottonPPT <- cottonPlot+
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

peanutPPT <- peanutPlot+
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

soybeanPPT <- soybeanPlot+
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

potatoPPT <- potatoPlot+
  geom_sf(data = NC,fill=alpha("grey",0),color="black",size=.5)


####*****            TMIN     ******####

var <- 'TMIN'
  
  
  # Import Data
  df <- read.csv(paste0(here("data/Crops_Final_Figs/Crops_Final_Figs/Figs6and8"),"/",var,"CountyCorrelation.csv"))%>%
  mutate('Class' = ifelse(corr < -.5, "-0.71 : -0.50",
                          ifelse(corr < -.25,"< -0.25",
                                 ifelse(corr < -0.05,"< -0.05",
                                        ifelse(corr < 0.05,"< 0.05",
                                               ifelse(corr < .25,"< 0.25",
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

cornTMIN <- cornPlot+
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

cottonTMIN <- cottonPlot+
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

peanutTMIN <- peanutPlot+
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

soybeanTMIN <- soybeanPlot+
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

potatoTMIN <- potatoPlot+
  geom_sf(data = NC,fill=alpha("grey",0),color="black",size=.5)


####****    TMAX    ****####

var <- 'TMAX'

# Import Data
df <- read.csv(paste0(here("data/Crops_Final_Figs/Crops_Final_Figs/Figs6and8"),"/",var,"CountyCorrelation.csv"))%>%
  mutate('Class' = ifelse(corr < -.5, "-0.71 : -0.50",
                          ifelse(corr < -.25,"< -0.25",
                                 ifelse(corr < -0.05,"< -0.05",
                                        ifelse(corr < 0.05,"< 0.05",
                                               ifelse(corr < .25,"< 0.25",
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

cornTMAX <- cornPlot+
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

cottonTMAX <- cottonPlot+
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

peanutTMAX <- peanutPlot+
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

soybeanTMAX <- soybeanPlot+
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

potatoTMAX <- potatoPlot+
  geom_sf(data = NC,fill=alpha("grey",0),color="black",size=.5)



### BUILD LEGEND ###

x1 <- 1
x2 <- 2
y1 <- 1
y2 <- 3

outdf <- data.frame()

for(n in 1:8){
  bl <- c(x1,y1)
  tl <- c(x1,y2)
  tr <- c(x2,y2)
  br <- c(x2,y1)
  xy <- list(rbind(bl,tl,tr,br,bl))
  poly <- st_as_sf(st_sfc(st_polygon(xy)))
  geom <- poly$x
  out <- data.frame("Object"=n, "geometry" = geom)
  outdf <- rbind(outdf,out)
  y1 <- y1+2
  y2 <- y2+2
}

sfp <- st_as_sf(outdf)
sfp$class <- c("NO DATA","-0.71 : -0.50","< -0.50","< -0.05","< 0.05","< 0.25","< 0.50","0.50 : 0.67")

# Color Classification
cols <- c("NO DATA" = "#c4c4c0",
          "-0.71 : -0.50" = "#800505",
          "< -0.50" = "#bf1717",
          "< -0.05" = "#d95d5d",
          "< 0.05" = "#e0ce75",
          "< 0.25" = "#43c463",
          "< 0.50" = "#1d7d35",
          "0.50 : 0.67" = "#025216")

legend <- ggplot(sfp)+
  geom_sf(aes(fill=class))+
  scale_fill_manual(values = cols)+
  theme(legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background = element_blank())+
  annotate("text", x = 6, y = seq(2,16,2), label = sfp$class)+
  xlim(xmin = 0, xmax = 10)


##------- Create Composite Plote -------##
# Create layout for figure
lay <- rbind(c(1,4,7,10),
             c(2,5,8,10),
             c(3,6,9,NA))

tiff(here("figures/composite.tiff"), width = 20, height = 23, units = 'cm', res = 1000)
grid.arrange(cornPPT,cottonPPT,peanutPPT,soybeanPPT,potatoPPT,
             cornTMIN,cottonTMIN,peanutTMIN,soybeanTMIN,potatoTMIN,
             cornTMAX,cottonTMAX,peanutTMAX,soybeanTMAX,potatoTMAX,
             legend, layout_matrix = lay)
dev.off()