library(ggplot2)
library(ggpubr)
library(ggsn)
library(dplyr)
library(sf)
library(here)

df <- read.csv(here("data/CountyMeans.csv"))%>%
  mutate('FIPS' = as.factor(FIPS))

sf <- st_read(here("data/shapefiles/tl_2015_us_county.shp"))%>%
  filter(STATEFP %in% c('37','45','13'))%>%
  left_join(df, by = c("GEOID"="FIPS"))%>%
  select(GEOID,NAME,Average_Max_Temperature_C,Average_Min_Temperature_C,Average_Precipitation_mm)

colnames(sf) <- c("GEOID","Name","Max_T","Min_T","Mean_ppt","geometry")

#######################
# Maximum Temperature #
#######################

# Calculate equal interval

min <- min(sf$Max_T)
max <- max(sf$Max_T)
nclass <- 5
increase <- (max-min)/nclass

i <- min
for(n in 1:6){
  print(i)
  i <- i+increase
}

# Classify and map max temp
maxT <- sf%>%
  mutate(class = ifelse(Max_T < 23.65,"21.5 - 23.6",
                        ifelse(Max_T < 25.65, "23.7 - 25.6",
                               ifelse(Max_T < 27.65, "25.7 - 27.6",
                                      ifelse(Max_T < 29.65, "27.7 - 29.6",
                                             ifelse(Max_T < 32, "29.7 - 31.7"))))))%>%
  ggplot()+
  geom_sf(aes(fill = class))+
  scale_fill_manual(values = c("21.5 - 23.6" = "#fef0d9","23.7 - 25.6" = "#fdcc8a","25.7 - 27.6" = "#fc8d59","27.7 - 29.6" = "#e34a33", "29.7 - 31.7" = "#b30000"),
                    name="Average Maximum \n Temperature (°C)")+
  labs(x="",y="")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  blank() +
  north(sf, location = "topleft", scale = .1, symbol = 12) +
  scalebar(sf, dist = 200, dist_unit = "km",
           transform = TRUE, model = "WGS84",height = .05, st.size = 3,
           st.bottom = FALSE,st.dist = .05, location = "bottomright")



#maxT

#######################
# Minimum Temperature #
#######################

# Calculate equal interval

min <- min(sf$Min_T)
max <- max(sf$Min_T)
nclass <- 5
increase <- (max-min)/nclass

i <- min
for(n in 1:6){
  print(i)
  i <- i+increase
}


minT <- sf%>%
  mutate(class = ifelse(Min_T < 12.5,"10.5 - 12.5",
                        ifelse(Min_T < 14.45, "12.6 - 14.4",
                               ifelse(Min_T < 16.4, "14.5 - 16.4",
                                      ifelse(Min_T < 18.4, "16.5 - 18.4",
                                             ifelse(Min_T < 22, "18.5 - 20.4"))))))%>%
  ggplot()+
  geom_sf(aes(fill = class))+
  scale_fill_manual(values = c("10.5 - 12.5" = "#0868ac","12.6 - 14.4" = "#43a2ca","14.5 - 16.4" = "#7bccc4","16.5 - 18.4" = "#bae4bc", "18.5 - 20.4" = "#f0f9e8"),
                    name="Average Minimum \n Temperature (°C)")+
  labs(x="",y="")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  blank() +
  north(sf, location = "topleft", scale = .1, symbol = 12) +
  scalebar(sf, dist = 200, dist_unit = "km",
           transform = TRUE, model = "WGS84",height = .05, st.size = 3,
           st.bottom = FALSE,st.dist = .05, location = "bottomright")


#minT


############
# Mean PPT #
############

# Calculate equal interval

min <- min(sf$Mean_ppt)
max <- max(sf$Mean_ppt)
nclass <- 5
increase <- (max-min)/nclass

i <- min
for(n in 1:6){
  print(i)
  i <- i+increase
}

meanPpt <- sf%>%
  mutate(class = ifelse(Mean_ppt < 621.5,"538 - 621",
                        ifelse(Mean_ppt < 704.5, "622 - 704",
                               ifelse(Mean_ppt < 786.5, "705 - 786",
                                      ifelse(Mean_ppt < 869.5, "787 - 869",
                                             ifelse(Mean_ppt < max+1, "870 - 952",NA))))))%>%
  ggplot()+
  geom_sf(aes(fill = class))+
  scale_fill_manual(values = c("538 - 621" = "#FFFFA1","622 - 704" = "#94EF62","705 - 786" = "#6DC98D","787 - 869" = "#5992B5", "870 - 952" = "#2F528F"),
                    name="    Average \n Precipitation (mm)")+
  labs(x="",y="")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  blank() +
  north(sf, location = "topleft", scale = .1, symbol = 12) +
  scalebar(sf, dist = 200, dist_unit = "km",
           transform = TRUE, model = "WGS84",height = .05, st.size = 3,
           st.bottom = FALSE,st.dist = .05, location = "bottomright")


#meanPpt



# Composite

fig <- ggarrange(maxT,minT,meanPpt, nrow = 1)

fig


