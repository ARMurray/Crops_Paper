library(ggplot2)
library(dplyr)
library(stringr)
setwd("D:/OneDrive/OneDrive - University of North Carolina at Chapel Hill/Documents/Spring 2019/Geog_803/Analysis/")

corn <- read.csv("cornPlanted.csv")
cotton <- read.csv("cottonPlanted.csv")
peanuts <- read.csv("peanutsPlanted.csv")
soybeans <- read.csv("soybeansPlanted.csv")
sweets <- read.csv("sweetsPlanted.csv")

df <- rbind(corn,cotton,peanuts,soybeans,sweets)%>%
  filter(!is.na(County.ANSI))%>%
  select(State.ANSI,County.ANSI,County,State,Year,Commodity,Value)

#Put leading zeros back in
df$County.ANSI <- str_pad(df$County.ANSI,3,pad = "0")
df$Fips <- paste0(df$State.ANSI,df$County.ANSI)
df <- df%>%
  select(Fips,County, State, Year, Commodity, Value)
colnames(df)[6] <- "AcresPlanted"
df$AcresPlanted <- as.numeric(gsub(",","",df$AcresPlanted))

write.csv(df, "apps/cropsPlanted/cropsPlanted/cropsPlanted.csv", row.names = FALSE)
