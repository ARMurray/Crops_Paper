library(dplyr)
library(tidyr)
library(ggplot2)

# Import crop data

soybeans <- read.csv("D:/OneDrive/OneDrive - University of North Carolina at Chapel Hill/Documents/Spring 2019/Geog_803/USDA_DOWNLOADS/Soybeans_BU_ACRE_GA_NC_SC_VA_1943_2017.csv")
peanuts <- read.csv("D:/OneDrive/OneDrive - University of North Carolina at Chapel Hill/Documents/Spring 2019/Geog_803/USDA_DOWNLOADS/Peanuts_LBs_ACRE_GA_NC_SC_VA_1934_2017.csv")
sweetPotatoes <- read.csv("D:/OneDrive/OneDrive - University of North Carolina at Chapel Hill/Documents/Spring 2019/Geog_803/USDA_DOWNLOADS/SweetPotatoes_CWT_ACRE_NC_SC_VA_1925_2017.csv")
cotton <- read.csv("D:/OneDrive/OneDrive - University of North Carolina at Chapel Hill/Documents/Spring 2019/Geog_803/USDA_DOWNLOADS/UplandCotton_LBs_ACRE_GA_NC_SC_VA_1925_2017.csv")

# Fix peanut and cotton data so values are numbers and not factors (R is having trouble with commas for thousands)

peanuts$Value <- as.numeric(gsub(",","",peanuts$Value))
cotton$Value <- as.numeric(gsub(",","",cotton$Value))

# Soybean Plot
soybeanPlot <- ggplot(soybeans, aes(x=Year, y=Value, color=State))+
  geom_point()+
  labs(title = "Soybean Yields by County",y="Bushels per Acre")+
  theme(text = element_text(size = 14))

soybeanPlot

# Peanut Plot
peanutPlot <- ggplot(peanuts, aes(x=Year, y=Value, color=State))+
  geom_point()+
  labs(title = "Peanut Yields by County",y="Pounds per Acre")+
  theme(text = element_text(size = 14))

peanutPlot

# Sweet Potato Plot
sweetPlot <- ggplot(sweetPotatoes, aes(x=Year, y=Value, color=State))+
  geom_point()+
  labs(title = "Sweet Potato Yields by County", y = "Hundred Pounds per Acre")+
  theme(text = element_text(size = 14))

sweetPlot

#Cotton Plot
cottonPlot <- ggplot(cotton, aes(x=Year, y=Value, color=State))+
  geom_point()+
  labs(title = "Upland Cotton Yields by County", y = "Pounds per Acre")+
  theme(text = element_text(size = 14))

cottonPlot

# Hi Montana!
