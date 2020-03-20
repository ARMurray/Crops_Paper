library(prism)
library(raster)
library(dplyr)
library(sf)
library(tidyr)
library(ggplot2)

# *****THIS CHUNK IS TO DOWNLOAD AND PREPARE DATA AND SHOULD ONLY BE RUN ONCE******
#       *******************IT TAKES AT LEAST ONE HOUR***********************
# Download the data
#get_prism_annual('ppt', years = 1987:2017)



#Download ppt
#options(prism.path = "/proj/diegorilab/users/Andrew/Crops_Paper/data/prism/ppt")
#get_prism_dailys('ppt',minDate = "1981-01-01",maxDate = "2018-12-31" ,keepZip = FALSE)

# Download TMin
options(prism.path = "/proj/diegorilab/users/Andrew/Crops_Paper/data/prism/tmin")
get_prism_dailys('tmin',minDate = "1995-12-09",maxDate = "2018-12-31" ,keepZip = FALSE)

# Download TMax
options(prism.path = "/proj/diegorilab/users/Andrew/Crops_Paper/data/prism/tmax")
get_prism_dailys('tmax',minDate = "1981-01-01",maxDate = "2018-12-31" ,keepZip = FALSE)




# Get list of Prism files

#list <- paste0("~/Documents/Geog_803/prism/",list.files(path = "~/Documents/Geog_803/prism/", pattern = '.bil$', recursive = TRUE))

#US_Counties <- st_read("~/Documents/Geog_803/shapefiles/US_county_2010.shp")

#SE_Counties <- US_Counties%>%
#  dplyr::filter(STATEFP10 == "37"| STATEFP10 == "13"| STATEFP10 == "45"| STATEFP10 == "51")

# Get CRS code table
#crs_data = rgdal::make_EPSG()
#View(crs_data)

# Set Coordinate Reference System (NAD83 CONUS Albers - meters)
#st_set_crs(SE_Counties,5070)

# Reproject Counties to the crs of the prism data 
# to perform spatial operations between them
#Counties <- st_transform(SE_Counties,4269)

#outData <- data.frame("County"=SE_Counties$NAMELSAD10,"StateFips"=SE_Counties$STATEFP10,"CT_Fips"=SE_Counties$COUNTYFP10)

# For loop to extract all of the avg ppt by county ... This took over an hour and the csv was already created
# Skip this step and import the csv below
#for(n in 1:length(list)){
#  filename <- list[n]
#  prism <- raster(filename)
#  year <- substr(list[n],51,54)
#  object <- paste0("prism_ppt_",year)
  
  #Calculate the mean ppt for each county
#  rastVals <- raster::extract(prism, Counties)
#  countyMeans <- lapply(rastVals, FUN=mean)
#  newData <- data.frame("ppt_mean"=countyMeans)
#  newDataLong <- gather(newData)
#  outData <- cbind(outData, newDataLong$value)
  
#}
#*********************************************************************
#************BEGIN HERE TO FORMAT TABLES******************

US_Counties <- st_read("~/Documents/Geog_803/shapefiles/US_county_2010.shp")

SE_Counties <- US_Counties%>%
  dplyr::filter(STATEFP10 == "37"| STATEFP10 == "13"| STATEFP10 == "45"| STATEFP10 == "51")

#import previously formatted prism data
outData <- read.csv("INSERT PATH TO COUNTY ppt Totals csv HERE")
colnames(outData) <- c("OID","County","ST_Fips","CO_Fips",1987:2017)

# Add leading zeros to county numbers
for(r in 1:nrow(outData)){
  row <- outData[r,]
  if(nchar(row$CO_Fips) == 2){
    newrecord <- paste0("0",row$CO_Fips)
    outData$CO_Fips[r] <- newrecord
  }
  else if(nchar(row$CO_Fips) == 1){
    newrecord <- paste0("00",row$CO_Fips)
    outData$CO_Fips[r] <- newrecord
  }
}

# Create a join key by combining state and county fips codes
outData$key <- paste0(outData$ST_Fips,outData$CO_Fips)
# Use gather to change from wide to long format
table <- outData%>%
  select(-OID,-County,-ST_Fips,-CO_Fips)%>%
  gather(year, ppt, -key )


# Pick variables to add to table output
vars <- SE_Counties%>%
  dplyr::select(STATEFP10,COUNTYFP10,GISJOIN,NAME10)
vars$key <- paste0(vars$STATEFP10,vars$COUNTYFP10)

# Join the variables to the ppt data
Counties <- as.data.frame(SE_Counties)
Counties$ST_CO <- paste0(Counties$STATEFP10,Counties$COUNTYFP10)
table <- dplyr::left_join(table, vars, by = 'key')

# Write Table
write.csv(table, "~/Documents/Geog_803/County_ppt_Avgs_1987_2017.csv")

# Make a plot for total ppt per county by year  
ggplot(table, aes(x=year,y=ppt,color=STATEFP10))+
  geom_point()+
  labs(title = "Total Precipitation by County 1987-2017", x = "",y="Total ppt in mm",color="State")+
  scale_color_manual(labels = c("Georgia","North Carolina","South Carolina","Virginia"), values = c("green","#4B9CD3","purple","yellow"))
  

