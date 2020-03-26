library(doParallel)
library(tidyr)
library(raster)
library(dplyr)
library(sf)
library(here)

## First, we will parse through all of the precipitation files. We then import a shapefile of
## counties we are analyzing and project the counties to match the projection of the prism data.
## We then reaggregate the prism data to calculate average precipitation by county, for every day
## since the start of the analysis (January 1, 1981)

# Get list of Prism Precipitation files

list <- list.files(path = here("data/prism/ppt"), pattern = '.bil$', recursive = TRUE,full.names = TRUE)
names <- list.files(path = here("data/prism/ppt"), pattern = '.bil$', recursive = TRUE,full.names = FALSE)

# Import the shapefile of counties
US_Counties <- st_read(here("data/shapefiles/tl_2015_us_county.shp"))

# Subset the national counties to our study area
SE_Counties <- US_Counties%>%
  dplyr::filter(STATEFP %in% c("37","13","45"))

# Get CRS code table
#crs_data = rgdal::make_EPSG()
#View(crs_data)

# Make sure to check the projections, and if needed, 
# reproject Counties to the crs of the prism data 
# to perform spatial operations between them
Counties <- st_transform(SE_Counties,4269)


# ***Prism Parallel Processing***
# Here, we will use parrallel processing to reaggregate prism data to daily
# county averages.



# Create an output dataframe that the loop will write to.
outData <- data.frame("ID"=paste0(SE_Counties$STATEFP,SE_Counties$COUNTYFP))

# Ask R to detect how many cores your computer is running
acores <- detectCores()

# Set the number of cores you will use for parallel processing (DO NOT USE ALL OF THEM)
registerDoParallel(cores=10)

# Subsetting so it finishes before I run out of server time
names <- names[1:6]

# Create a start time to track processing speed
psTime <- Sys.time()

# Create an output dataframe that the loop will write to.
outData <- data.frame("ID"=paste0(SE_Counties$STATEFP,SE_Counties$COUNTYFP))

runs <- foreach(k=1:length(names),.combine=rbind,.packages=c("dplyr","raster","sf","tidyr")) %dopar% {
  filename <- list[k]
  prism <- raster(filename)
  date <- substr(names[k],60,67)
  
  #Calculate the mean ppt for each county
  rastVals <- raster::extract(prism, Counties)
  countyMeans <- lapply(rastVals, FUN=mean)
  newData <- data.frame("ppt_mean"=countyMeans)
  names(newData) <- c(1:NCOL(newData))
  newDataLong <- gather(newData)
  ppt <- newDataLong[2]
  names(ppt) <- date
  outData <- cbind(outData, ppt)
  longData <- gather(outData,Date,ppt,-ID)
}
peTime <- Sys.time()

write.csv(runs,here("data/pptAgg/ppt_long_1_50.csv"))

#Diagnostics
writeLines(c(paste0("Start Time: ",psTime),paste0("End Time: ", peTime), paste0("Available Cores: ", acores),
             paste0("Cores used: ", acores-10), paste0("Time: ", peTime - psTime), paste0("Iterations: ", length(list))),
           con = here("data/pptAgg/ppt_messages_1_50.txt"))

