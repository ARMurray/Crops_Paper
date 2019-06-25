install.packages('sf')
library(doParallel)
library(raster)
library(dplyr)
library(sf)


# Get list of Prism files
list <- paste0("~/files/",list.files(path = "~/files/", pattern = '.bil$', recursive = TRUE))

#Subset for testing
list <- list[120:152]

US_Counties <- st_read("~/files/shapefiles/tl_2015_us_county.shp")

SE_Counties <- US_Counties%>%
 dplyr::filter(STATEFP == "37"| STATEFP == "13"| STATEFP == "45"| STATEFP == "51")

# Set Coordinate Reference System (NAD83 CONUS Albers - meters)
st_set_crs(SE_Counties,4269)

# Reproject Counties to the crs of the prism data 
# to perform spatial operations between them
Counties <- st_transform(SE_Counties,4269)

outData <- data.frame("ID"=paste0(SE_Counties$STATEFP,SE_Counties$COUNTYFP))

# For loop to extract all of the avg ppt by county ... This took over an hour and the csv was already created
# Skip this step and import the csv below
sTime <- Sys.time()
for(n in 1:length(list)){
 filename <- list[n]
 prism <- raster(filename)
 date <- substr(list[n],41,48)
 object <- paste0("prism_ppt_",year)
 
 #Calculate the mean ppt for each county
 rastVals <- raster::extract(prism, Counties)
 countyMeans <- lapply(rastVals, FUN=mean)
 newData <- data.frame("ppt_mean"=countyMeans)
 newDataLong <- gather(newData)
 ppt <- newDataLong[2]
 names(ppt) <- date
 outData <- cbind(outData, ppt)
 longData <- gather(outData,Date,ppt,-ID)
}  
eTime <- Sys.time()

# Results
# Ran 5 iterations
# Total time: 10.7492minutes
# Time per iteration: 2.14984 minutes
# Est. time for entire dataset (n = 13879): 29,837.63 minutes / 497.2938 hours / 20.72058 days

# ***Prism Parallel Processing***
psTime <- Sys.time()
library(doParallel)

outData <- data.frame("ID"=paste0(SE_Counties$STATEFP,SE_Counties$COUNTYFP))

registerDoParallel(cores=32)
runs <- foreach(k=1:length(list),.combine=rbind,.packages=c("dplyr","raster","sf","tidyr")) %dopar% {
  filename <- list[k]
  prism <- raster(filename)
  date <- substr(list[k],41,48)
  
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

write.csv(longData,"~/outputs/ppt_data.csv")


#Diagnostics

tpi <- length(list)/(as.double(peTime)-as.double(psTime))
acores <- detectCores()
writeLines(c(paste0("Start Time: ",psTime),paste0("End Time: ", peTime), paste0("Available Cores: ", acores),
             paste0("Cores used: ", "32"), paste0("Time per Iteration: ", tpi," Minutes")),
           con = "~/outputs/ppt_messages.txt")

