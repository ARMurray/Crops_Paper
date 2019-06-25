mirror <- "http://archive.linux.duke.edu/cran/"
#install.packages('Rcpp', repo = mirror)
#install.packages('sf', repo = mirror)
#install.packages('raster', repo = mirror)
library(doParallel)
library(raster)
library(dplyr)
library(sf)


# Get list of Prism files
list <- paste0("/pine/scr/a/r/armurray/files/files/",list.files(path = "/pine/scr/a/r/armurray/files/files/", pattern = '.bil$', recursive = TRUE))

#Subset for testing
#list <- list[1:2500]

US_Counties <- st_read("/pine/scr/a/r/armurray/tl_2015_us_county.shp")

SE_Counties <- US_Counties%>%
  dplyr::filter(STATEFP == "37"| STATEFP == "13"| STATEFP == "45"| STATEFP == "51")
SE_Counties <- st_as_sf(SE_Counties)
# Set Coordinate Reference System (NAD83 CONUS Albers - meters)
st_set_crs(SE_Counties,4269)

# Reproject Counties to the crs of the prism data 
# to perform spatial operations between them
Counties <- st_transform(SE_Counties,4269)


# ***Prism Parallel Processing***
psTime <- Sys.time()

outData <- data.frame("ID"=paste0(SE_Counties$STATEFP,SE_Counties$COUNTYFP))

acores <- detectCores()

registerDoParallel(cores=acores)
runs <- foreach(k=1:length(list),.combine=rbind,.packages=c("dplyr","raster","sf","tidyr")) %dopar% {
  filename <- list[k]
  prism <- raster(filename)
  date <- substr(list[k],68,75)
  
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

write.csv(runs,"/pine/scr/a/r/armurray/outputs/ppt_data.csv")


#Diagnostics


writeLines(c(paste0("Start Time: ",psTime),paste0("End Time: ", peTime), paste0("Available Cores: ", acores),
             paste0("Cores used: ", acores), paste0("Time: ", peTime - psTime), paste0("Iterations: ", length(list))),
           con = "/pine/scr/a/r/armurray/outputs/ppt_messages.txt")

