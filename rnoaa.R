#Reference manual for rnoaa package
#https://cran.r-project.org/web/packages/rnoaa/rnoaa.pdf 

#Data type IDs found using the following URL:
#ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt 

#User input for lat and long position where they plan to garden
lat_long_el = c(40.622023, -111.812297, 1288.085)
lat_lon_df <- data.frame(id = "Station", latitude = lat_long_el[1], longitude = lat_long_el[2])
#Load in all station metadata from previously saved file
options(noaakey = "YzLzNDLCXIzUwfAsWljYgxvxmZPMHtIj")
library(rnoaa)
load("/Users/Zach/Documents/Hydroinformatics 7460/AguaLibre/station_data.Rdata") 
#Retrieve the 10 closest weather station's metadata (distance in km)
closest_stations <- meteo_nearby_stations(lat_lon_df = lat_lon_df,
                                          station_data = station_data,
                                          radius = 5)
closest_stations <- as.data.frame(closest_stations)
lat <- as.vector(closest_stations$Station.latitude)
long <- as.vector(closest_stations$Station.longitude)
#Retrieve data from closest weather stations (prcp in tenths of mm, temp in tenths of decC)
monitorIDs <- as.vector(closest_stations[[1]])
climate_data <- meteo_pull_monitors(monitorIDs,
                                    var = c('PRCP','TMIN','TMAX'),
                                    date_min = '2018-01-01')
#Determine which stations have the most and least amount of available data
station_NAs <- vector(mode = "list", length = nrow(closest_stations))
for (i in 1:nrow(closest_stations)){
  df <- climate_data[climate_data$id == closest_stations[i,1],]
  id <- df$id[1]
  prcpNAs <- sum(is.na(df$prcp))
  tminNAs <- sum(is.na(df$tmin))
  tmaxNAs <- sum(is.na(df$tmax))
  totalNAs <- prcpNAs + tminNAs + tmaxNAs
  if (totalNAs == 0){
    totalNAs <- NA
    id <- closest_stations[i,1]
  }
  v <- c(id, prcpNAs, tminNAs, tmaxNAs, totalNAs)
  station_NAs[[i]] <- v
}
station_NAs = do.call(rbind, station_NAs)
colnames(station_NAs) <- c('id', 'prcpNAs', 'tminNAs', 'tmaxNAs', 'totalNAs')
#Populate the climate data with this new information
newcols <- c('prcpNAs', 'tminNAs', 'tmaxNAs', 'MissingData')
closest_stations[, newcols] <- NA
for (j in 1:nrow(closest_stations)){
  closest_stations$prcpNAs[j] <- as.numeric(station_NAs[j,2])
  closest_stations$tminNAs[j] <- as.numeric(station_NAs[j,3])
  closest_stations$tmaxNAs[j] <- as.numeric(station_NAs[j,4])
  closest_stations$MissingData[j] <- as.numeric(station_NAs[j,5])
}
#Determine optimal station based on distance from user location and missing data
rankings <- cbind(closest_stations$Station.id, sapply(closest_stations[,c(5,9)], rank))
rankings <- as.data.frame(rankings)
rankings$Total <- NA
colnames(rankings) <- c('id', 'Distance','MissingData', 'Total')
for (k in 1:nrow(rankings)){
  rankings$Total[k] <- as.numeric(rankings$Distance[k]) + as.numeric(rankings$MissingData[k])
}
for (l in 1:nrow(rankings)){
  if (rankings$Total[l] == min(rankings$Total)){
    optimal_station <- as.character(rankings$id[l])
  }
}
rm(i,j,k,l,id,newcols,prcpNAs,tmaxNAs,tminNAs,totalNAs,v)
climate_data <- climate_data[climate_data$id == optimal_station,]
#Create map showing user location relative to nearby weather stations
library(ggmap)
bbox <- make_bbox(long,lat,f=0.05)
map <- get_map(bbox,maptype="toner-lite",source="stamen")
mapPoints <- ggmap(map) + ggtitle('Nearby Weather Stations: 5 km Radius
Optimal Weather Station:', optimal_station) + 
  geom_point(aes(x = Station.longitude, y = Station.latitude, color = MissingData, size = -Station.distance),
             data = closest_stations) + 
  scale_colour_gradient(low = "purple", high = "cyan", na.value = 'purple') +
  xlab("Longitude") + ylab("Latitude") + geom_point(aes(x = longitude, y = latitude),
                                                    color = 'red',
                                                    shape = 18,
                                                    size = 5,
                                                    data = lat_lon_df)
mapPoints
#Create plot of tmin and tmax at the optimal station
library(ggplot2)
ggplot(climate_data, aes(date)) + 
  geom_line(aes(y = tmin, colour = "tmin")) + 
  geom_line(aes(y = tmax, colour = "tmax")) +
  xlab("Date") + ylab("Temperature (tenths of Deg C)")
