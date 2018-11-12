#Reference manual for rnoaa package
#https://cran.r-project.org/web/packages/rnoaa/rnoaa.pdf 

#Data type IDs found using the following URL:
#ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt 

#User input for lat and long position where they plan to garden
lat_long = c(40.771142, -111.848166)
lat_lon_df <- data.frame(id = "Station", latitude = lat_long[1], longitude = lat_long[2])
#Load in all station metadata from previously saved file
options(noaakey = "YzLzNDLCXIzUwfAsWljYgxvxmZPMHtIj")
library('rnoaa')
load("/Users/Zach/Documents/Hydroinformatics 7460/station_data.Rdata") 
#Retrieve the 10 closest weather station's metadata (distance in km)
closest_stations <- meteo_nearby_stations(lat_lon_df = lat_lon_df,
                                         station_data = station_data,
                                         radius = 12)
closest_stations <- as.data.frame(closest_stations)
lat <- as.vector(closest_stations$Station.latitude)
long <- as.vector(closest_stations$Station.longitude)
#Retrieve data from closest weather stations (evap in tenths of mm, temp in tenths of decC)
monitorIDs <- as.vector(closest_stations[[1]])
climate_data <- meteo_pull_monitors(monitorIDs,
                                    var = c('PRCP','EVAP'),
                                    date_min = '2017-01-01')
#Determine which stations have the most and least amount of available data
station_NAs <- vector(mode = "list", length = nrow(closest_stations))
for (i in 1:nrow(closest_stations)){
  df <- climate_data[climate_data$id == closest_stations[i,1],]
  id <- df$id[1]
  prcpNAs <- sum(is.na(df$prcp))
  evapNAs <- sum(is.na(df$evap))
  totalNAs <- prcpNAs + evapNAs
  if (totalNAs == 0){
    totalNAs <- NA
  }
  v <- c(id, prcpNAs, evapNAs, totalNAs)
  station_NAs[[i]] <- v
}
station_NAs = do.call(rbind, station_NAs)
colnames(station_NAs) <- c('id', 'prcpNAs', 'evapNAs', 'totalNAs')
print(station_NAs)
#Populate the climate data with this new information
newcols <- c('prcpNAs', 'evapNAs', 'MissingData')
closest_stations[, newcols] <- NA
for (j in 1:nrow(closest_stations)){
  closest_stations$prcpNAs[j] <- as.numeric(station_NAs[j,2])
  closest_stations$evapNAs[j] <- as.numeric(station_NAs[j,3])
  closest_stations$MissingData[j] <- as.numeric(station_NAs[j,4])
}
rm(i,j, df, prcpNAs, evapNAs, v, id, totalNAs)
#Create map showing user location relative to nearby weather stations
library("ggmap")
bbox <- make_bbox(long,lat,f=0.05)
map <- get_map(bbox,maptype="toner-lite",source="stamen")
mapPoints <- ggmap(map) + ggtitle("Nearby Weather Stations: 12km Radius") + 
  geom_point(aes(x = Station.longitude, y = Station.latitude, color = MissingData, size = Station.distance),
             data = closest_stations) + 
  scale_colour_gradient(low = "cyan", high = "purple", na.value = 'purple') +
  xlab("Longitude") + ylab("Latitude") + geom_point(aes(x = longitude, y = latitude),
                                                    color = 'red',
                                                    shape = 18,
                                                    size = 5,
                                                    data = lat_lon_df)
mapPoints
