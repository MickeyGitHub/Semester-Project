#Reference manual for rnoaa package
#https://cran.r-project.org/web/packages/rnoaa/rnoaa.pdf 

#Data type IDs found using the following URL:
#ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt 

#Start
SLUT <- c(40.762428, -111.860265)
lat <- SLUT[1]
long <- SLUT[2]
start_date <- '2017-01-01'
end_date <- '2018-01-01'
lat_lon_df <- data.frame(id = "Station", latitude = lat, longitude = long)
#Load in all station metadata from previously saved file
options(noaakey = "YzLzNDLCXIzUwfAsWljYgxvxmZPMHtIj")
library(rnoaa)
load("/Users/Zach/Documents/Hydroinformatics 7460/AguaLibre/station_data.Rdata") 
#Retrieve the 10 closest weather station's metadata (distance in km)
closest_stations <- meteo_nearby_stations(lat_lon_df = lat_lon_df,
                                          station_data = station_data,
                                          radius = 10)
closest_stations <- as.data.frame(closest_stations)
lat <- as.vector(closest_stations$Station.latitude)
long <- as.vector(closest_stations$Station.longitude)
#Retrieve data from closest weather stations (prcp in tenths of mm)
monitorIDs <- as.vector(closest_stations[[1]])
climate_data <- meteo_pull_monitors(monitorIDs,
                                    var = c('PRCP'),
                                    date_min = start_date,
                                    date_max = end_date)
#Determine which stations have the most and least amount of available data
station_data <- vector(mode = "list", length = nrow(closest_stations))
for (i in 1:nrow(closest_stations)){
  df <- climate_data[climate_data$id == closest_stations[i,1],]
  id <- df$id[1]
  avail_prcp <- nrow(df)
  if (avail_prcp == 0){
    avail_prcp <- NA
    id <- closest_stations[i,1]
  }
  v <- c(id, avail_prcp)
  station_data[[i]] <- v
}
station_data <-  do.call(rbind, station_data)
colnames(station_data) <- c('id', 'avail_prcp')
#Populate the closest stations data with this new information
newcols <- c('AvailableData')
closest_stations[, newcols] <- NA
for (j in 1:nrow(closest_stations)){
  closest_stations$AvailableData[j] <- as.numeric(station_data[j,2])
}
#Determine optimal station based on distance from user location and available data
closest_stations <- closest_stations[!is.na(closest_stations$AvailableData),]
dist <- rank(closest_stations$Station.distance)
dat <- rank(-closest_stations$AvailableData)
tot <- dist + dat
rankings <- cbind(closest_stations$Station.id, closest_stations$Station.name, dist, dat, tot)
colnames(rankings) <- c('id', 'Name', 'Distance','AvailableData', 'Total')
row_pos <- which.min(rankings[,5])
optimal_station <- rankings[row_pos,1]
optimal_data <- climate_data[climate_data$id == optimal_station,]
optimal_data$SiteName <- id
station_metadata <- closest_stations[which(closest_stations$Station.id == optimal_station),]
prcp_data <- optimal_data
rm(i,j,id,newcols,avail_prcp,dat,dist,tot,v,row_pos,station_data,df)
# Determine monthly precp 
values <- prcp_data$prcp
dates <- prcp_data$date
dates <- as.Date.factor(dates)
dates <- format.Date(dates, "%m")
dates <- as.numeric(dates)
months <- vector(mode = "list", length = 12)
for (i in 1:12){
  a <- months[[i]]
  for (k in 1:length(values)){
    if (dates[k] == i){
      a[k] <- values[k]
    }
  }
  months[[i]] <- a
  months[[i]] <- sum(months[[i]], na.rm = TRUE)
}
monthly_prcp <-  do.call(rbind, months)
monthly_prcp <- monthly_prcp*10
#Create map showing user location relative to nearby weather stations
library(ggmap)
bbox <- make_bbox(long,lat,f=0.05)
map <- get_map(bbox,maptype="toner-lite",source="stamen")
mapPoints <- ggmap(map) + ggtitle('Nearby Weather Stations: 10 km Radius, Optimal Weather Station:', station_metadata$Station.name) + 
  geom_point(aes(x = Station.longitude, y = Station.latitude, color = AvailableData, size = Station.distance),
             data = closest_stations) + 
  scale_colour_gradient(low = "purple", high = "cyan", na.value = 'purple') +
  xlab("Longitude") + ylab("Latitude") + geom_point(aes(x = Station.longitude, y = Station.latitude),
                                                    color = 'red',
                                                    shape = 18,
                                                    size = 3,
                                                    data = station_metadata)
mapPoints
ggsave('NearbyWeatherStations.png', width = 7, height = 5)
pathname <- '/Users/Zach/Documents/Hydroinformatics 7460/AguaLibre'
localMap <- paste0(pathname,'/NearbyWeatherStations.png')
#End

