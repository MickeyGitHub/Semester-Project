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
#Retrieve the  10 closest weather station's metadat based on the user input lat long location
closest_stations <- meteo_nearby_stations(lat_lon_df = lat_lon_df,
                                         station_data = station_data,
                                         limit = 10)
closest_stations <- as.data.frame(closest_stations)
lat <- as.vector(closest_stations$Station.latitude)
long <- as.vector(closest_stations$Station.longitude)
#Retrieve data from the 5 closest weather stations
monitorIDs <- as.vector(closest_stations[[1]])
climate_data <- meteo_pull_monitors(monitorIDs, var = c('PRCP','EVAP','TMAX','TMIN','TAVG'))
#Create map showing user location relative to nearby weather stations
library("ggmap")
bbox <- make_bbox(long,lat,f=0.05)
map <- get_map(bbox,maptype="toner-lite",source="stamen")
mapPoints <- ggmap(map) + ggtitle("Nearby Weather Stations") + 
  geom_point(aes(x = Station.longitude, y = Station.latitude, size = Station.distance),
             color = 'red',
             data = closest_stations) + 
  xlab("Longitude") + ylab("Latitude") + geom_point(aes(x = longitude, y = latitude),
                                                    color = 'blue',
                                                    shape = 18,
                                                    size = 3,
                                                    data = lat_lon_df)
mapPoints

