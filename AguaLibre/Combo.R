# Start of 1st function
# Input
lat <- 40.762565
long <- -111.860121
# Determine period of analysis from today's current date (end_date) to 10 years ago (start_date) 
end_date <- as.character(Sys.Date())
dates <- as.Date(end_date)
end_year <- as.numeric(format.Date(dates, "%y"))
end_month <- as.numeric(format.Date(dates, "%m"))
end_day <- as.numeric(format.Date(dates, "%d"))
start_year <- 2000 + end_year - 10
start_date <- paste(start_year,'-',end_month,'-',end_day, sep = '')
# Start of 1st part for prcp data retrieval
lat_lon_df <- data.frame(id = "Station", latitude = lat, longitude = long)
#Load in all station metadata from previously saved file
options(noaakey = "YzLzNDLCXIzUwfAsWljYgxvxmZPMHtIj")
library(rnoaa)
load("data/utah_stations.Rdata") 
#Retrieve the closest weather station's metadata within a 10 km radius
closest_stations <- meteo_nearby_stations(lat_lon_df = lat_lon_df,
                                          station_data = utah_stations,
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
  # Divide by 10 to get average monthly prcp for the last 10 year period of record
  months[[i]] <- sum(months[[i]], na.rm = TRUE)/10
}
monthly_prcp <-  do.call(rbind, months)
#Convert from tenths of mm to inches
monthly_prcp <- ((monthly_prcp/10)/1000)*39.3701
# Create map showing user location relative to nearby weather stations
library(ggmap)
bbox <- make_bbox(long,lat,f=0.05)
map <- get_map(bbox,maptype="toner-lite",source="stamen")
mapPoints <- ggmap(map) + ggtitle('Nearby Weather Stations within 10 km Radius. Optimal Weather Station:', station_metadata$Station.name) + 
  geom_point(aes(x = Station.longitude, y = Station.latitude, color = AvailableData, size = Station.distance),
             data = closest_stations) + 
  scale_colour_gradient(low = "purple", high = "cyan", na.value = 'purple') +
  xlab("Longitude") + ylab("Latitude") + geom_point(aes(x = Station.longitude, y = Station.latitude, shape = Station.name),
                                                    color = 'red',
                                                    size = 3,
                                                    data = station_metadata)
mapPoints
ggsave("NearbyWeatherStations.jpg", width = 7, height = 5)
#pathname <- '/Users/Zach/Documents/Hydroinformatics 7460/AguaLibre'
localMap <- paste0('NearbyWeatherStations.jpg')
# End of 1st part
# Start of 2nd function
# Load new packages 
library(rjson)
library(jsonlite)
library(RCurl)
# Input
lat <- station_metadata[3]
long <- station_metadata[4]
start_date <- '2000-01-01'
# Save base and full url as variables
url_usu = "https://climate.usu.edu/API/api.php/v2/key=62PyzUjrCDYh0JB95faxrDcGB9tTss/evapotranspiration/average_monthly_sum"
full_url = paste0(url_usu,'/state=UT/network=ghcn','/lat=',lat,'/long=',long,'/start_date=',start_date,'/end_date=',end_date,'/units=e/month=(1,2,3,4,5,6,7,8,9,10,11,12)/buffer=10')
# Convert JSON to data frame
evap_data <- fromJSON(getURL(full_url))
# Break down list structure into numeric dataframe
evap_data <- do.call(rbind, evap_data[[2]])
evap_data <- evap_data[c(2:13),]
evap <- as.numeric(evap_data[,1])
mon <- as.numeric(evap_data[,2])
std <- as.numeric(evap_data[,3])
# Combine monthly prcp and evap data together in dataframe
climate_data <- data.frame('month' = mon, 'prcp_in' = monthly_prcp, 'evap_sum_in' = evap, 'evap_std' = std)

climate_plot <- data.frame('Collected_Rain_m3' = c(1,5,3,8,24,8,23,6,13,4,4,8), 'Water_Demand_m3' = c(5,1,7,2,3,7,3,4,8,4,2,5))
library(reshape2)
climate_plot <- melt(climate_plot)
mon <- rep(1:12,2)
climate_plot <- data.frame(mon,climate_plot)
colnames(climate_plot) <-  c('Month', 'Variable', 'Value_m3')
library(ggplot2)
ggplot() + geom_point(data = climate_plot, aes(x = climate_plot$Month, y = climate_plot$Value_m3, color = climate_plot$Variable)) + 
  scale_x_continuous(breaks=c(1:12)) + 
  labs(title = 'Rainwater Supply and Demand: 10 Year Historical Average', x = 'Month', y = 'Water Volume (m3)', color = "Legend Title\n") + 
  theme_set(theme_gray(base_size = 14))



