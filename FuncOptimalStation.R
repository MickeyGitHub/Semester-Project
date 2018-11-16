#This function requires a user input of latitude and longitude. The function works by
#finding the closest weather stations within a 5 km radius of the user input location.
#This is done by using the package 'rnoaa' anda pre downloaded file containing all NOAA 
#station data. The stations are ranked based on distance from the user location and amount
#of missing data. The data of interest is precipitation, min and max temeprature.
#An optimal station is determiend based on the sum of the two individual ranks.
#The function returns three seperate things: a dataframe consisting of the climate data from
#the optimal weather station, a dataframe consisting of the optimal station's metadata, and
#a stamenmaps plot of the local area illustrating all nearby weather stations,
#their relative distance from the user location, and the amount of missing data for each.

OptimalStation <- function(lat, long)
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
  #Retrieve data from closest weather stations (prcp in tenths of mm, temp in tenths of decC)
  monitorIDs <- as.vector(closest_stations[[1]])
  climate_data <- meteo_pull_monitors(monitorIDs,
                                      var = c('PRCP'),
                                      date_min = start_date)
  #Determine which stations have the most and least amount of available data
  station_data <- vector(mode = "list", length = nrow(closest_stations))
  for (i in 1:nrow(closest_stations)){
    df <- climate_data[climate_data$id == closest_stations[i,1],]
    id <- df$id[1]
    avail_prcp <- sum(df$prcp, na.rm = TRUE)
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
  rm(i,j,id,newcols,avail_prcp,dat,dist,tot,v,row_pos,station_data,df)
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
return(climate_data, station_metadata, localMap)


