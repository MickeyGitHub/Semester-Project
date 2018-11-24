#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#EXAMPLE for calculating with user-input values
#https://stackoverflow.com/questions/40997817/reactive-variables-in-shiny-for-later-calculations


######GLOBAL####GLOBAL####GLOBAL####GLOBAL####GLOBAL####GLOBAL####GLOBAL####GLOBAL####GLOBAL####GLOBAL####
######GLOBAL####GLOBAL####GLOBAL####GLOBAL####GLOBAL####GLOBAL####GLOBAL####GLOBAL####GLOBAL####GLOBAL####
######GLOBAL####GLOBAL####GLOBAL####GLOBAL####GLOBAL####GLOBAL####GLOBAL####GLOBAL####GLOBAL####GLOBAL####

library(shiny)
library(leaflet)
library(rnoaa)
library(ggmap)
library(ggplot2)
library(dplyr)
library(rjson)
library(jsonlite)
library(RCurl)
library(scales)

crops <- read.csv('data/crops.csv',header=TRUE)

OptimalStation <- function(lat, long, Roof, roofeff, veggies){
  start_date <- '2017-01-01'
  end_date <- '2018-01-01'
  # Start of 1st function
  lat_lon_df <- data.frame(id = "Station", latitude = lat, longitude = long)
  #Load in all station metadata from previously saved file
  options(noaakey = "YzLzNDLCXIzUwfAsWljYgxvxmZPMHtIj")
  library(rnoaa)
  load("utah_stations.Rdata") 
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
    months[[i]] <- sum(months[[i]], na.rm = TRUE)
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
  pathname <- '/Users/Zach/Documents/Hydroinformatics 7460/AguaLibre'
  localMap <- paste0(pathname,'/NearbyWeatherStations.jpg')
  # End of 1st function
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
  mon <- c(1:12)
  mon_char <- c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec")
  std <- as.numeric(evap_data[,3])
  
  ######Addition######Addition######Addition######Addition######Addition######Addition######
  ######Addition######Addition######Addition######Addition######Addition######Addition######
  ######Addition######Addition######Addition######Addition######Addition######Addition######
  
  #dimension effective precipitation dataframe
  precip_effective <- rep(NA,12)
  
  #for loop to calculate effective precipitation according to emperical FAO calculations
  for (i in 1:nrow(monthly_prcp)) {
    if (monthly_prcp[i] >= 2.95276){
      precip_effective[i] <- ((monthly_prcp[i]*0.8)-0.984252)
    }
    else (precip_effective[i] <- ((monthly_prcp[i]*0.6)-0.393701))
    
  }
  precip_effective[precip_effective<0] <- 0
  
  #Calculating the monthly collected rainfall in cubic meters
  
  #dimension effective precipitation dataframe
  Collected_Precip <- rep(NA,12)
  Stored_Water <- rep(NA,12)
  
  #acre-feet to cubic meters
  conversion <- 1233.48
  
  for (i in 1:12) {
    Collected_Precip[i] <- ((monthly_prcp[i]/12)*conversion*Roof*(roofeff/100))
  }
  
  Stored_Water[1] <- Collected_Precip[1]
  for (i in 2:12) {
    Stored_Water[i] <- (Collected_Precip[i] + Stored_Water[i-1])
  }
  
  # determine mean crop coefficient based on user selection of crops
  crop <- as.character(crops$crop)
  coeff <- crops$coefficient
  crop_coeff <- rep(NA,length(veggies))
  for (i in veggies){
    row_pos <- which(crop == i)
    crop_coeff[i] <- coeff[row_pos]
  }
  crop_coeff <- na.omit(crop_coeff)
  crop_coeff <- as.numeric(crop_coeff)
  crop_coeff <- mean(crop_coeff)
  
  #Chosen_Crop_K <- rep(NA,length(crops))
  #Portion_Of_Crop <- rep(NA,length(crops))
  #Coeff_Factor <- rep(NA,length(crops))
  
  # Combine monthly prcp and evap data together in dataframe
  climate_data <- data.frame('index' = mon, 'Month' = mon_char, 'Precipitation_in' = monthly_prcp,
                             'Effective_Precip' = precip_effective,
                             'Collected_Rain_m3' = Collected_Precip, 
                             'Cumulative_Rain_m3' = Stored_Water, 
                             'evap_sum_in' = evap, 
                             'evap_std' = std)
  
  ######Addition######Addition######Addition######Addition######Addition######Addition######
  ######Addition######Addition######Addition######Addition######Addition######Addition######
  ######Addition######Addition######Addition######Addition######Addition######Addition######
  
  output_data <- list(climate_data, localMap)
  return(output_data)
}

##ENDglobal##ENDglobal##ENDglobal##ENDglobal##ENDglobal##ENDglobal##ENDglobal##ENDglobal##ENDglobal##ENDglobal

######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui
######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui
######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui

ui <- fluidPage(
  # Application title
  titlePanel("AQUA LIBRE:
             Rainwater Collection and Garden Irrigation Demand"),
  actionButton("execute", "Get Your Data: run time is less than 1 minute"),
  sidebarLayout(
    sidebarPanel(
      numericInput("Lat","Enter Latitude of Site",
                   value = 40.767011, min = NA, max = NA, step = 0.000001, width = NULL)
    ),
    sidebarPanel(
      numericInput("Long","Enter Longitude of Site",
                   value = , -111.846033, min = NA, max = NA, step = 0.000001, width = NULL)
    )
  ),
  # Sidebar with a slider input for number of bins 
  sidebarPanel(
    numericInput("Garden_Area",
                 "Input Garden Area in Acres:",
                 value = 0.5, min = NA, max = NA, step = 0.001, width = NULL
    )
  ),
  sidebarPanel(
    numericInput ("Roof_Area",
                  "Input Roof Area in Acres:",
                  value = 0.8, min = NA, max = NA, step = 0.001, width = NULL
    )
  ),
  
  mainPanel( 
    
    
    #Text explaining how to find areas using the map
    h6("Use the 'Create 
       New Measurement' tool in the upper right side of the map to trace
       the outside of your garden. Input the area value into the 'Garden Area' box.
       Repeat the process for measuring the roof area used for rainwater collection.
       Input into 'Roof Area' box."),
    
    leafletOutput("map1"),
    tableOutput("table1"),
    plotOutput("plot1"),
    imageOutput("map2")
    
    ),
  sidebarLayout(
    sidebarPanel(
      
      #making a selection box widget for cops
      selectInput("Chosen_Crops", label = h4("Choose Your Crops"), 
                  choices = unique(crops$crop), 
                  selected = "Carrots",
                  multiple = TRUE,
                  selectize = TRUE,
                  width = NULL,
                  size = NULL),
      
      # Making an Irrigation Efficiency Widget 
      sliderInput("Irrigation_Eff", label = h4("Choose Your Irrigation Efficiency"), min = 0, 
                  max = 100, value = 75),
      
      h6("Common Efficiencies: Drip Irrigation 90%. Sprinklers 75%"),
      
      # Making a Rooftop Efficiency Widget 
      sliderInput("Roof_Eff", label = h4("Choose Your Rainwater Collection Efficiency"), min = 0, 
                  max = 100, value = 75),
      
      h6("Common Efficiencies: 90% for most roofs. New metal roofs up to 95%.")
      
    ),
    mainPanel()
  )
  )

######ENDui######ENDui######ENDui######ENDui######ENDui######ENDui######ENDui######ENDui######ENDui######

######server######server######server######server######server######server######server######server######
######server######server######server######server######server######server######server######server######
######server######server######server######server######server######server######server######server######

server <- function(input, output, session) {
  
  # Render interactive map for user to draw garden and roof areas
  output$map1 <-renderLeaflet({
    m<-leaflet() %>%
      addProviderTiles('Esri.WorldImagery') %>%
      addMarkers(lng = input$Long, lat = input$Lat, popup="Your Site") %>%
      setView(lng = input$Long, lat = input$Lat, zoom = 16 )  %>%
      addMeasure()
  })
  
  # Action button linked to latitude and longitude user inputs that are linked to data
  # retrieval functions
  coords <- reactiveValues(data = NULL)
  
  observeEvent(input$execute, {
    coords$lat <- input$Lat
  })
  
  observeEvent(input$execute, {
    coords$long <- input$Long
  }) 
  
  ######Addition######Addition######Addition######Addition######Addition######Addition######
  ######Addition######Addition######Addition######Addition######Addition######Addition######
  ######Addition######Addition######Addition######Addition######Addition######Addition######
  
  observeEvent(input$execute, {
    coords$Roof <- input$Roof_Area
  }) 
  
  observeEvent(input$execute, {
    coords$roofeff <- input$Roof_Eff
  }) 
  
  observeEvent(input$execute, {
    coords$wantedcrops <- input$Chosen_Crops
  }) 
  
  # Call function OptimalStation and store it as a reactive value
  myReactives <- reactiveValues()
  observeEvent(input$execute, {
    myReactives$data <-  OptimalStation(lat = coords$lat, long = coords$long,
                                        Roof = coords$Roof,  roofeff = coords$roofeff,
                                        veggies = coords$wantedcrops)
  })
  
  # Render table of monthly precip and evapotranspiration data from optimal station
  output$table1 <- renderTable({
    climate_data <- myReactives$data[[1]]
  })
  
  #Plotting outputs  
  output$plot1 <- renderPlot({
    climate_data <- myReactives$data[[1]]
    ggplot(data = climate_data, aes(x = climate_data$index, y = climate_data$Cumulative_Rain_m3)) + geom_line(color='red') + xlab("Month") + ylab("Cumulative Rainfall (m3)") + scale_x_continuous(breaks=c(1:12))
  })
  
  # Render map image showing user location relative to nearby weather stations
  output$map2 <- renderImage({
    localMap <- myReactives$data[[2]]
    list(src = localMap, contentType = 'image/jpg', width = 800, height = 700)
  })
  
  ######Addition######Addition######Addition######Addition######Addition######Addition######
  ######Addition######Addition######Addition######Addition######Addition######Addition######
  ######Addition######Addition######Addition######Addition######Addition######Addition######
  
}

shinyApp(ui = ui, server = server)

