#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
library(reshape2)
library(plotly)

load("data/crops.Rdata") 
load("data/utah_stations.Rdata")

OptimalStation <- function(lat, long){
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
  mapPoints <- ggmap(map) + ggtitle('Nearby Weather Stations: 10 km Radius') + 
    geom_point(aes(x = Station.longitude, y = Station.latitude, color = AvailableData, size = Station.distance),
               data = closest_stations) + 
    scale_colour_gradient(low = "purple", high = "cyan", na.value = 'purple') +
    xlab("Longitude") + ylab("Latitude") + geom_point(aes(x = Station.longitude, y = Station.latitude, shape = Station.name),
                                                      color = 'red',
                                                      size = 3,
                                                      data = station_metadata)
  mapPoints
  ggsave("data/NearbyWeatherStations.jpg", width = 7, height = 5)
  localMap <- 'data/NearbyWeatherStations.jpg'
  # End of 1st part
  # Start of 2nd part for evap data retreival 
  # Load new packages 
  library(rjson)
  library(jsonlite)
  library(RCurl)
  # Input
  lat <- station_metadata[3]
  long <- station_metadata[4]
  # Save base and full url as variables
  url_usu <-  "https://climate.usu.edu/API/api.php/v2/key=62PyzUjrCDYh0JB95faxrDcGB9tTss/evapotranspiration/average_monthly_sum"
  full_url <-  paste0(url_usu,'/state=UT/network=ghcn','/lat=',lat,'/long=',long,'/start_date=',start_date,'/end_date=',end_date,'/units=e/month=(1,2,3,4,5,6,7,8,9,10,11,12)/buffer=10')
  # Convert JSON to data frame
  evap_data <- fromJSON(getURL(full_url))
  # Break down list structure into numeric dataframe
  evap_data <- do.call(rbind, evap_data[[2]])
  evap_data <- evap_data[c(2:13),]
  monthly_evap <- as.numeric(evap_data[,1])
  
  # Combine monthly prcp and evap data together in dataframe
  climate_data <- data.frame('Evap_in' = monthly_evap,
                             'Prcp_in' = monthly_prcp)
  
  output_data <- list(climate_data, localMap)
  return(output_data)
}

##ENDglobal##ENDglobal##ENDglobal##ENDglobal##ENDglobal##ENDglobal##ENDglobal##ENDglobal##ENDglobal##ENDglobal

######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui
######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui
######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui

ui <- fluidPage(
  # Application title
  titlePanel("AGUA LIBRE:
             Roof-2-Food Calculator"),
  
  #Text explaining the app
  h5("Welcome to Agua Libre. This app allows you to select a potential garden 
     site and estimate its irrigation demand. The app also estimates rainwater collection
     potential for nearby roofs that can be used to irrigate your garden."),
  
  h5("Step 1: Enter the Latitude and Longitude for your site."),
  h5("Step 2: Enter values for area in acres for garden use and rainwater collection. Use the 
     measure tool on the map to measure areas if needed."),
  h5("Step 3: Customize your garden and collection system using the widgets on the side panel."),
  h5("Step 4: Once all parameters are set to your liking, click the 'Get Your Data' button.
     Processing may take up to 1 minute."),
  
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
                 value = 0.2, min = NA, max = NA, step = 0.001, width = NULL
    )
  ),
  sidebarPanel(
    numericInput ("Roof_Area",
                  "Input Roof Area in Acres:",
                  value = 0.4, min = NA, max = NA, step = 0.001, width = NULL
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
    
    tabsetPanel(
      tabPanel("Output Data Table",tableOutput("table1")),
      tabPanel("Monthly Demand & Collection", plotOutput("plot1")),
      tabPanel("Water in Tank",plotOutput("plot2"),textOutput("text1")),
      tabPanel("Water Supplies",plotlyOutput("plot3")),
      tabPanel("Data Access Map",imageOutput("map2")))
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
      
      h6("Common Efficiencies: 90% for most roofs. New metal roofs up to 95%."),
      
      # Making a irrigation season widget
      sliderInput("season", label = h4("Choose Your Growing Season"), min = 1, 
                  max = 12, value = c(4,10)),
      
      h6("Typical growing seasons begin in April (4) and end in October (10)."),
      
      actionButton("execute", "Get Your Data: takes less than 1 minute")
    ),
    mainPanel()
  ),
  
  
  mainPanel()
  
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
  coords <- reactiveValues()
  
  observeEvent(input$execute, {
    coords$lat <- input$Lat
  })
  
  observeEvent(input$execute, {
    coords$long <- input$Long
  }) 
  
  # Call function OptimalStation and store it's output as a reactive value
  myReactives <- eventReactive(input$execute, {
    OptimalStation(lat = coords$lat, long = coords$long)
  })
  
  
  # Render table of water budget
  output$table1 <- renderTable({
    data1 <- myReactives()
    climate_data <- data1[[1]]
    monthly_prcp <- climate_data$Prcp_in
    monthly_evap <- climate_data$Evap_in
    mon <- c(1:12)
    mon_char <- c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec")
    
    #Calculate effective precipitation according to emperical FAO calculations
    precip_effective <- rep(NA,12)
    for (i in 1:length(monthly_prcp)) {
      if (monthly_prcp[i] >= 2.95276){
        precip_effective[i] <- ((monthly_prcp[i]*0.8)-0.984252)
      }
      else (precip_effective[i] <- ((monthly_prcp[i]*0.6)-0.393701))
      
    }
    precip_effective[precip_effective<0] <- 0
    
    #Calculating the monthly collected rainfall in cubic meters
    Collected_Precip <- rep(NA,12)
    cum_balance <- rep(NA,12)
    cum_prcp <- rep(NA,12)
    #acre-feet to cubic meters
    conversion <- 1233.48
    for (i in 1:12) {
      Collected_Precip[i] <- ((monthly_prcp[i]/12)*conversion*input$Roof_Area*(input$Roof_Eff/100))
    }
    
    # determine mean crop coefficient based on user selection of crops
    crop <- as.character(crops$crop)
    coeff <- crops$coefficient
    crop_coeff <- rep(NA,length(input$Chosen_Crops))
    for (i in input$Chosen_Crops){
      row_pos <- which(crop == i)
      crop_coeff[i] <- coeff[row_pos]
    }
    crop_coeff <- na.omit(crop_coeff)
    crop_coeff <- as.numeric(crop_coeff)
    crop_coeff <- mean(crop_coeff)
    
    # determine monthly demand of rainwater and balance between rain water supply
    water_demand <- (crop_coeff*monthly_evap-precip_effective)/12
    water_demand <- (water_demand*input$Garden_Area*conversion)/(input$Irrigation_Eff/100)
    season_max <- max(input$season)
    season_min <- min(input$season)
    irrig_demand <- rep(0,12)
    for (i in season_min:season_max){
      irrig_demand[i] <- 1
    }
    irrig_demand <- water_demand*irrig_demand
    
    balance <- Collected_Precip - irrig_demand
    cum_balance[1] <- balance[1]
    required_input <- rep(0,12)
    cum_prcp[1] <- Collected_Precip[1]
    for (i in 2:12) {
      cum_balance[i] <- balance[i] + cum_balance[i-1]
      cum_prcp[i] <- Collected_Precip[i] + cum_prcp[i-1]
      if (cum_balance[i] < 0){
        required_input[i] <- cum_balance[i]*(-1)
        cum_balance[i] <- 0
      }
    }
    
    # Combine monthly prcp and evap data together in dataframe
    water_budget <- data.frame('index' = mon, 'Month' = mon_char,
                               'Collected_Rain_m3' = Collected_Precip,
                               'Irrigation_Demand_m3' = irrig_demand,
                               'Tank_Storage_m3' = cum_balance,
                               'Required_Input_m3' = required_input)
    water_budget
  })
  
  # Plotting outputs  
  output$plot1 <- renderPlot({
    data1 <- myReactives()
    climate_data <- data1[[1]]
    monthly_prcp <- climate_data$Prcp_in
    monthly_evap <- climate_data$Evap_in
    mon <- c(1:12)
    mon_char <- c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec")
    
    #Calculate effective precipitation according to emperical FAO calculations
    precip_effective <- rep(NA,12)
    for (i in 1:length(monthly_prcp)) {
      if (monthly_prcp[i] >= 2.95276){
        precip_effective[i] <- ((monthly_prcp[i]*0.8)-0.984252)
      }
      else (precip_effective[i] <- ((monthly_prcp[i]*0.6)-0.393701))
      
    }
    precip_effective[precip_effective<0] <- 0
    
    #Calculating the monthly collected rainfall in cubic meters
    Collected_Precip <- rep(NA,12)
    cum_balance <- rep(NA,12)
    cum_prcp <- rep(NA,12)
    #acre-feet to cubic meters
    conversion <- 1233.48
    for (i in 1:12) {
      Collected_Precip[i] <- ((monthly_prcp[i]/12)*conversion*input$Roof_Area*(input$Roof_Eff/100))
    }
    
    # determine mean crop coefficient based on user selection of crops
    crop <- as.character(crops$crop)
    coeff <- crops$coefficient
    crop_coeff <- rep(NA,length(input$Chosen_Crops))
    for (i in input$Chosen_Crops){
      row_pos <- which(crop == i)
      crop_coeff[i] <- coeff[row_pos]
    }
    crop_coeff <- na.omit(crop_coeff)
    crop_coeff <- as.numeric(crop_coeff)
    crop_coeff <- mean(crop_coeff)
    
    # determine monthly demand of rainwater and balance between rain water supply
    water_demand <- (crop_coeff*monthly_evap-precip_effective)/12
    water_demand <- (water_demand*input$Garden_Area*conversion)/(input$Irrigation_Eff/100)
    season_max <- max(input$season)
    season_min <- min(input$season)
    irrig_demand <- rep(0,12)
    for (i in season_min:season_max){
      irrig_demand[i] <- 1
    }
    irrig_demand <- water_demand*irrig_demand
    
    balance <- Collected_Precip - irrig_demand
    cum_balance[1] <- balance[1]
    required_input <- rep(0,12)
    cum_prcp[1] <- Collected_Precip[1]
    for (i in 2:12) {
      cum_balance[i] <- balance[i] + cum_balance[i-1]
      cum_prcp[i] <- Collected_Precip[i] + cum_prcp[i-1]
      if (cum_balance[i] < 0){
        required_input[i] <- cum_balance[i]*(-1)
        cum_balance[i] <- 0
      }
    }
    
    # Combine monthly prcp and evap data together in dataframe
    water_budget <- data.frame('index' = mon, 'Month' = mon_char,
                               'Collected_Rain_m3' = Collected_Precip,
                               'Irrigation_Demand_m3' = irrig_demand,
                               'Tank_Storage_m3' = cum_balance,
                               'Required_Input_m3' = required_input)
    
    # Create dataframe to reference for ggplot
    climate_plot <- data.frame('Collected Rain' = Collected_Precip, 'Irrigation Demand' = irrig_demand)
    library(reshape2)
    climate_plot <- melt(climate_plot)
    mon2 <- rep(1:12,2)
    climate_plot <- data.frame(mon2,climate_plot)
    colnames(climate_plot) <-  c('Month', 'Variable', 'Value_m3')
    
    #create ggplot smooth line graph
    ggplot() + geom_smooth(data = climate_plot, aes(x = climate_plot$Month, y = climate_plot$Value_m3, color = climate_plot$Variable)) + 
      scale_x_continuous(breaks=c(1:12)) + 
      labs(title = 'Rainwater Supply and Demand: 10 Year Historical Average', x = 'Month', y = 'Water Volume (m3)', color = "Legend Title\n") + 
      theme_set(theme_gray(base_size = 14))
  })
  
  output$plot2 <- renderPlot({
    data1 <- myReactives()
    climate_data <- data1[[1]]
    monthly_prcp <- climate_data$Prcp_in
    monthly_evap <- climate_data$Evap_in
    mon <- c(1:12)
    mon_char <- c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec")
    
    #Calculate effective precipitation according to emperical FAO calculations
    precip_effective <- rep(NA,12)
    for (i in 1:length(monthly_prcp)) {
      if (monthly_prcp[i] >= 2.95276){
        precip_effective[i] <- ((monthly_prcp[i]*0.8)-0.984252)
      }
      else (precip_effective[i] <- ((monthly_prcp[i]*0.6)-0.393701))
      
    }
    precip_effective[precip_effective<0] <- 0
    
    #Calculating the monthly collected rainfall in cubic meters
    Collected_Precip <- rep(NA,12)
    cum_balance <- rep(NA,12)
    cum_prcp <- rep(NA,12)
    #acre-feet to cubic meters
    conversion <- 1233.48
    for (i in 1:12) {
      Collected_Precip[i] <- ((monthly_prcp[i]/12)*conversion*input$Roof_Area*(input$Roof_Eff/100))
    }
    
    # determine mean crop coefficient based on user selection of crops
    crop <- as.character(crops$crop)
    coeff <- crops$coefficient
    crop_coeff <- rep(NA,length(input$Chosen_Crops))
    for (i in input$Chosen_Crops){
      row_pos <- which(crop == i)
      crop_coeff[i] <- coeff[row_pos]
    }
    crop_coeff <- na.omit(crop_coeff)
    crop_coeff <- as.numeric(crop_coeff)
    crop_coeff <- mean(crop_coeff)
    
    # determine monthly demand of rainwater and balance between rain water supply
    water_demand <- (crop_coeff*monthly_evap-precip_effective)/12
    water_demand <- (water_demand*input$Garden_Area*conversion)/(input$Irrigation_Eff/100)
    season_max <- max(input$season)
    season_min <- min(input$season)
    irrig_demand <- rep(0,12)
    for (i in season_min:season_max){
      irrig_demand[i] <- 1
    }
    irrig_demand <- water_demand*irrig_demand
    
    balance <- Collected_Precip - irrig_demand
    cum_balance[1] <- balance[1]
    required_input <- rep(0,12)
    cum_prcp[1] <- Collected_Precip[1]
    for (i in 2:12) {
      cum_balance[i] <- balance[i] + cum_balance[i-1]
      cum_prcp[i] <- Collected_Precip[i] + cum_prcp[i-1]
      if (cum_balance[i] < 0){
        required_input[i] <- cum_balance[i]*(-1)
        cum_balance[i] <- 0
      }
    }
    
    # Combine monthly prcp and evap data together in dataframe
    water_budget <- data.frame('index' = mon, 'Month' = mon_char,
                               'Collected_Rain_m3' = Collected_Precip,
                               'Irrigation_Demand_m3' = irrig_demand,
                               'Tank_Storage_m3' = cum_balance,
                               'Required_Input_m3' = required_input)
    
    # Required tank design volume
    storage_vol <- round(max(cum_balance), digits = 0)
    
    ggplot() + geom_col(data = water_budget, aes(water_budget$index, water_budget$Tank_Storage_m3)) + 
      scale_x_continuous(breaks=c(1:12)) + 
      labs(title = paste('End of the Month Tank Storage.                     Required Tank Size (m3)', storage_vol), x = 'Month', y = 'Water Volume (m3)') + 
      theme_set(theme_gray(base_size = 14))
  })
  
  output$plot3 <- renderPlotly({
    data1 <- myReactives()
    climate_data <- data1[[1]]
    monthly_prcp <- climate_data$Prcp_in
    monthly_evap <- climate_data$Evap_in
    mon <- c(1:12)
    mon_char <- c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec")
    
    #Calculate effective precipitation according to emperical FAO calculations
    precip_effective <- rep(NA,12)
    for (i in 1:length(monthly_prcp)) {
      if (monthly_prcp[i] >= 2.95276){
        precip_effective[i] <- ((monthly_prcp[i]*0.8)-0.984252)
      }
      else (precip_effective[i] <- ((monthly_prcp[i]*0.6)-0.393701))
      
    }
    precip_effective[precip_effective<0] <- 0
    
    #Calculating the monthly collected rainfall in cubic meters
    Collected_Precip <- rep(NA,12)
    cum_balance <- rep(NA,12)
    cum_prcp <- rep(NA,12)
    #acre-feet to cubic meters
    conversion <- 1233.48
    for (i in 1:12) {
      Collected_Precip[i] <- ((monthly_prcp[i]/12)*conversion*input$Roof_Area*(input$Roof_Eff/100))
    }
    
    # determine mean crop coefficient based on user selection of crops
    crop <- as.character(crops$crop)
    coeff <- crops$coefficient
    crop_coeff <- rep(NA,length(input$Chosen_Crops))
    for (i in input$Chosen_Crops){
      row_pos <- which(crop == i)
      crop_coeff[i] <- coeff[row_pos]
    }
    crop_coeff <- na.omit(crop_coeff)
    crop_coeff <- as.numeric(crop_coeff)
    crop_coeff <- mean(crop_coeff)
    
    # determine monthly demand of rainwater and balance between rain water supply
    water_demand <- (crop_coeff*monthly_evap-precip_effective)/12
    water_demand <- (water_demand*input$Garden_Area*conversion)/(input$Irrigation_Eff/100)
    season_max <- max(input$season)
    season_min <- min(input$season)
    irrig_demand <- rep(0,12)
    for (i in season_min:season_max){
      irrig_demand[i] <- 1
    }
    irrig_demand <- water_demand*irrig_demand
    
    balance <- Collected_Precip - irrig_demand
    cum_balance[1] <- balance[1]
    required_input <- rep(0,12)
    cum_prcp[1] <- Collected_Precip[1]
    for (i in 2:12) {
      cum_balance[i] <- balance[i] + cum_balance[i-1]
      cum_prcp[i] <- Collected_Precip[i] + cum_prcp[i-1]
      if (cum_balance[i] < 0){
        required_input[i] <- cum_balance[i]*(-1)
        cum_balance[i] <- 0
      }
    }
    
    # Combine monthly prcp and evap data together in dataframe
    water_budget <- data.frame('index' = mon, 'Month' = mon_char,
                               'Collected_Rain_m3' = Collected_Precip,
                               'Irrigation_Demand_m3' = irrig_demand,
                               'Tank_Storage_m3' = cum_balance,
                               'Required_Input_m3' = required_input)
    
    # Required tank design volume
    storage_vol <- round(max(cum_balance), digits = 0)
    cum_input <- round(sum(required_input), digits = 0)
    tot_demand <- round(sum(irrig_demand))
    savings <- round(sum(irrig_demand)-cum_input, digits = 0)
    
    dat <- c(cum_input, savings)
    lbls <- c('Remaining Irrigation Demand', 'Rainwater Collection')
    df <- data.frame(dat, lbls)
    plot_ly(df, labels = ~lbls, values = ~dat, type = 'pie',textposition = 'outside',textinfo = 'label+percent') %>%
      layout(title = 'Estimated Annual Water Budget',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  # Render map image showing user location relative to nearby weather stations
  output$map2 <- renderImage({
    data1 <- myReactives()
    localMap <- data1[[2]]
    list(src = localMap, contentType = 'image/jpg', width = 900, height = 700)
  })
  
}

shinyApp(ui = ui, server = server)

