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

crops <- read.csv('data/crops.csv',header=TRUE)

OptimalStation <- function(lat, long, start_date, end_date){
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
  #Retrieve data from closest weather stations
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
  localMap <- mapPoints
  output_data <- list(optimal_data, station_metadata, localMap)
  return(output_data)
}

##ENDglobal##ENDglobal##ENDglobal##ENDglobal##ENDglobal##ENDglobal##ENDglobal##ENDglobal##ENDglobal##ENDglobal

######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui
######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui
######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui
######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui
######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui
######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui
######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui
######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui
######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui
######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui
######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui
######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui######ui

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("AQUA LIBRE:
             Rainwater Collection and Garden Irrigation Demand"),
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

      # Show a plot of the generated distribution
      mainPanel( 
        
      
        #Text explaining how to find areas using the map
        h6("Use the 'Create 
           New Measurement' tool in the upper right side of the map to trace
           the outside of your garden. Input the area value into the 'Garden Area' box.
           Repeat the process for measuring the roof area used for rainwater collection.
           Input into 'Roof Area' box."),
        
        leafletOutput("map1"),
                 plotOutput("distPlot")
        
      ),
  sidebarLayout(
    sidebarPanel(
      
      #making a selection box widget
      selectInput("Chosen_Crops", label = h4("Choose Your Crops"), 
                  choices = unique(crops$crop), 
                  selected = NULL,
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
    mainPanel(
      
      plotOutput("IrrigationPlot"),
      
      h4("These are the words. The words I wish were not so far down here.")
    )
    
)
)
###ENDui###ENDui###ENDui###ENDui###ENDui###ENDui###ENDui###ENDui###ENDui###ENDui###ENDui###ENDui###ENDui###ENDui###ENDui

#####server#####server#####server#####server#####server#####server#####server#####server#####server#####server#####server
#####server#####server#####server#####server#####server#####server#####server#####server#####server#####server#####server
#####server#####server#####server#####server#####server#####server#####server#####server#####server#####server#####server
#####server#####server#####server#####server#####server#####server#####server#####server#####server#####server#####server
#####server#####server#####server#####server#####server#####server#####server#####server#####server#####server#####server
#####server#####server#####server#####server#####server#####server#####server#####server#####server#####server#####server
#####server#####server#####server#####server#####server#####server#####server#####server#####server#####server#####server
#####server#####server#####server#####server#####server#####server#####server#####server#####server#####server#####server
#####server#####server#####server#####server#####server#####server#####server#####server#####server#####server#####server
#####server#####server#####server#####server#####server#####server#####server#####server#####server#####server#####server
#####server#####server#####server#####server#####server#####server#####server#####server#####server#####server#####server
#####server#####server#####server#####server#####server#####server#####server#####server#####server#####server#####server
#####server#####server#####server#####server#####server#####server#####server#####server#####server#####server#####server
#####server#####server#####server#####server#####server#####server#####server#####server#####server#####server#####server
#####server#####server#####server#####server#####server#####server#####server#####server#####server#####server#####server

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  output$map1 <-renderLeaflet({
    m<-leaflet() %>%
      addTiles() %>%
      addMarkers(lng = input$Long, lat = input$Lat, popup="Your Site") %>%
      setView(lng = input$Long, lat = input$Lat, zoom = 16 )  %>%
      addMeasure()
  })

}

##ENDserver##ENDserver##ENDserver##ENDserver##ENDserver##ENDserver##ENDserver##ENDserver##ENDserver##ENDserver##ENDserver


# Run the application # Run the application # Run the application # Run the application # Run the application 
# Run the application # Run the application # Run the application # Run the application # Run the application 
# Run the application # Run the application # Run the application # Run the application # Run the application 
# Run the application # Run the application # Run the application # Run the application # Run the application 
# Run the application # Run the application # Run the application # Run the application # Run the application 
# Run the application # Run the application # Run the application # Run the application # Run the application 
# Run the application # Run the application # Run the application # Run the application # Run the application 
# Run the application # Run the application # Run the application # Run the application # Run the application 
# Run the application # Run the application # Run the application # Run the application # Run the application 

shinyApp(ui = ui, server = server)

