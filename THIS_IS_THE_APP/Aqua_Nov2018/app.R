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

crops <- read.csv('data/crops.csv',header=TRUE)


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
  
  #making a selection box widget
  selectInput("Chosen_Crops", label = h3("Choose Your Crops"), 
              choices = unique(crops$crop), 
              selected = NULL,
              multiple = TRUE,
              selectize = TRUE,
              width = NULL,
              size = NULL),
 
  # Making an Irrigation Efficiency Widget 
  sliderInput("Irrigation_Eff", label = h3("Choose Your Irrigation Efficiency"), min = 0, 
              max = 100, value = 75),

  #Text suggesting irrigation efficiencies
  textOutput("Suggested_Irrigation_Eff"),
  
  # Making a Rooftop Efficiency Widget 
  sliderInput("Roof_Eff", label = h3("Choose Your Rainwater Collection Efficiency"), min = 0, 
              max = 100, value = 75),
  
  #Text suggesting roof efficiencies
  textOutput("Suggested_Roof_Eff"),
  
  #TEST: total score = 4* "Roof_Eff"
  textOutput("Total_Score"),
  
  #TEST: Print the coefficient for the chosen crop
  textOutput("COEF"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput("Garden_Area",
                   "Input Garden Area in Acres:",
                   value = 0.5, min = NA, max = NA, step = 0.001, width = NULL
      )
    ),
    sidebarLayout(
      sidebarPanel(
        numericInput ("Roof_Area",
                      "Input Roof Area in Acres:",
                      value = 0.8, min = NA, max = NA, step = 0.001, width = NULL
        )
      ),
      
      # Show a plot of the generated distribution
      mainPanel( leafletOutput("map1"),
                 plotOutput("distPlot")
      )
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
      setView(lng = -111.8449, lat = 40.768, zoom=14 )  %>%
      addMeasure()
  })
  
  #making a Crop Selection selection box
  output$Chosen_Crops <- renderPrint({ input$Chosen_Crops })
  
  # Making an Irrigation Efficiency Widget
  output$Irrigation_Eff <- renderPrint({ input$Irrigation_Eff})
  
  # Print Text suggesting an Irrigation Efficiency Value
  output$Suggested_Irrigation_Eff <- renderText(paste('Common Efficiencies: Drip Irrigation 90%. Sprinklers 75%'))
  
  # Making an Rainwater Collection Widget
  output$Roof_Eff <- renderPrint({ input$Roof_Eff })
  
  # Print Text suggesting a Rainwater Collection Efficiency
  output$Suggested_Roof_Eff <- renderText(paste('Common Efficiencies: 90% for most roofs. New metal roofs up to 95%.'))
  
  # output of Garden Area numeric input
  output$Garden_Area <- renderPrint({ input$Garden_Area })
  
  # output of Roof Area numeric input
  output$Roof_Area <- renderPrint({ input$Roof_Area })
 
   # TEST calculating "Total_Score" with input form Roof Area slider bar
  output$Total_Score <- renderText({
    Roof_4 <- input$Roof_Area * 10
    paste(Roof_4)
    })
  
  #  #TEST: Print the coefficient for the chosen crop
  output$COEF <- renderText({
    paste(crops$input$Chosen_Crops[2])
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

