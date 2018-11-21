library(shiny)
library(leaflet)
library(rnoaa)
library(ggmap)
library(ggplot2)
library(dplyr)
library(rjson)
library(jsonlite)
library(RCurl)

######################################################################################
######################################################################################
######################################################################################

#Calculating Collected Rooftop Rainfall

#acre-feet to cubic meters
conversion <- 1233.48

reactive({

  #Assuming Monthly Rainfall data is acquired as dataframe called "Monthly_Precip". using fake data for now
  Monthly_Precip <- data.frame(Month = (1:12), PrecipData = (21:33))

  #Calculating the monthly collected rainfall in cubic meters
  Collected_Precip <- data.frame(Month = Monthly_Precip$Month, 
                               Collected = ((Monthly_Precip$PrecipData/12)*input$Roof_Area*conversion*input$Roof_Eff))
})

######################################################################################
######################################################################################

#Calcualting effective precipitation
  #assuming precipitation data will come in as inches per month

#fake data, raw monthly precip in inches
precip_raw <- data.frame(Month = (1:12), PrecipInches = c(3.2,3.3,4.1,4.6,3.3,2.1,0.9,1.2,1.9,3.3,3.0,3.0))

#dimension effective precipitation dataframe
precip_effective <- data.frame(Month = precip_raw$Month, EffectiveInches = rep(NA,12))

#for loop to calculate effective precipitation according to emperical FAO calculations
for (i in 1:nrow(precip_raw)) {
  if (precip_raw$PrecipInches[i] >= 2.95276){
    precip_effective$EffectiveInches[i] <- ((precip_raw$PrecipInches[i]*0.8)-0.984252)
  }
  else (precip_effective$EffectiveInches[i] <- ((precip_raw$PrecipInches[i]*0.6)-0.393701))
  
}

######################################################################################
######################################################################################

#Calculating the monthly irrigation requirement

#crops data frame should be read into global.  csv file must be in the working directory
crops <- read.csv('data/crops.csv',header=TRUE)

#dimensioning variables to match length of variable 'crops'
Chosen_Crop_K <- rep(NA,length(crops))
Portion_Of_Crop <- rep(NA,length(crops))
Coeff_Factor <- rep(NA,length(crops))

#reactive area dependent on user inputs
reactive({
  
  # the crop coefficiens corresponding to the chosen crops are stored in a data frame  
  for (i in 1:nrow(crops)) {
   if (crops$crop == input$Chosen_Crops){
  
      Chosen_Crop_K[i] <- crops$coefficient[i]
    }
  }
  
  # If crop is chosen, sliderbar appears and user chooses the portion of garden area used by that crop
  #  !!! this task will not work. Need to rethink our strategy !!!
  for (i in 1:nrow(crops)) {
    if (crops$crop == input$Chosen_Crops){
      
      sliderInput(Portion_Of_Crop[i],'What portion (decimal) of the garden will this crop utilize?',0,min = 0,max = 1)
    }
  }

  #crop coefficients are multiplied by the decimal percentage of use.
  for (i in 1:nrow(Chosen_Crop_K)){
  Coeff_Factor[i] <- (Portion_Of_Crop[i] * Chosen_Crop_K[i])
  
  }
  #an average crop coefficient for the garden is developed
  CombinedCropK <- sum(Coeff_Factor)

  #stores the area of the whole garden input by the user
  acres <- input$Garden_Area

  #acre-feet to cubic meters
  conversion <- 1233.48

  #using fake data for now. Replace with retrieved ET data
  EvapoT <- data.frame(Month = c(1:12), ETo = c(30:41))

  #applying crop coefficient to evapotranspiratin data and garden area to find the irrigation demand (volume of water)
  IrrigationVol <- data.frame(Month = EvapoT$Month , WaterVol = ((EvapoT$ETo/12)*acres*conversion*CombinedCropK))
  
  #deducting the amount of effective rain that fell from the irrigation demand
  NetIrrigation <- data.frame(Month = EvapoT$Month , 
                              IrrigRequired = (IrrigationVol$WaterVol - 
                                                 (precip_effective$EffectiveInches*acres*conversion)))
  
  #applying the user-input irrigation efficiency to the irrigation demand
  FinalIrrigation <- data.frame(Month = EvapoT$Month , FinalNeededWater = (IrrigRequired / input$Irrigation_Eff))

})

######################################################################################
######################################################################################
  
      
#Plotting outputs  
output$IrrigationPlot <- renderPlot({
   ggplot() +
    geom_line(data=FinalIrrigation,aes(x=FinalIrrigation$Month , y=FinalIrrigation$FinalNeededWater),color='red') + 
    geom_line(data=Collected_Precip,aes(x=Collected_Precip$Month , y=Collected_Precip$Collected),color='blue')
    
})

######################################################################################
######################################################################################
######################################################################################



  