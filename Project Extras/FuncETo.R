#This function requires an input of latitude and longitude for a GHCN weather station determined
#by first using the function 'OptimalStation'. Also, there is a required input of start and end
#dates for this function that the user is prompted for within the app. This function retreives 
#average monthly sums of evapotranspiration data in inches from the Utah Climate Center's API.
#The output of this function is a dataframe for the average monthly evapotranspiration for each
#month in the year along with the standard deviation that coincides.

ETo <- function(lat, long, start_date, end_date){
  # Load new packages 
  library(rjson)
  library(jsonlite)
  library(RCurl)
  # Input
  start_date <- '2000-01-01'
  end_date <- '2018-01-01'
  # Save base and full url as variables
  url_usu = "https://climate.usu.edu/API/api.php/v2/key=62PyzUjrCDYh0JB95faxrDcGB9tTss/evapotranspiration/average_monthly_sum"
  full_url = paste0(url_usu,'/state=UT/network=ghcn','/lat=',lat,'/long=',long,'/start_date=',start_date,'/end_date=',end_date,'/units=m/month=(1,2,3,4,5,6,7,8,9,10,11,12)/buffer=10')
  # Convert JSON to data frame
  evap_data <- fromJSON(getURL(full_url))
  # Break down list structure into numeric dataframe
  evap_data <- do.call(rbind, evap_data[[2]])
  evap_data <- evap_data[c(2:13),]
  evap <- as.numeric(evap_data[,1])
  mon <- as.numeric(evap_data[,2])
  std <- as.numeric(evap_data[,3])
  evap_data <- data.frame('month' = mon, 'evap_sum' = evap, 'std_dev' = std)
  return(df)
}


  