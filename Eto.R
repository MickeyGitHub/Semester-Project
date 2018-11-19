#Load packages
library(rjson)
library(jsonlite)
library(RCurl)

#User input
lat <- 40.4555
long <- -109.5287
start_date <- '1980-01-01'
end_date <- '2017-01-01'

#Save base and full url as variables
url_usu = "https://climate.usu.edu/API/api.php/v2/key=62PyzUjrCDYh0JB95faxrDcGB9tTss/evapotranspiration/average_monthly_sum"
full_url = paste0(url_usu,'/state=UT/network=ghcn','/lat=',lat,'/long=',long,'/start_date=',start_date,'/end_date=',end_date,'/units=m/month=(1,2,3,4,5,6,7,8,9,10,11,12)/buffer=10')

# Convert JSON to data frame
df <- fromJSON(getURL(full_url))

#break down list structure into numeric dataframe
df <- do.call(rbind, df[[2]])
df <- do.call(rbind, df)
keep <- seq(2,nrow(df),2)
df <- df[keep,]
evap_sum <- as.numeric(df[,1])
mon <- as.numeric(df[,2])
sd <- as.numeric(df[,3])
df <- data.frame("month" = mon,"evap_sum" = evap_sum,"sd" = sd)















