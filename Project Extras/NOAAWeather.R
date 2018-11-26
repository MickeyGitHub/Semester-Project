#Reference manual for NOAAWeather package:
#https://cran.r-project.org/web/packages/NOAAWeather/NOAAWeather.pdf 

data <- getWeatherData(online=TRUE,
                       location="Austin",state="TX",from="2017-01-10",
                       to="2017-01-11")

metrics <- getMetrics(online = TRUE)