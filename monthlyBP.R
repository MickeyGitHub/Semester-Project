#Read in file
datatable <- read.csv('/Users/Zach/Desktop/RB_GIRF_C - Evapotrans_ETr - Evapotranspiration, potential.csv')
values <- datatable$Value
dates <- datatable$Date
dates <- as.Date.factor(dates)
dates <- format.Date(dates, "%m")
dates <- as.numeric(dates)
months <- vector(mode = "list", length = 12)
monthstats <- vector(mode = "list", length = 12)
b <- c(1:length(values))
#Assign NA to all values of -9999
for (j in b){
  if (values[j] == -9999){
    values[j] <- NA
  }
}
#Populate monthly data
for (i in 1:12){
  a <- months[[i]]
  for (k in 1:length(values)){
    if (dates[k] == i){
      a[k] <- values[k]
    }
  }
  monthstats[[i]] <- summary(a)
  months[[i]] <- a
}
months_summary = do.call(rbind, monthstats)
boxplot( months[[1]], months[[2]], months[[3]], months[[4]], months[[5]], 
         months[[6]], months[[7]], months[[8]], months[[9]], months[[10]], 
         months[[11]], months[[12]], ylab ="Evapotranspiration Potential (mm)", xlab ="Month", las = 1, names = c(1:12))

