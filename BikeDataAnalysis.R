# Most popular stations
getMostPopular <- function(v){
  uniqueValues = unique(v)
  uniqueValues[which.max(tabulate(match(v,uniqueValues)))]
}

# Los-Angeles Metro Area Bike Data Analysis by Sadaqat Ali
library(geosphere)
fullData = metro_bike_share_trip_data[complete.cases(metro_bike_share_trip_data),]
mostPopularStarting = getMostPopular(fullData$`Starting Station ID`)
mostPopularEnding = getMostPopular(fullData$`Ending Station ID`)
uniqueValues = unique(metro_bike_share_trip_data$`Passholder Type`)
distrPasses = tabulate(match(metro_bike_share_trip_data$`Passholder Type`,uniqueValues))
regularCustomers = 132427 - distrPasses[3]
average <- getAverageDistance(fullData)




# Average Distance Function by Sadaqat Ali
getAverageDistance <- function(fullData){
totalDistance = 0;
aveDist= 0
cat("Calculating..")
for (k in 1:97825){
    startlat = as.numeric(fullData[k, "Starting Station Latitude"])
    endlat = as.numeric(fullData[k, "Ending Station Latitude"])
    startlong = as.numeric(fullData[k, "Starting Station Longitude"])
    endlong = as.numeric(fullData[k, "Ending Station Longitude"])
    
    starting = c(startlong,startlat)
    ending = c(endlong,endlat)
    
    d = distm(starting,ending , fun = distVincentyEllipsoid)
    totalDistance = totalDistance + (d/1609.344)
}
aveDist = totalDistance/97825
cat("Calculation finished...\nAverage Distance Traveled is: ")
aveDist
}

