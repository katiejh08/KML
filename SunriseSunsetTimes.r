
# create Spatial Points object of Sakhalvasho coordinates
crds <- cbind(data$long, data$lat)
data$crds <- SpatialPoints(crds,proj4string=CRS("+proj=longlat +datum=WGS84"))

# create Date object to pass to sunriset function
dates <- gsub('-', '/', as.character(data$date))
data$dates <- as.POSIXct(dates,tz=tzOffset)  

#first time need to install package
#install.packages("maptools")
library(maptools)
# calculate sunrise times
data$srise <- sunriset(data$crds, data$dates, direction=c("sunrise"),POSIXct.out=TRUE)[,2]

# calculate sunset times
data$sset <- sunriset(data$crds, data$dates, direction=c("sunset"),POSIXct.out=TRUE)[,2]

# determine if point is during night or day
data$night <- ifelse(data$dttz < data$srise | data$dttz > data$sset,'night','day')

drops <- c("dates","crds","season")
data <- data[ , !(names(data) %in% drops)]

