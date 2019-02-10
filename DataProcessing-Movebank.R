# read data from csv and ammend column names
data <- read.csv(file.choose(), sep=",",header=TRUE)
# select only columns with name, device number, datetime, lat, long and alt
data <- data[,c("tag.local.identifier","individual.local.identifier","timestampGMT","long","lat","height.raw","study.local.timestamp")]
# change column names to more practical shorter names
colnames(data)[1:7] <- c("dev","name","dt","long","lat","alt","ltime")

##########################################
####     Add metadata for birds      #####
##########################################
#include table with origin and sex of birds
## if this info doesnt exist keep this commented out
#individuals <- read.csv(file.choose(), sep=",",header=TRUE)
#merge Location data with metadata
#data <- merge(data,individuals,all.x=T)

#############################################
####  Create proper datetime objects    #####
#############################################
#remove milliseconds and create dt-object (POSIXct) (JAF Not necessary since igotu data has no milliseconds)
#data$dt <- gsub('.000', '', data$dt)
##NOTE: make sure date in csv is formatted as 2010-12-31 12:12:00 and in GMT
data$dt <- as.POSIXct(strptime(data$dt, format="%Y/%m/%d %H:%M:%S"), tz='GMT')
# class(data$dt)

data$dttz <- format(data$dt, tz=tzOffset, usetz=TRUE)
# class(data$dttz)
data$dttz <- as.POSIXct(strptime(data$dttz,format="%Y-%m-%d %H:%M:%S"),tz=tzOffset)

##########################################
####    Extract yday, mth, ...       #####
##########################################
#create a date-object
data$date <- as.Date(data$dttz, tz = tzOffset)

# define seasonal cycles per bird
#first time need to install package
#install.packages("lubridate")
library(lubridate)
data$yr <- year(data$dttz)
data$mth <- month(data$dttz)

# define some other potential splits for tracks
data$name_yr_mth <- paste(data$name,data$yr,data$mth,sep='_')
data$name_yr <- paste(data$name,data$yr,sep='_')

# determine yday and unique days per bird
data$yday <- yday(data$dttz)
data$yday <- sprintf('%03d',data$yday)
data$yday <- paste('D',data$yday,sep='')

# define a date per indiv
data$indday <- as.factor(paste(data$name, data$yr, data$yday, sep ='_', collapse = NULL))

########################################################
#### Calculate point to point movement statistics    ###
########################################################
source('Point2Point-stats.R')
data$hr <- hour(data$dttz)	

source('dailystats.R')
data <- merge(data,daylocs[,c("indday","daily.dist","daily.dir","daily.c.dist","straightness")],all.x=T)
data <- merge(data,unique(data2[,c("indday","daily.start","daily.end")]),all.x=T)

data$dayrange <- as.numeric(difftime(data$daily.end,data$daily.start))/3600

########################################################
#### Calculate sunrise and sunset times              ###
########################################################

source('SunriseSunsetTimes.R')









