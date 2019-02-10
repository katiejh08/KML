# Working directory for Projects Folder in Documents
## KJH:Change this to where you have the files you're working on
## In Documents folder I created this structure of folders:
setwd("~/Documents/Projects/R/MovebankKML")

## KJH: The first time you run this you will need to install a few libraries
## After they are installed, you can comment out the install commands
#install.packages("fossil")
#install.packages("maptools")
#install.packages("lubridate")

# convert date times to Correct timezone
## NOTE must change the timezone if working with other data. Plus is Minus.
tzOffset <- "Etc/GMT+3"

# Run the data processing script
source('DataProcessing-Movebank.r')

#################################################
#####           PRODUCE KMLS                 ####
#################################################

##### STEP 1: RUN THE FOLLOWING LINE TO TEACH R THE FUNCTION TO CREATE GOOGLE EARTH FILES
###########################################################################################################
source("NONAME_track2kml_v03052017.R")


##### STEP 2: EXTRACT DATA OF THOSE INDIVIDUALS, DATES & TIMERANGES FOR WHICH YOU WANT TO PRODUCE TRACKS 
###########################################################################################################
###Option 2.1: Use all data (including Camila, Julia and Buckeye)
######################################################################
datax <- data

###Option 2.2: Extract data for one or several individuals
######################################################################
# Specify names of individuals (YOU ONLY HAVE TO CHANGE NAMES HERE AND THEN RUN CODE)
#this was for the neck script
names <- c("Bobo","Squawk","Carcass","Gojo","Marilou","Pole Evans")
#this is for the settlement
names <- c("Pole Evans")

# Run subset() function to extract ALL data for these birds
datax <- subset(data,data$name %in% names)

###Option 2.3: Extract data for one or several individuals within a specific DATE range
######################################################################
# Specify names of individuals (YOU HAVE TO CHANGE NAMES HERE)
names <- c("Donald")

# Specify the start and end date of the daterange for which you want to produce tracks 
#(YOU HAVE TO ADJUST THE DATES HERE AND THEN RUN CODE)
startDate <- as.Date('2016-07-20',tz='Etc/GMT+05')
endDate <- as.Date('2017-11-04',tz='Etc/GMT+05')

# Run subset() function to extract data for the selected birds and daterange
datax <- subset(data,data$name %in% names & data$date >= startDate & data$date <= endDate)

###Option 2.4: Extract data for one or several idividuals on a specific DATE 
######################################################################
# Specify names of individuals
names <- c("Gifford")

# Specify the date for which you want to produce tracks
theDate <- as.Date('2016-08-08',tz='Etc/GMT+05')

# Run subset() function to extract data for the selected birds and daterange
datax <- subset(data,data$name %in% names & data$date == theDate)

###Option 2.5: Extract data for one or several idividuals within a specific TIMErange 
######################################################################
# Specify names of individuals (YOU HAVE TO CHANGE NAMES HERE)
names <- c("Donald")

# Specify the start and end TIMES of the timerange for which you want to produce tracks 
startTime <- as.POSIXct(strptime("2016-08-12 11:30:00",format="%Y-%m-%d %H:%M:%S"),tz="Etc/GMT+5")
endTime <- as.POSIXct(strptime("2017-08-12 18:30:00",format="%Y-%m-%d %H:%M:%S"),tz="Etc/GMT+5")

# Run subset() function to extract data for the selected birds and timerange
datax <- subset(data,data$name %in% names & data$dttz >= startTime & data$dttz <= endTime)


##### STEP 3: SPECIFY A COLOUR SCHEME FOR THE GOOGLE EARTH FILE THAT YOU WILL PRODUCE IN STEP 4
###########################################################################################################
### Option 3.1: Use only one colour for everything
######################################################################
datax$colour <- rep('seagreen3')

### Option 3.2: Define a colour based on daytime/nightime
######################################################################
datax$colour <- ifelse(datax$night == 'night','blue4','red4')

### Option 3.3: Define a colour based on individual (list of colours available here: 
### http://sape.inf.usi.ch/quick-reference/ggplot2/colour)
######################################################################
datax$colour <- rep('NA')
datax[which(datax$name == "Saunders01"),]$colour <- 'red'
datax[which(datax$name == "Saunders02"),]$colour <- 'green'
datax[which(datax$name == "Saunders07"),]$colour <- 'yellow'
datax[which(datax$name == "Saunders08"),]$colour <- 'purple'
datax[which(datax$name == "Saunders09"),]$colour <- 'brown'
datax[which(datax$name == "Saunders10"),]$colour <- 'blue'
unique(datax[,c("name","colour")])

### Option 3.4: Define a colour based on spd
######################################################################
datax$colour <- ifelse(datax$spd > 3,'violet','green')

### Option 3.5: Define a colour based on distance to a certain point of interest (POI)
######################################################################
# Specify lat and long of the point of interest
POI_lat <- 52.6
POI_long <- 45.2

# Specify distance buffer around point of interest (in feet!)
POI_dist <- 50

# Run the following line to calculate distance of each point relative to the POI in feet
datax$dist.to.POI <- (deg.dist(lat1=datax$lat,long1=datax$long,lat2=POI_lat,long2=POI_long)*1000)*3.28084

# Run the following line to colour points within threshold distance and those further away in different colours
datax$colour <- ifelse(datax$dist.to.POI < POI_dist,'blue','red')

### Option 3.6: Do multiple colours for four seasons
######################################################################

datax$season <- rep('NA')
datax[which(datax$mth >= 3 & datax$mth < 6),]$season <- 'Fall'
datax[which(datax$mth >= 6 & datax$mth <= 8),]$season <- 'Winter'
datax[which(datax$mth >= 9 & datax$mth <= 11),]$season <- 'Spring'
datax[which(datax$mth == 12 | datax$mth <= 2),]$season <- 'Summer'

datax$colour <- rep('NA')
datax[which(datax$season == "Spring"),]$colour <- '#66c2a5'
datax[which(datax$season == "Summer"),]$colour <- '#e78ac3'
datax[which(datax$season == "Fall"),]$colour <- '#fc8d62'
datax[which(datax$season == "Winter"),]$colour <- '#8da0cb'

# datax$colour <- rep('NA')
# datax[which(datax$season == "Spring"),]$colour <- 'green'
# datax[which(datax$season == "Summer"),]$colour <- 'yellow'
# datax[which(datax$season == "Fall"),]$colour <- 'darkorange1'
# datax[which(datax$season == "Winter"),]$colour <- 'blue'

unique(datax[,c("name","colour")])

##### STEP 4: SPECIFY SIZE FOR POINTS IN GOOGLE EARTH USING A SCALING FACTOR
###########################################################################################################
# Choose a value between 0.1 - 1 in order to determine point size
pointsize <- 0.2 # default value is 1

##### STEP 5: PRODUCE GOOGLE EARTH FILE BY APPLYING MAGIC SAUCE
####################################################################################################
# RUN the function for producing Google Earth files

##Note: This will appear at the beginning of each filename and should describe the type of color scheme you chose
fname <- "R22SeasonsAll"

for(i in 1:length(unique(datax$name))){

	ss <- subset(datax, name== unique(datax$name)[i])
	ss <- ss[order(ss$dttz),]	## data must be ordered on time before generating a kml! ##
	NONAME.track2kml(
# Can?t touch this:
		latitude=ss$lat, 
		longitude=ss$long, 
		datetime=ss$dttz,
		col.scheme = ss$colour,
# Make the box:
		data.variables=data.frame(ss$name,ss$season),# generates an attribute table for every GPS fix including the variables specified here
# Specify the filename:
    output.filename=paste(fname,names[i],sep='_'))
	}

##### STEP 6: Save Data to CSV 
####################################################################################################
# 

write.csv(datax,file="ScriptOutput.csv")

