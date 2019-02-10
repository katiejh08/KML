# Working directory for Projects Folder in Documents
## KJH:Change this to where you have the files you're working on
## In Documents folder I created this structure of folders:
setwd("~/Documents/Projects/R/KMLFolderBuilder")
tzOffset <- "Etc/GMT+3"

## KJH: The first time you run this you will need to install a few libraries
## After they are installed, you can comment out the install commands
#install.packages("fossil")
#install.packages("maptools")
#install.packages("lubridate")

##### Load Data ####
# Run the data processing script
#source('DataProcessing-Drone.r')
#source('DataProcessing-Movebank.r')
source('DataProcessing-IGotU.r')



##### STEP 1: Load Function ####
### RUN THE FOLLOWING LINE TO TEACH R THE FUNCTION TO CREATE GOOGLE EARTH FILES
source("NONAME_track2kml_v03052017.R") #Standard KML is created.
#source("NONAME_track2kml_NoTrackLines.R") #this creates a KML without the track lines


##### STEP 2: EXTRACT DATA ####
####OF THOSE INDIVIDUALS, DATES & TIMERANGES FOR WHICH YOU WANT TO PRODUCE TRACKS 
### Option 2.1: Use all data (including Camila, Julia and Buckeye)
datax <- data

### Option 2.2: Extract data for one or several individuals
# Specify names of individuals (YOU ONLY HAVE TO CHANGE NAMES HERE AND THEN RUN CODE)
#this was for the neck script
names <- c("Bobo","Carcass","Gojo","Marilou","Pole Evans","Squawk")
#this is for the settlement
names <- c("778-16422-16")

# Run subset() function to extract ALL data for these birds
datax <- subset(data,data$name %in% names)

###Option 2.3: Extract data for one or several individuals within a specific DATE range
# Specify names of individuals (YOU HAVE TO CHANGE NAMES HERE)
names <- c("Donald")

# Specify the start and end date of the daterange for which you want to produce tracks 
#(YOU HAVE TO ADJUST THE DATES HERE AND THEN RUN CODE)
startDate <- as.Date('2016-07-20',tz='Etc/GMT+05')
endDate <- as.Date('2017-11-04',tz='Etc/GMT+05')

# Run subset() function to extract data for the selected birds and daterange
datax <- subset(data,data$name %in% names & data$date >= startDate & data$date <= endDate)

###Option 2.4: Extract data for one or several idividuals on a specific DATE 
# Specify names of individuals
names <- c("Gifford")

# Specify the date for which you want to produce tracks
theDate <- as.Date('2016-08-08',tz='Etc/GMT+05')

# Run subset() function to extract data for the selected birds and daterange
datax <- subset(data,data$name %in% names & data$date == theDate)

###Option 2.5: Extract data for one or several idividuals within a specific TIMErange 
# Specify names of individuals (YOU HAVE TO CHANGE NAMES HERE)
names <- c("Donald")

# Specify the start and end TIMES of the timerange for which you want to produce tracks 
startTime <- as.POSIXct(strptime("2016-08-12 11:30:00",format="%Y-%m-%d %H:%M:%S"),tz="Etc/GMT+5")
endTime <- as.POSIXct(strptime("2017-08-12 18:30:00",format="%Y-%m-%d %H:%M:%S"),tz="Etc/GMT+5")

# Run subset() function to extract data for the selected birds and timerange
datax <- subset(data,data$name %in% names & data$dttz >= startTime & data$dttz <= endTime)


##### STEP 3: Color Scheme ####
### SPECIFY A COLOUR SCHEME FOR THE GOOGLE EARTH FILE THAT YOU WILL PRODUCE IN STEP 4
### Option 3.1: Use only one colour for everything
datax$colour <- rep('#66c2a5')

### Option 3.2: Define a colour based on daytime/nightime

datax$colour <- ifelse(datax$night == 'night','blue4','red4')

### Option 3.3: Define a colour based on individual (list of colours available here: 
### http://sape.inf.usi.ch/quick-reference/ggplot2/colour)

##Set A Igotu data from saunders
datax$colour <- rep('NA')
datax[which(datax$name == "Saunders01"),]$colour <- 'red'
datax[which(datax$name == "Saunders02"),]$colour <- 'green'
datax[which(datax$name == "Saunders07"),]$colour <- 'yellow'
datax[which(datax$name == "Saunders08"),]$colour <- 'purple'
datax[which(datax$name == "Saunders09"),]$colour <- 'brown'
datax[which(datax$name == "Saunders10"),]$colour <- 'blue'
unique(datax[,c("name","colour")])


##Set B Movebank Data Caracara Carcass","Gojo","Marilou","Pole Evans
datax$colour <- rep('NA')
datax[which(datax$name == "Bobo"),]$colour <- 'red'
datax[which(datax$name == "Squawk"),]$colour <- 'green'
datax[which(datax$name == "Carcass"),]$colour <- 'yellow'
datax[which(datax$name == "Gojo"),]$colour <- 'purple'
datax[which(datax$name == "Marilou"),]$colour <- 'violet'
datax[which(datax$name == "Pole Evans"),]$colour <- 'blue'
unique(datax[,c("name","colour")])

### Option 3.4: Define a colour based on spd

datax$colour <- ifelse(datax$spd > 3,'violet','green')

### Option 3.5: Define a colour based on distance to a certain point of interest (POI)

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


datax$season <- rep('NA')
datax[which(datax$mth >= 3 & datax$mth < 6),]$season <- 'Fall'
datax[which(datax$mth >= 6 & datax$mth < 9),]$season <- 'Winter'
datax[which(datax$mth >= 9 & datax$mth < 12),]$season <- 'Spring'
datax[which(datax$mth == 12 | datax$mth <= 2),]$season <- 'Summer'

datax$colour <- rep('NA')
datax[which(datax$season == "Spring"),]$colour <- '#66c2a5'
datax[which(datax$season == "Summer"),]$colour <- '#e78ac3'
datax[which(datax$season == "Fall"),]$colour <- '#fc8d62'
datax[which(datax$season == "Winter"),]$colour <- '#8da0cb'

unique(datax[,c("name","colour")])

##### STEP 4: Point Size ####
### SPECIFY SIZE FOR POINTS IN GOOGLE EARTH USING A SCALING FACTOR 

# Choose a value between 0.1 - 1 in order to determine point size
pointsize <- 0.2 # default value is 1

##### STEP 5: PRODUCE KML #####
# RUN the function for producing Google Earth files

#### 5.1 #### 
##Note: This is the name of the KML
fname <- "778-16422-16"
NONAME.track2kmlOpenFile(output.filename= fname)

####5.2 choose one of these options ####

#5.2A 
  #Separate into folders by name (all in the same KML)
  for(i in 1:length(unique(datax$name))){
  
  	ss <- subset(datax, name== unique(datax$name)[i])
  	ss <- ss[order(ss$dttz),]	## data must be ordered on time before generating a kml! ##
  	NONAME.track2kmlAppendData(
  # Can?t touch this:
  		latitude=ss$lat, 
  		longitude=ss$long, 
  		datetime=ss$dttz,
  		col.scheme = ss$colour,
  # Make the box:
  		data.variables=data.frame(ss$name),# generates an attribute table for every GPS fix including the variables specified here
  # Specify the foldername:
      output.filename=fname,descriptive.foldername=names[i])
  }

#5.2B
  #Separate into folders by Day (all in the same KML) 
  #use for shorter deployments (uses Name-year-day)
  for(i in 1:length(unique(datax$indday))){
    
    ss <- subset(datax, indday== unique(datax$indday)[i])
    ss <- ss[order(ss$dttz),]	## data must be ordered on time before generating a kml! ##
    NONAME.track2kmlAppendData(
      # Can?t touch this:
      latitude=ss$lat, 
      longitude=ss$long, 
      datetime=ss$dttz,
      col.scheme = ss$colour,
      # Make the box:
      data.variables=data.frame(ss$name, ss$night),# generates an attribute table for every GPS fix including the variables specified here
      # Specify the filename:
      output.filename=fname,descriptive.foldername=indday[i])
  }

#5.2C
  #Separate into folders by season (all in the same KML)
  #Use for longer deployments
  ## NOTE: Need to run option 3.6 and 5.1 prior
  seasons <- c( "Winter", "Spring", "Summer","Fall")
  
  for(i in 1:length(unique(datax$name))){
    
    ss <- subset(datax, name== unique(datax$name)[i])
    ss <- ss[order(ss$dttz),]	## data must be ordered on time before generating a kml! ##
    NONAME.track2kmlAddFolder(output.filename= fname, descriptive.foldername= unique(datax$name)[i])
    for(j in 1:length(unique(ss$season))){
      ss1 <- subset(ss, season == unique(ss$season)[j])
      #ss1 <- ss1[order(ss1$dttz),]	## data must be ordered on time before generating a kml! ##
      NONAME.track2kmlAppendData(
        # Can?t touch this:
        latitude=ss1$lat, 
        longitude=ss1$long, 
        datetime=ss1$dttz,
        col.scheme = ss1$colour,
        # Make the box:
        data.variables=data.frame(ss1$name, ss1$season),# generates an attribute table for every GPS fix including the variables specified here
        # Specify the foldername:
        output.filename=fname,descriptive.foldername=unique(ss$season)[j])
    }
    NONAME.track2kmlCloseFolder(output.filename= fname)
  }


#5.2D
  #Separate into folders by season (all in the same KML)
  #Use for longer deployments
  ## NOTE: Need to run option 3.6 and 5.1 prior
  seasons <- c( "Winter", "Spring", "Summer","Fall")
  yrs <- c(unique(as.numeric(datax$yr)))
  
  
  for(i in 1:length(yrs)){
    ss <- subset(datax, yr== yrs[i])
    ss <- ss[order(ss$dttz),]	## data must be ordered on time before generating a kml! ##
    NONAME.track2kmlAddFolder(output.filename= fname, descriptive.foldername= yrs[i])
    mths <- c(unique(as.numeric(ss$mth)))
    if(length(mths) > 0) {
      for(j in 1:length(mths)){
        ss1 <- subset(ss, mth == mths[j])
        ss1 <- ss1[order(ss1$dttz),]	## data must be ordered on time before generating a kml! ##
        NONAME.track2kmlAddFolder(output.filename= fname, descriptive.foldername= mths[j])
        if(length(unique(ss1$name)) > 0) {
          for(k in 1:length(unique(ss1$name))){
            
            ss2 <- subset(ss1, name== unique(ss1$name)[k])
            ss2 <- ss2[order(ss2$dttz),]	## data must be ordered on time before generating a kml! ##
            NONAME.track2kmlAppendData(
              # Can?t touch this:
              latitude=ss2$lat, 
              longitude=ss2$long, 
              datetime=ss2$dttz,
              col.scheme = ss2$colour,
              # Make the box:
              data.variables=data.frame(ss2$name, ss2$season),# generates an attribute table for every GPS fix including the variables specified here
              # Specify the foldername:
              output.filename=fname,descriptive.foldername=unique(ss1$name)[k])
          }
        }
      NONAME.track2kmlCloseFolder(output.filename= fname) #cloe the month folder
      }
    }
    NONAME.track2kmlCloseFolder(output.filename= fname) #close the year folder
  }
  
  
#5.2E
  #Separate into folders by Year (all in the same KML) 
  #use for longer deployments (uses yr)
  for(i in 1:length(unique(datax$yr))){
    
    ss <- subset(datax, yr== unique(datax$yr)[i])
    ss <- ss[order(ss$dttz),]	## data must be ordered on time before generating a kml! ##
    NONAME.track2kmlAppendData(
      # Can?t touch this:
      latitude=ss$lat, 
      longitude=ss$long, 
      datetime=ss$dttz,
      col.scheme = ss$colour,
      # Make the box:
      data.variables=data.frame(ss$name, ss$dttz),# generates an attribute table for every GPS fix including the variables specified here
      # Specify the filename:
      output.filename=fname,descriptive.foldername=yr[i])
  }

  
  
#### 5.3 #### 
#Put the final document closure to File
NONAME.track2kmlCloseFile(output.filename=fname)


##### STEP 6: Save Data to CSV #####

write.csv(datax,file="ScriptOutput.csv")

