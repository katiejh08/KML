# Working directory for Projects Folder in Documents
## KJH:Change this to where you have the files you're working on
## In Documents folder I created this structure of folders:
setwd('/Users/jamesfahlbusch/Documents/Projects/R/IGOTUKML_Current')

## KJH: The first time you run this you will need to install a few libraries
## After they are installed, you can comment out the install commands
#install.packages("fossil")
#install.packages("maptools")
#install.packages("lubridate")

tzOffset <- "Etc/GMT+3"

# Run the data processing script
source('DataProcessing_TWLogger_V3.r')

#################################################
#####           PRODUCE KMLS                 ####
#################################################

##### STEP 1: RUN THE FOLLOWING LINE TO TEACH R THE FUNCTION TO CREATE GOOGLE EARTH FILES
###########################################################################################################
source("NONAME_track2kml_v03052017.R")


##### STEP 2: EXTRACT DATA OF THOSE INDIVIDUALS, DATES & TIMERANGES FOR WHICH YOU WANT TO PRODUCE TRACKS 
###########################################################################################################

datax <- data
names <- c(name)

#simple plot
plot(datax$long,datax$lat)
#simple plot
plot(datax$dttz)
plot(diff(datax$dttz)) # diff returns lag for subsequent values (if negative value, bad hit)

# GPS data already subset from CombineCSV_V3 script
# start <- "2017-07-10 13:14:00" #This should be the Deployment Date/Time (GMT)
# end <- "2017-07-16 19:00:00" #This should be the Recovery Date/Time (GMT)


# startTime <- as.POSIXct(strptime(start,format="%Y-%m-%d %H:%M:%S"),tz='GMT')
# endTime <- as.POSIXct(strptime(end,format="%Y-%m-%d %H:%M:%S"),tz='GMT')
# # Run subset() function to extract data for the selected timerange
# datax <- subset(datax,datax$dt >= startTime & datax$dt <= endTime)
# plot(datax$dttz)

# Calculate deployment length
data$dttz[length(data$dttz)]-data$dttz[1]

### Option 3: Define a colour based on daytime/nightime
######################################################################
datax$colour <- ifelse(datax$night == 'night','blue4','red4')


##### STEP 4: SPECIFY SIZE FOR POINTS IN GOOGLE EARTH USING A SCALING FACTOR
###########################################################################################################
# Choose a value between 0.1 - 1 in order to determine point size
pointsize <- 0.2 # default value is 1

##### STEP 5: PRODUCE GOOGLE EARTH FILE BY APPLYING MAGIC SAUCE
####################################################################################################
# RUN the function for producing Google Earth files

##Note: This will appear at the beginning of each filename and should describe the type of color scheme you chose
fname <- name

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
		data.variables=data.frame(ss$name,ss$night),# generates an attribute table for every GPS fix including the variables specified here
# Specify the filename:
    output.filename=fname)
	}

##### STEP 6: Save Data to XLSX 
####################################################################################################
# 

write.csv(datax,file=paste(name,"ScriptOutput.csv",sep="_"))

##### STEP 7: Create and Save Map to PNG
####################################################################################################
# Google has changed permissions, so this may not work without an API key

## Map
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

mapZoom <- 12 # KJH!! Adjust this to fit all the data (20 is all the way zoomed in)
#plot the extent of this map (NOTE: must adjust zoom parameter)
myMap <-  get_googlemap(center = c(lon = mean(c(min(datax$long), max(datax$long))),  lat = mean(c(min(datax$lat),max(datax$lat)))),
                        zoom = mapZoom, 
                        size = c(640,400),
                        scale = 2,
                        extent="device",
                        maptype="hybrid", #"terrain"
                        style = c(feature = "all", element = "labels", visibility = "off"),
                        key="AIzaSyAHRpFoa86K8IXnsIo1H3fLjWyhorawZew")

#plot map
ggmap(myMap)+
  # geom_point(data=datax,aes(x=as.numeric(as.character(long)),y=as.numeric(as.character(lat))),alpha = 0.5, color= 'yellow') +
  geom_point(data=datax,aes(x=as.numeric(as.character(long)),y=as.numeric(as.character(lat)),color=night),alpha = 0.5) +
  scale_alpha(guide = 'none') + 
  theme(legend.position="right", 
        plot.title = element_text(hjust = 0.5)) + 
  ggtitle(name) + coord_fixed(1.3) + 
  labs(
    x = "Longitude",
    y = "Latitude"
  )
#save map output as .png
ggsave(sprintf("%s_Map.png",name), plot = last_plot(), device = "png",
       scale = 2, width = 7, height = 5, units = "in", dpi = 300, limitsize = F)

##### STEP 8: Calculate MPC and KDE
####################################################################################################
#
