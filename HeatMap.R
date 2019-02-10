library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(RColorBrewer)

## Load data
source('DataProcessing-Movebank.r')
datax <- data

# Add seasons
datax$season <- rep('NA')
datax[which(datax$mth >= 3 & datax$mth < 6),]$season <- 'Fall'
datax[which(datax$mth >= 6 & datax$mth < 9),]$season <- 'Winter'
datax[which(datax$mth >= 9 & datax$mth < 12),]$season <- 'Spring'
datax[which(datax$mth == 12 | datax$mth <= 2),]$season <- 'Summer'

## Create birdyear
# Add field with last two digits of year
datax$yr_small <- strftime(x = datax$dttz, format = '%y', tz = tzOffset)
datax$yr_small <- as.numeric(datax$yr_small)

datax <- datax %>% 
  # Make seasons # do not need first mutate if have already created seasons above
  # mutate(
  #   season = case_when(
  #     mnth >= 3 & mnth < 6 ~ 'Fall',
  #     mnth >= 6 & mnth < 9 ~ 'Winter',
  #     mnth >= 9 & mnth < 12 ~ 'Spring',
  #     mnth == 12 & mnth <= 2 ~ 'Summer' ) ) %>%
  # # ... the bird year is
  mutate (
    bird_year = case_when(
      season == 'Summer' ~ (yr_small), 
      season == 'Fall' | season == 'Winter' | season == 'Spring' ~ (yr_small-1) ) )

# Final column to make the full bird year start/end
datax <- datax %>%  
  mutate (BY = paste('BY',datax$bird_year, datax$bird_year + 1, sep = '') )

## Create map
mapZoom <- 9 # KJH!! Adjust this to fit all the data (20 is all the way zoomed in)
# Plot the extent of this map (NOTE: must adjust zoom parameter)
myMap <-  get_googlemap(center = c(lon = mean(c(min(datax$long), max(datax$long))),  lat = mean(c(min(datax$lat),max(datax$lat)))),
                        zoom = mapZoom, 
                        size = c(640,400),
                        scale = 2,
                        extent="device",
                        maptype="hybrid", #"terrain"
                        style = c(feature = "all", element = "labels", visibility = "off"),
                        key="AIzaSyAHRpFoa86K8IXnsIo1H3fLjWyhorawZew")

## Multiple frames per plot
## Do not need this if using facet_wrap or facet_grid in ggplot
# par(mfrow=c(4,2))
# require(gridExtra)

# First attempt to plot map
m1 <-ggmap(myMap)+
  stat_density2d(data=datax,aes(x=as.numeric(long),y=as.numeric(lat),fill=..level..,alpha=..level..), geom = "polygon") +
  # scale_fill_gradientn(colours = c("steelblue", "tomato"), name="Density") +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral"))) +
  geom_point(aes(colour = factor(data$season)),alpha=0.8) +
  # scale_alpha_continuous(range=c(0.1,0.5)) +
  facet_wrap( ~ BY) +
  guides(size=FALSE, alpha = FALSE) +
  theme(legend.position="right", 
        plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Map of Location Density") +
  # coord_fixed(1.3) + 
  labs(
    x = "Longitude",
    y = "Latitude"
  )
m1

# Second attempt to plot map
m2 <- ggplot(datax, aes(x = as.numeric(long), y = as.numeric(lat))) +
  geom_point()+
  xlim(-61.5, -59.8) +
  ylim(-51.5, -50.90) +
  # geom_density_2d()
  facet_grid(.~BY) +
  stat_density_2d(aes(fill = stat(nlevel)), geom = "polygon")+ 
  scale_fill_viridis_c()+
  ggtitle("Location Density Estimate Per Bird Year (summer to spring)") +
  labs(
    x = "Longitude",
    y = "Latitude"
  )
m2

# Third attempt to plot map
m3 <-ggmap(myMap)+
  geom_point(datax, aes(x = as.numeric(long), y = as.numeric(lat)))+
  xlim(-61.5, -59.8) +
  ylim(-51.5, -50.90) +
  # geom_density_2d()
  facet_grid(.~BY) +
  stat_density_2d(aes(fill = stat(nlevel)), geom = "polygon")+ 
  scale_fill_viridis_c()
m3

#save map output as .png
ggsave(sprintf("%s_Map.png",name), plot = last_plot(), device = "png",
       scale = 2, width = 7, height = 5, units = "in", dpi = 300, limitsize = F)