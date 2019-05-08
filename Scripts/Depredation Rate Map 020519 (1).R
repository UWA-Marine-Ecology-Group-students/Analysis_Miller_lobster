#Script from Charlotte

# Libraries required
detach("package:plyr", unload=TRUE)#will error - no worries
library(tidyr)
library(dplyr)
options(dplyr.width = Inf) #enables head() to display all coloums
library(ggplot2)
install.packages("readxl")
library(readxl)
library(googlesheets)
library(zoo) #for filling in missing values
library(rgeos)
library(rgdal)
library(gtable)
library(gridExtra)
install.packages("jpeg")
library(jpeg)
library(grid)
install.packages("png")
library(png)
library(maptools)
install.packages("maps")
library(maps)
library(GISTools)
library(scales)

# Set directories----
rm(list=ls()) #clear memory
study<-"lobster"

# Set your specific work.dir here-
work.dir=("Z:/Analysis_Miller_lobster")
# work.dir=("/Volumes/GoogleDrive/My Drive/Masters_Charlotte/Analysis/Model Outputs")


## Sub directories ----

data.dir<-paste(work.dir,"Data",sep="/")
map.dir<-paste(work.dir,"Maps",sep="/")
plots.dir<-paste(work.dir,"Plots",sep="/")

#import data

# dat<-read.csv("Carnarvon Data No Crabs TIDIED.csv")

# setwd("/Volumes/GoogleDrive/My Drive/Masters_Charlotte/Analysis/Map layers")
# dir()
setwd("Z:/Analysis_Miller_lobster/Map layers")

auspoly<-readOGR("World.shp", layer = "World")
ausmap<-fortify(auspoly, region = "parent_id")


# Create a basic map of Aus

map <- ggplot(data=ausmap, aes(long,lat))+ ### the map needs to be plotted using ggplot in order to annotate
  geom_polygon(aes(x = long, y = lat, group = group), colour = 'transparent',     fill = 'lightgrey') + # ADDING    sanctuaries
  geom_path(alpha=0)+
  #annotate("text", x = 114.37, y = -21.3, face="bold", angle=40, size=1.5,label = "200m")+
  #annotate("text", x = 115.1, y = -21.1, face="bold", angle=60, size=1.5,label = "30m")+
  theme_classic()+
  theme_bw()
map

# Save the map

ggsave(filename="mapdepredationrate.pdf", plot=map)


## Theme for plots----
Generictheme<-theme(panel.grid.major = element_blank(),   
                    legend.justification=c(1,0), legend.position=c(0.85,0.6),
                    legend.background = element_rect(fill="transparent"),
                    panel.grid.minor = element_blank(),                                               
                    strip.background = element_blank(),
                    legend.key = element_blank(),
                    legend.key.height=unit(1.25,"line"),
                    legend.title = element_text(size=14),
                    panel.border = element_rect(colour = "black"),
                    legend.text = element_text(size=14),
                    axis.text.x=element_text(size=18),
                    axis.text.y=element_text(size=18),
                    axis.title.x=element_text(size=23),
                    axis.title.y=element_text(size=23),
                    axis.ticks.length=unit(0.1,"cm"),
                    plot.margin = unit(c(1,1,2,2),"lines"),
                    plot.title=element_text(size=8,face="italic"))

Generictheme2<-theme(panel.grid.major = element_blank(),   
                     legend.justification=c(1,0), legend.position=c(0.85,0.6),
                     legend.background = element_rect(fill="transparent"),
                     panel.grid.minor = element_blank(),                                               
                     strip.background = element_blank(),
                     legend.key = element_blank(),
                     legend.key.height=unit(2,"line"),
                     legend.title = element_text(size=20),
                     panel.border = element_rect(colour = "black"),
                     legend.text = element_text(size=17),
                     axis.text.x=element_text(size=18),
                     axis.text.y=element_text(size=18),
                     axis.title.x=element_text(size=23),
                     axis.title.y=element_text(size=23),
                     axis.ticks.length=unit(0.1,"cm"),
                     plot.margin = unit(c(1,1,2,2),"lines"),
                     plot.title=element_text(size=8))

Generictheme3<-theme(panel.grid.major = element_blank(),   
                     legend.justification=c(1,0), legend.position=c(0.85,0.6),
                     legend.background = element_rect(fill="transparent"),
                     panel.grid.minor = element_blank(),                                               
                     strip.background = element_blank(),
                     legend.key = element_blank(),
                     legend.key.height=unit(2,"line"),
                     legend.title = element_text(size=20),
                     panel.border = element_rect(colour = "black"),
                     legend.text = element_text(size=17),
                     axis.text.x=element_text(size=18),
                     axis.text.y=element_text(size=18),
                     axis.title.x=element_text(size=23),
                     axis.title.y=element_text(size=23),
                     axis.ticks.length=unit(0.1,"cm"),
                     plot.margin = unit(c(1,1,2,2),"lines"),
                     plot.title=element_text(size=8,face="italic"))




#FUNCTIONS NEEDED TO ADD A SCALE BAR ----

#
# Result #
#--------#
# Return a list whose elements are :
#   - rectangle : a data.frame containing the coordinates to draw the first rectangle ;
#   - rectangle2 : a data.frame containing the coordinates to draw the second rectangle ;
#   - legend : a data.frame containing the coordinates of the legend texts, and the texts as well.
#
# Arguments : #
#-------------#
# lon, lat : longitude and latitude of the bottom left point of the first rectangle to draw ;
# distanceLon : length of each rectangle ;
# distanceLat : width of each rectangle ;
# distanceLegend : distance between rectangles and legend texts ;
# dist.units : units of distance "km" (kilometers) (default), "nm" (nautical miles), "mi" (statute miles). 

library(ggsn)

#northSymbols()

createScaleBar <-
  function(lon,lat,distanceLon,distanceLat,distanceLegend, dist.units =
             "km"){
    # First rectangle
    bottomRight <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distanceLon, dist.units = dist.units, model = "WGS84")
    
    topLeft <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distanceLat, dist.units = dist.units, model = "WGS84")
    rectangle <- cbind(lon=c(lon, lon, bottomRight[1,"long"], bottomRight[1,"long"], lon),
                       lat = c(lat, topLeft[1,"lat"], topLeft[1,"lat"],lat, lat))
    rectangle <- data.frame(rectangle, stringsAsFactors = FALSE)
    
    # Second rectangle t right of the first rectangle
    bottomRight2 <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distanceLon*2, dist.units = dist.units, model = "WGS84")
    rectangle2 <- cbind(lon = c(bottomRight[1,"long"], bottomRight[1,"long"], bottomRight2[1,"long"], bottomRight2[1,"long"],
                                bottomRight[1,"long"]),
                        lat=c(lat, topLeft[1,"lat"], topLeft[1,"lat"], lat, lat))
    rectangle2 <- data.frame(rectangle2, stringsAsFactors = FALSE)
    
    # Now let's deal with the text
    onTop <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distanceLegend, dist.units = dist.units, model = "WGS84")
    onTop2 <- onTop3 <- onTop
    onTop2[1,"long"] <- bottomRight[1,"long"]
    onTop3[1,"long"] <- bottomRight2[1,"long"]
    
    legend <- rbind(onTop, onTop2, onTop3)
    legend <- data.frame(cbind(legend, text = c(0, distanceLon, distanceLon*2)), stringsAsFactors = FALSE, row.names = NULL)
    return(list(rectangle = rectangle, rectangle2 = rectangle2, legend = legend)) } 

#We also need a function to obtain the coordinates of the North arrow:

#
# Result #
#--------#
# Returns a list containing :
#   - res : coordinates to draw an arrow ;
#   - coordinates of the middle of the arrow (where the "N" will be plotted).
#
# Arguments : #
#-------------#
# scaleBar : result of createScaleBar() ;
# length : desired length of the arrow ;
# distance : distance between legend rectangles and the bottom of the arrow ;
# dist.units : units of distance "km" (kilometers) (default), "nm" (nautical miles), "mi" (statute miles). 
createOrientationArrow <-
  function(scaleBar, length, distance = 1, dist.units = "km"){
    lon <- scaleBar$rectangle2[1,1]
    lat <- scaleBar$rectangle2[1,2]
    
    # Bottom point of the arrow
    begPoint <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance, dist.units = dist.units, model = "WGS84")
    lon <- begPoint[1,"long"]
    lat <- begPoint[1,"lat"]
    
    # Let us create the endpoint
    onTop <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = length, dist.units = dist.units, model = "WGS84")
    
    leftArrow <- gcDestination(lon = onTop[1,"long"], lat = onTop[1,"lat"], bearing = 225, dist = length/5, dist.units =
                                 dist.units, model = "WGS84")
    
    rightArrow <- gcDestination(lon = onTop[1,"long"], lat = onTop[1,"lat"], bearing = 135, dist = length/5, dist.units =
                                  dist.units, model = "WGS84")
    
    res <- rbind(
      cbind(x = lon, y = lat, xend = onTop[1,"long"], yend = onTop[1,"lat"]),
      cbind(x = leftArrow[1,"long"], y = leftArrow[1,"lat"], xend = onTop[1,"long"], yend = onTop[1,"lat"]),
      cbind(x = rightArrow[1,"long"], y = rightArrow[1,"lat"], xend = onTop[1,"long"], yend = onTop[1,"lat"]))
    
    res <- as.data.frame(res, stringsAsFactors = FALSE)
    
    # Coordinates from which "N" will be plotted
    coordsN <- cbind(x = lon, y = (lat + onTop[1,"lat"])/2)
    
    return(list(res = res, coordsN = coordsN)) } #The last function enables the user to draw the elements:

#
# Result #
# This function enables to draw a scale bar on a ggplot object, and optionally an orientation arrow #
# Arguments : #
#-------------#
# lon, lat : longitude and latitude of the bottom left point of the first rectangle to draw ;
# distanceLon : length of each rectangle ;
# distanceLat : width of each rectangle ;
# distanceLegend : distance between rectangles and legend texts ;
# dist.units : units of distance "km" (kilometers) (by default), "nm" (nautical miles), "mi" (statute miles) ;
# rec.fill, rec2.fill : filling colour of the rectangles (default to white, and black, resp.);
# rec.colour, rec2.colour : colour of the rectangles (default to black for both);
# legend.colour : legend colour (default to black);
# legend.size : legend size (default to 3);
# orientation : (boolean) if TRUE (default), adds an orientation arrow to the plot ;
# arrow.length : length of the arrow (default to 500 km) ;
# arrow.distance : distance between the scale bar and the bottom of the arrow (default to 300 km) ;
# arrow.North.size : size of the "N" letter (default to 6). 
scaleBar <- function(lon, lat, distanceLon, distanceLat, distanceLegend,
                     dist.unit = "km", rec.fill = "white", rec.colour = "black", rec2.fill = "black", rec2.colour = "black", legend.colour = "black", legend.size = 2, orientation = TRUE, arrow.length = 500, arrow.distance = 300, arrow.North.size = 6){
  laScaleBar <- createScaleBar(lon = lon, lat = lat, distanceLon = distanceLon, distanceLat = distanceLat, distanceLegend = distanceLegend, dist.unit = dist.unit)
  # First rectangle
  rectangle1 <- geom_polygon(data = laScaleBar$rectangle, aes(x = lon, y = lat), fill = rec.fill, colour = rec.colour)
  
  # Second rectangle
  rectangle2 <- geom_polygon(data = laScaleBar$rectangle2, aes(x = lon, y = lat), fill = rec2.fill, colour = rec2.colour)
  
  # Legend
  scaleBarLegend <- annotate("text", label = paste(laScaleBar$legend[,"text"], dist.unit, sep=""), x =
                               laScaleBar$legend[,"long"], y = laScaleBar$legend[,"lat"], size =
                               legend.size, colour = legend.colour)
  
  res <- list(rectangle1, rectangle2, scaleBarLegend)
  
  if(orientation){# Add an arrow pointing North
    coordsArrow <- createOrientationArrow(scaleBar = laScaleBar, length = arrow.length, distance = arrow.distance, dist.unit =
                                            dist.unit)
    arrow <- list(geom_segment(data = coordsArrow$res, aes(x = x, y = y, xend = xend, yend = yend)), annotate("text", label = "N", x =
                                                                                                                coordsArrow$coordsN[1,"x"], y = coordsArrow$coordsN[1,"y"], size =
                                                                                                                arrow.North.size, colour = "black"))
    res <- c(res, arrow)
  }
  return(res) }




rawdepdata <-map+
  geom_polygon(aes(x = long, y = lat, group = group), colour = 'transparent',     fill = 'grey85') +
  geom_point(aes(Long,Lat,alpha=DepredationRate), size=3 ,data=dat)+
  xlab("\nLongitude ()")+
  ylab("Latitude ()\n")+ 
  coord_cartesian(xlim = c(112.7, 113.8),ylim = c(-25.41,-23.70))+    
  Generictheme  +
  scaleBar(lon = 114.35, lat = -23.85, distanceLon = 20,distanceLat = 2, distanceLegend = -10, dist.unit = "km", rec.fill = "black", rec.colour = "black", rec2.fill = "white", rec2.colour = "white", legend.colour = "black", legend.size = 5, orientation = TRUE, arrow.length = 50, arrow.distance = 70, arrow.North.size = 8) + 
  annotate("text", label="Tb", x=114.01, y=-21.91219, size=5, colour="black") + annotate("text", label="CB", x=113.80, y=-23.15507, size=5, colour="black") + 
  annotate("text", label="Bd", x=114.14, y=-21.83122, size=5, colour="black") + annotate("text", label="Em", x=114.09, y=-21.95584, size=5, colour="black") + 
  annotate("text", label="NMP", x=113.67, y=-22.35, size=5, colour="black", angle=50) + annotate("text", label="NMP", x=113.5, y=-23.6, size=5, colour="black", angle=50) + annotate("text", label="EG", x=114.3, y=-22.15, size=5, colour="black") + 
  theme(legend.position=c(0.54,0.64)) + 
  scale_y_continuous(breaks=seq(-24,-21.5,0.5))


# Now make the plot with the bathy shapefile added

# First read the bathy shapefile
#To create small W.A image

setwd("Z:/Analysis_Miller_lobster/Map layers")

bathy<-readOGR("Bathym_Statewide.shp", layer = "Bathym_Statewide")
bathy<-fortify(bathy, region = "OBJECTID")


WAinsertmap <- ggplot(data=ausmap, aes(long,lat)) + 
  geom_polygon(aes(x = long, y = lat, group = group), colour = 'transparent',     fill = 'grey85') +
  geom_path(alpha=0)+
  coord_cartesian(xlim = c(112, 129),ylim = c(-36,-12)) + 
  annotate("rect", xmin = 114.6, xmax = 115.1, ymin = -29.0, ymax = -29.9, colour="red", alpha = 0, size = 1) + 
  annotate("text", label="Western\n Australia", x=122.5, y=-26, size=4.5, colour="black") + 
  theme(panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.7),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.length=unit(0,"cm"),
        plot.margin=unit(c(0,0,0,0), "cm"),
        plot.title=element_blank()) 


WAinsertmap




# Now make the plot and add the bathy shapefile with the 'geom_polygon' function
# Have to set the original basemap at the start of the script (called 'map') so that the line is white (colour="white"), because the bathy layer already has a line for the coast so don't need two 

library(RColorBrewer)
install.packages("colorRamps")
library(colorRamps)
myPalette <- colorRampPalette(c("lightgoldenrod1", "orange", "darkorange", "orangered", "orangered1", "red1", "red2"))
  colorRamps::matlab.like2 # Set the colour palette to use for the data points (fishing locations) on the map, based on the percentage depredation rate

install.packages("viridis")

  
summary(dat)

##plot finished - just need to work out how to change axis titles into lat and long 
## then add in WA insert map 

rawdepdatabathy <-map +
  #geom_point(aes(Long,Lat,colour=DepredationRate),size=2.8, data=dat) +
  #geom_polygon(aes(x = long, y = lat, group = group), colour = 'transparent',     fill = 'transparent') +
  #scale_colour_viridis_c(name="Depredation rate (%)", option='inferno', direction=-1) +
  xlab("\nLongitude")+
  ylab("Latitude\n")+ 
  coord_cartesian(xlim = c(114.7, 115.1), ylim = c(-29.0, -29.9)) + 
  geom_polygon(aes(x = long, y = lat, group = group), colour = 'transparent',     fill = 'grey85') +
  Generictheme  +
  scaleBar(lon = 115.0, lat = -29.6, distanceLon = 10,distanceLat = 2, distanceLegend = -5, dist.unit = "km", rec.fill = "black", rec.colour = "black", rec2.fill = "white", rec2.colour = "black", legend.colour = "black", legend.size = 3, orientation = TRUE, arrow.length = 12, arrow.distance = 5, arrow.North.size = 6) + 
  # annotate("text", label="20", x=113.29, y=-25.27, size=3, colour="black", angle=50) +
  # annotate("text", label="50", x=113.52, y=-24.81, size=3, colour="black", angle=50) +
  # annotate("text", label="20", x=113.70, y=-25.45, size=3, colour="black", angle=50) +
  # annotate("text", label="20", x=113.35, y=-25.25, size=3, colour="black", angle=40) + 
  # annotate("text", label="20", x=113.43, y=-25.45, size=3, colour="black", angle=40) + 
  # annotate("text", label="150", x=113.41, y=-24.79, size=3, colour="black", angle=40) + 
  # annotate("text", label="150", x=113.09, y=-25.32, size=3, colour="black", angle=85) + 
  # annotate("text", label="150", x=113.232, y=-24.72, size=3, colour="black", angle=60) + 
  # annotate("text", label="150", x=113.35, y=-24.46, size=3, colour="black", angle=70) + 
  # annotate("text", label="350", x=112.83, y=-25.29, size=3, colour="black", angle=85) + 
  # annotate("text", label="350", x=112.64, y=-24.8, size=3, colour="black", angle=85) + 
  theme(legend.position=c(0.34,0.74))+ 
  theme(axis.text=element_text(size = 0.1))+
  #scale_y_continuous(breaks=seq(-29.0,-29.9, 0.1)) + #,0.1 
  #scale_x_continuous(breaks=seq(114.7,115.1, 0.5)) + #,0.5 
  geom_polygon(data=bathy,aes(x=long, y=lat, group=group), show.legend=FALSE, colour="grey30", fill="white", size=0.1, alpha=0.1) + 
  theme(plot.margin=unit(c(1,1,0.5,0.5), "cm"))     

rawdepdatabathy

print(WAinsertmap, vp=viewport(width=0.20, height=0.28, x=0.84, y=0.35))
             
# Now save the final plot by exporting as a pdf in the panel on the right, this saves it as a higher resolution than if you save it as an image 
             
