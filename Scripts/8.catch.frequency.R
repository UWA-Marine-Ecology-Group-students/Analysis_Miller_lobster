# Explore catch data----

rm(list=ls()) #clear memory

# librarys----
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(readr)

# Study name----
study<-"frequency.plot"

# Set work directory----
work.dir=("~/GitHub/Analysis_Miller_WRL") #for Tim's github
work.dir=("~/workspace/Analysis_Miller_WRL") #for ecocloud server

# Set sub-directories----
data.dir=paste(work.dir,"Data",sep="/")
plot.dir=paste(work.dir,"Plots",sep="/")

#Import data----
dat.comb<-gs_title("Lobster_Data_All_Combined")%>% 
  gs_read_csv(ws = "Sheet1" )%>%
  filter(!is.na(Tag.number))%>% #Filter out individuals with no tag.no
  filter(Tag.number!="CT")%>%   #Filter out weird 'CT' tags from Oscars data  
  filter(!is.na(Carapace.length))%>% #Filter out individuals with no length measurements
  mutate(mini.site=Site)%>%
  mutate(Site=str_replace_all(.$Site,c("Seven Mile Beach"= "Seven Mile","Little Horseshoe"="Cliff Head", "Cliff Head North"="Cliff Head","Cliff Head Mid"= "Cliff Head","Cliff Head South"="Cliff Head","Cliff Head OUT1"= "Cliff Head","CHM"="Cliff Head", "Davids Marks"="Cliff Head","CHM"= "Cliff Head", "CHS"="Cliff Head", "CHN"="Cliff Head", "Jim Bailey"="Irwin Reef", "Long Reef"="Irwin Reef", "South Dummy"="Irwin Reef","South Rig"= "Irwin Reef","Whites Lump"= "Irwin Reef","WP"= "Irwin Reef","Whitepoint"="Irwin Reef")))%>%
  mutate(mini.site=str_replace_all(.$mini.site, c("Jim Bailey"="White Point" ,"WP"="White Point" , "Whitepoint"="White Point" , "CHS"="Cliff Head South","CHM"="Cliff Head Mid","CHN"="Cliff Head North", "Seven Mile Beach"="Seven Mile.out")))%>%
  filter(!Source== "Fisheries" & !Source== "UWA") %>%
  filter(is.na(Site)| Site!="Rivermouth")%>% 
  mutate(Trip=paste("T",Trip,sep=""))%>%
  select(Trip, Date,Tag.number, Carapace.length, Site, Sex, Colour, Total.damage, mini.site, Longitude, Latitude)%>% 
  glimpse()


#Bring in Ben's Seven Mile data----

dat.smb <- gs_title("Lobster_Data_Fisheries_SMB")%>%
  gs_read_csv("Dat.smb")%>%
  mutate(Tag.number=as.character(Tag.number))%>%
  mutate(Site=str_replace_all(.$Site,c("Seven Mile Beach"="Seven Mile")))%>%
  mutate(Trip=paste("T",Trip,sep=""))%>%
  mutate(mini.site="Seven Mile.in")%>%
  glimpse()

#Filter out Jan-April for Seven Mile----
dat.smb.edit<-dat.smb%>%
  mutate(month=format(as.Date(Date),'%m'))%>%
  mutate(month=month((as_date(Date))))%>%
  filter(month%in%c(5:12))%>% #Remove Jan-April
  select(Trip, Date,Tag.number, Carapace.length, Site, Sex, Colour, Total.damage, mini.site, Longitude, Latitude)%>% 
  glimpse()


catch.data <-rbind(dat.comb, dat.smb.edit)%>%
  mutate(Count=1)%>%
  dplyr::rename(location=Site)%>%
  dplyr::rename(site=mini.site)%>%
  filter(!is.na(location))%>%
  filter(!Sex=="U" &!Sex=="NA" & !Sex=="UNKNOWN")%>%
  glimpse()


# Plotting Themes ----
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=12),
    text=element_text(size=12),
    strip.text.y = element_text(size = 12,angle = 270),
    axis.title.x=element_text(vjust=0.3, size=12),
    axis.title.y=element_text(vjust=0.6, angle=90, size=12),
    axis.text.x=element_text(size=10,colour="black"),
    axis.text.y=element_text(size=10,colour="black"),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.ticks.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank(),
    plot.background = element_blank()) # Brooke added


#Frequency plots----
glimpse(catch.data)


catch.plot <- ggplot(data=catch.data, aes(Carapace.length))+
  #geom_freqpoly(binwidth=2)+
  # geom_line(aes(y = Count, colour = Sex))+
  #geom_histogram(binwidth = 2, color="black", fill="gray")+
  geom_histogram(binwidth=2, aes(colour=Sex, fill=Sex), alpha=0.4)+
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+
  Theme1+
  coord_cartesian(xlim = c(30, 100))+
  scale_x_continuous(breaks= c(30, 40, 50, 60, 70, 80, 90, 100))+
  ylab("Frequency")+
  xlab("Carapace length")+
  theme(strip.text = element_text(size=12, face="plain"))+
  facet_wrap(~location)
catch.plot


#Cool gradient plot
ggplot(catch.rate, aes(x = `Mean Temperature [F]`, y = `Month`)) +
  geom_density_ridges_gradient(aes(fill = ..x..), scale = 3, size = 0.3) +
  scale_fill_gradientn(colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"),name = "Temp. [F]")+
  labs(title = 'Temperatures in Lincoln NE') 


barfill <- "#4271AE"
barlines <- "#1F3552"

p7 <- ggplot(airquality, aes(x = Ozone)) +
  geom_histogram(aes(y = ..count..), binwidth = 5,
                 colour = barlines, fill = barfill) +
  scale_x_continuous(name = "Mean ozone in\nparts per billion",
                     breaks = seq(0, 175, 25),
                     limits=c(0, 175)) +
  scale_y_continuous(name = "Count") +
  ggtitle("Frequency histogram of mean ozone") +
  theme_bw()




#Catch frequency per Trip, location, sex

av.catch.data<-catch.data%>%
  filter(!Trip=="T0" & !Trip=="T10")%>%
  select(Trip, location, Sex, Count)%>%
  glimpse()


av.test <- av.catch.data%>%
  group_by(Trip, location, Sex)%>%
  glimpse()
  

av <- dplyr::summarise(av.test, catch=n())%>%
  glimpse()


av.plot<-ggplot(av, aes(x=Trip, y=catch, fill=location))+
  geom_bar(stat="identity")
av.plot




#create dataframe for 'initial' tags
# lob.dat<- lob.dat%>%
#   arrange(Date)%>%
#   glimpse()
# 
# lob.initial <- lob.dat%>%
#   select(Date, Tag.number, Source, Carapace.length, location, Sex, Colour,Trip,Total.damage, site, Longitude, Latitude)%>%
#   dplyr::distinct(Tag.number,.keep_all = TRUE)%>% #keeps only the first tag cl, filters out the duplicates (recaptues)
#   dplyr::rename(initial.cl=Carapace.length)%>%
#   mutate(month=format(as.Date(Date),'%m'))%>%
#   mutate(month=month((as_date(Date))))%>%
#   mutate(year=format(as.Date(Date),'%Y'))%>%
#   mutate(year=year((as_date(Date))))%>%
#   glimpse()
# 
# 
# lob.initial%>%
#   filter(Source!="Oscar")%>%
#   group_by(month)%>%
#   dplyr::summarise(Total=n())%>%
#   glimpse()
# 
# 
# 
# #Now recpatures
# lob.recap <- lob.dat[duplicated(lob.dat$Tag.number), ]%>%
#   mutate(month=format(as.Date(Date),'%m'))%>%
#   mutate(month=month((as_date(Date))))%>%
#   select(Date,Tag.number, Carapace.length, Source, month,location, Sex, Colour,Trip, Total.damage, site, Longitude, Latitude)%>%
#   dplyr::rename(recap.Date = Date)%>%
#   dplyr::rename(recap.cl=Carapace.length)%>%
#   glimpse()
# 
# 
# l <- lob.recap%>%
#   filter(Source!="Oscar")%>%
#   glimpse()
# 
# length(l$Tag.number)
# length(lob.recap$Tag.number)
# 
# 
# #Figure out recaptures...hmmm....
# #Will need information on release month and then recapture month
# #I hate this lalalala
# lob.rr<- left_join(lob.initial, lob.recap, by="Tag.number")%>%
#   glimpse()
# 
# lob.rr%>%
#   filter(Source.x!="Oscar")%>%
#   filter(month.x=="12")%>%
#   group_by(month.y)%>%
#   dplyr::summarise(Total=n())%>%
#   glimpse()
#  
# 



  


### Spatial Plots ----
# # Plot legal and sublegal spatially 
# setwd(map.dir)
# dir()
setwd(map.dir)
setwd("~/Google Drive/Project_WRL_low-catch zone/Fieldwork and Reporting/03_Trapping/Analysis_WRL_Reds_2018/Map Layers")

au <-readOGR("aust_cd66states.shp")
au <-fortify(au)

Theme_spatial <-
  theme(strip.text.x = element_text(size = 12,angle = 0,family="serif"),
        strip.text.y = element_text(size = 12,family="serif"),
        axis.title.x=element_text(vjust=-0.0, size=12,family="serif"),
        axis.title.y=element_text(vjust=0.0, angle=90, size=12,family="serif"),
        axis.text.x=element_text(size=10, angle=0,family="serif"),
        axis.text.y=element_text(size=10,family="serif"),
        legend.text = element_text(size=12,family="serif"),
        legend.title = element_text(size=12,family="serif"),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0, vjust=1,family="serif",face = "italic",size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"),
        panel.border = element_rect(fill=NA,colour = "black"),
        plot.margin = unit(x=c(0.1,0.1,0.1,0.1),units="cm"))

dat.map.legal<-dat.pot%>%
  dplyr::select(Trap.ID,Pot.Number,Day,Site.Name,Date.Recovered,Longitude.x,Latitude.y)%>%
  full_join(dat.legal)%>%
  filter(!Legal=="All")%>%
  rename(lon=Longitude.x,lat=Latitude.y)%>%
  mutate(lon=as.numeric(lon))%>%
  mutate(lat=as.numeric(lat))#%>%
#full_join(johns)%>%
#mutate(Johns=str_replace_all(.$Johns,c("Yes"="John's pot","No"="Empty pot")))

dat.map.sublegal<-dat.pot%>%
  dplyr::select(Trap.ID,Pot.Number,Day,Site.Name,Date.Recovered,Longitude.x,Latitude.y)%>%
  full_join(dat.sublegal)%>%
  filter(!Legal=="All")%>%
  rename(lon=Longitude.x,lat=Latitude.y)%>%
  mutate(lon=as.numeric(lon))%>%
  mutate(lat=as.numeric(lat))#%>%

dat.map

str(dat.map)
summary(dat.map)
## Make map ----
center<- c(114.9,-29.38)

gg.map <- ggmap(get_map(location = center, source = "stamen", maptype = "toner-lite", zoom = 11))


## Stamen theme ----

spatial.legal<-gg.map+
  geom_point(data=filter(dat.map.legal,Abundance>0),aes(lon,lat,size=Abundance),shape=21,colour="black",fill="dodgerblue",alpha=0.3)+
  geom_point(data=filter(dat.map.legal,Abundance<1),aes(lon,lat),shape=21,colour="black",fill="white",size=1)+
  scale_size_continuous(range = c(1,10), breaks=seq(min(dat.map.legal$Abundance),max(dat.map.legal$Abundance), by=(max(dat.map.legal$Abundance)/4)))+
  xlab('Longitude')+
  ylab('Latitude')
spatial.legal

spatial.sublegal<-gg.map+
  geom_point(data=filter(dat.map.sublegal,Abundance>0),aes(lon,lat,size=Abundance),shape=21,colour="black",fill="dodgerblue",alpha=0.3)+
  geom_point(data=filter(dat.map.sublegal,Abundance<1),aes(lon,lat),shape=21,colour="black",fill="white",size=1)+
  scale_size_continuous(range = c(1,10), breaks=seq(min(dat.map.sublegal$Abundance),max(dat.map.sublegal$Abundance), by=(max(dat.map.sublegal$Abundance)/4)))+
  xlab('Longitude')+
  ylab('Latitude')
spatial.sublegal

setwd(plots.dir)
ggsave(spatial.legal,file=paste(study,"trip4.stamen.legal", "png",sep = "."), width = 20, height = 16,units = "cm")
ggsave(spatial.sublegal,file=paste(study,"trip4.stamen.sublegal", "png",sep = "."), width = 20, height = 16,units = "cm")

## Using google theme ----
gg.map<-ggmap(get_map(location=center, source = "google", zoom = 11))
spatial.legal<-gg.map+
  geom_point(data=filter(dat.map.legal,Abundance>0),aes(lon,lat,size=Abundance),shape=21,colour="black",fill="dodgerblue",alpha=0.3)+
  geom_point(data=filter(dat.map.legal,Abundance<1),aes(lon,lat),shape=21,colour="black",fill="white",size=1)+
  scale_size_continuous(range = c(1,10), breaks=seq(min(dat.map.legal$Abundance),max(dat.map.legal$Abundance), by=(max(dat.map.legal$Abundance)/4)))+
  xlab('Longitude')+
  ylab('Latitude')
spatial.legal

spatial.sublegal<-gg.map+
  geom_point(data=filter(dat.map.sublegal,Abundance>0),aes(lon,lat,size=Abundance),shape=21,colour="black",fill="dodgerblue",alpha=0.3)+
  geom_point(data=filter(dat.map.sublegal,Abundance<1),aes(lon,lat),shape=21,colour="black",fill="white",size=1)+
  scale_size_continuous(range = c(1,10), breaks=seq(min(dat.map.sublegal$Abundance),max(dat.map.sublegal$Abundance), by=(max(dat.map.sublegal$Abundance)/4)))+
  xlab('Longitude')+
  ylab('Latitude')
spatial.sublegal

setwd(plots.dir)
ggsave(spatial.legal,file=paste(study,"trip3.google.legal", "png",sep = "."), width = 20, height = 16,units = "cm")
ggsave(spatial.sublegal,file=paste(study,"trip3.google.sublegal", "png",sep = "."), width = 20, height = 16,units = "cm")


## Testing leaflet (I think this is good for showing in meetings not good for print) ----
library(leaflet)
test.legal<-leaflet(dat.map.legal)%>%
  addTiles() %>%
  addCircleMarkers(lat=subset(dat.map.legal, Abundance>0)$lat,lng=subset(dat.map.legal, Abundance>0)$lon,radius = (dat.map.legal$Abundance/max(dat.map.legal$Abundance))*40,stroke = FALSE, fillOpacity = 0.4)%>%
  addCircleMarkers(lat=subset(dat.map.legal, Abundance==0)$lat,lng=subset(dat.map.legal, Abundance==0)$lon,radius = 1,color="white")

test.legal

test.sublegal<-leaflet(dat.map.sublegal)%>%
  addTiles() %>%
  addCircles(lat=subset(dat.map.sublegal, Abundance>0)$lat,lng=subset(dat.map.sublegal, Abundance>0)$lon,radius = (dat.map.legal$Abundance/max(dat.map.legal$Abundance))*4000,stroke = FALSE, fillOpacity = 0.4)%>%
  addCircleMarkers(lat=subset(dat.map.sublegal, Abundance==0)$lat,lng=subset(dat.map.sublegal, Abundance==0)$lon,radius = 1,color="white")

test.sublegal


dat.map<-bind_rows(dat.map.legal,dat.map.sublegal)

qpal <- colorFactor(c("red", "blue"), domain = c("Legal", "Sublegal"))

test.leaflet<-leaflet(dat.map)%>%
  addTiles() %>%
  addCircleMarkers(lat=subset(dat.map, Abundance>0)$lat,lng=subset(dat.map, Abundance>0)$lon,radius = (dat.map$Abundance/max(dat.map$Abundance))*40,stroke = FALSE, fillOpacity = 0.4,color=~qpal(Legal))%>%
  addCircleMarkers(lat=subset(dat.map, Abundance==0)$lat,lng=subset(dat.map, Abundance==0)$lon,radius = 1,color=~qpal(Legal))%>%
  addLegend(pal=qpal,values = ~Legal)

test.leaflet

## Bring in TIF ----
setwd(map.dir)
map <- raster("AUS00752P0.tif")

# Colours for map ----
coltab <- colortable(map)
# turn raster into data.frame and copy the colortable
map.df <- data.frame(rasterToPoints(map))
colTab <- colortable(map)

# give the colors their apropriate names:
names(colTab) <- 0:(length(colTab) - 1) 

# only define the colors used in the raster image
from <- min(map.df[[3]], na.rm = T)+1 
to <- max(map.df[[3]], na.rm = T)+1
used_cols <- colTab[from:to] 

summary(dat.pot.4)

## plot - facetted by legal size 
# legal.map2<-gplot(map, maxpixels =100000000) + #, maxpixels = 5e10
#   geom_tile(aes(fill = value)) +
#   facet_wrap(~ variable) +
#   scale_fill_gradientn(colours=used_cols, guide=FALSE)+
#   coord_equal()+
#   Theme_spatial+
#   #geom_point(data=filter(dat.map,Abundance<1&Johns=="No"),aes(lon,lat),shape=4,colour="red",size=2)+
#   geom_point(data=filter(dat.map,Abundance<1),aes(lon,lat),shape=2,colour="red",size=2)+
#   geom_point(data=filter(dat.map,Abundance>0),aes(lon,lat,size=Abundance),shape=21,colour="black",fill="dodgerblue",alpha=0.3)+
#   scale_size_continuous(range = c(1,14), breaks=seq(0,80, by=10), name = "Abundance per pot")+
#   Theme_spatial+
#   xlab('Longitude')+
#   ylab('Latitude')+
#   coord_cartesian(xlim = c(114.7,115.17),ylim=c(-29.6,-29)) +
#   facet_wrap(~Legal,nrow=1)
# legal.map2
# 
# str(dat.map)


## These take a while to save and run, to increase the quality increase the pixels (more=longer to save)----
legal.map.legal<-gplot(map, maxpixels = 1e7) + #, maxpixels = 5e101000000000000000
  geom_tile(aes(fill = value)) +
  scale_fill_gradientn(colours=used_cols, guide=FALSE)+
  coord_equal()+
  Theme_spatial+
  geom_point(data=filter(dat.map.legal,Abundance>0),aes(lon,lat,size=Abundance),shape=21,colour="black",fill="dodgerblue",alpha=0.3)+
  geom_point(data=filter(dat.map.legal,Abundance<1),aes(lon,lat),shape=21,colour="black",fill="black",size=1)+
  scale_size_continuous(range = c(3,10),breaks=seq(min(dat.map.legal$Abundance),max(dat.map.legal$Abundance), by=(max(dat.map.legal$Abundance)/4)), name = "Abundance per pot")+
  Theme_spatial+
  xlab('Longitude')+
  ylab('Latitude')+
  coord_cartesian(xlim = c(114.7,115.17),ylim=c(-29.6,-29))
legal.map.legal

legal.map.sublegal<-gplot(map, maxpixels =1e7) + #, maxpixels = 5e10
  geom_tile(aes(fill = value)) +
  #facet_wrap(~ variable) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())+
  scale_fill_gradientn(colours=used_cols, guide=FALSE)+
  coord_equal()+
  Theme_spatial+
  geom_point(data=filter(dat.map.sublegal,Abundance>0),aes(lon,lat,size=Abundance),shape=21,colour="black",fill="dodgerblue",alpha=0.3)+
  geom_point(data=filter(dat.map.sublegal,Abundance<1),aes(lon,lat),shape=21,colour="black",fill="black",size=1)+
  scale_size_continuous(range = c(3,10), breaks=seq(min(dat.map.sublegal$Abundance),max(dat.map.sublegal$Abundance), by=(max(dat.map.sublegal$Abundance)/4)), name = "Abundance per pot")+
  Theme_spatial+
  xlab('Longitude')+
  ylab('Latitude')+
  coord_cartesian(xlim = c(114.7,115.17),ylim=c(-29.6,-29))
legal.map.sublegal

cropped.legal<-legal.map.legal+
  coord_cartesian(xlim = c(114.8,115),ylim=c(-29.6,-29.2))

cropped.sublegal<-legal.map.sublegal+
  coord_cartesian(xlim = c(114.8,115),ylim=c(-29.6,-29.2))

setwd(plots.dir)
ggsave(cropped.legal,file=paste(study,"trip4.tif.legal", "png",sep = "."), width = 15, height = 17,units = "cm")
ggsave(cropped.sublegal,file=paste(study,"trip4.tif.sublegal", "png",sep = "."), width = 15, height = 17,units = "cm")


setwd(plots.dir)
gc()
memory.limit()
memory.limit(size=56000)
#ggsave(legal.map.legal,file=paste(study,"legal.map.legal", "png",sep = "."), width = 15, height = 17,units = "cm")
#ggsave(legal.map.sublegal,file=paste(study,"legal.map.sublegal", "png",sep = "."), width = 15, height = 17,units = "cm")

#ggsave(legal.map.legal,file=paste(study,"legal.map.legal.small", "png",sep = "."), width = 10, height = 12,units = "cm")
#ggsave(legal.map.sublegal,file=paste(study,"legal.map.sublegal.small", "png",sep = "."), width = 10, height = 12,units = "cm")



#ggsave(legal.map2,file=paste(study,"legal.spatial.plot.with.Johns.2", "png",sep = "."), width = 40, height = 20,units = "cm")


legal.map<-gg.map+
  geom_point(data=filter(dat.map,Abundance<1),aes(lon,lat,size=Abundance,fill=Johns),shape=21,colour="black",alpha=0.5)+
  geom_point(data=filter(dat.map,Abundance<1),aes(lon,lat,size=Abundance,fill=Johns),shape=21,colour="black",alpha=0.5)+
  geom_point(data=filter(dat.map,Abundance>0),aes(lon,lat,size=Abundance),shape=21,colour="black",fill="dodgerblue",alpha=0.3)+
  scale_size_continuous(range = c(1,10), breaks=seq(0,40, by=10), name = "Abundance per pot")+
  Theme_spatial+
  xlab('Longitude')+
  ylab('Latitude')+facet_wrap(~Legal)
legal.map

setwd(plots.dir)
summary(dat.map)
#ggsave(legal.map,file=paste(study,"legal.spatial.plot.with.Johns", "png",sep = "."), width = 25, height = 15,units = "cm")
