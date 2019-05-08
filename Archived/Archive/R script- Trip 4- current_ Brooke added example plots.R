# Explore catch data----
rm(list=ls()) #clear memory

# librarys----
library(tidyr)
library(dplyr)
options(dplyr.width = Inf) #enables head() to display all coloums
library(forcats)
library(readxl)
library(googlesheets)
library(stringr)
library(ggmap)
library(ggplot2)
library(cowplot)
library(rgdal)# Brooke added
#install.packages('rasterVis') #AM added (needed for mac?)
library(rasterVis) # Brooked added for tif files
library(raster)

# Study name----
study<-"Reds.Catch"

work.dir=("C:/Users/00097191/Google Drive/MEG/Project_WRL low-catch zone/Fieldwork and Reporting/03_Trapping/Analysis_WRL_Reds_2018")
work.dir=("~/Google Drive/Projects/Project_WRL low-catch zone/Fieldwork and Reporting/03_Trapping/Analysis_WRL_Reds_2018")

## Sub directories ----
data.dir<-paste(work.dir,"Data",sep="/")
map.dir<-paste(work.dir,"Map Layers",sep="/")
plots.dir<-paste(work.dir,"Plots",sep="/")

# Functions----
se <- function(x) sd(x) / sqrt(length(x))
se.min <- function(x) (mean(x)) - se(x)
se.max <- function(x) (mean(x)) + se(x)
sd.min <- function(x) (mean(x)) - sd(x)
sd.max <- function(x) (mean(x)) + sd(x)
scaleFUN <- function(x) sprintf("%.0f", x)

# Import and make data----
#setwd(Data) 
setwd("~/Documents/University/Masters Project/Data") # For Ash
#setwd("~/Google Drive/Analysis_WRL_Reds_2018/Data")
setwd(data.dir)
dir()

#### Trip 2 ----

# Import length data----
dat.length.3 <- read_csv("Lobsters.var.trip3  .csv")%>% # For Ash
  glimpse()

dat.length.2<-gs_title("Data by Trip")%>% # For Brooke to use GoogleSheets
  gs_read_csv(ws = "Lobsters.var.trip2.csv")%>%
  glimpse()

dat.length.2<-dat.length.2%>%
  mutate(Count=1)%>%
  filter(!is.na(Carapace.length))%>%
  mutate(Carapace.length=as.numeric(as.character(Carapace.length)))%>%
  mutate(day.trap=paste(Day,Trap.Number,sep="."))%>%
  glimpse()

write.csv(dat.length.2,"lob.dat.csv")
dat.length.2<-read.csv("lob.dat.csv")

unique(dat.length.2$Trap.ID) # 92 levels


# # Import pot data----
dat.pot.2 <-read_csv("Pot.var.trip2.csv")%>% # For Ash
  mutate(day.trap=paste(Day,Pot.Number,sep="."))%>%
  mutate(Site.Name=str_replace_all(.$Site.Name,c("IR"="Irwin Reef","LH"="Little Horseshoe", "CHin1_"="Cliff Head IN1","CHin2_"="Cliff Head IN2","CHout1_" = "Cliff Head OUT1","CHout2_" = "Cliff Head OUT2", "JB"="Jim Bailey", "GR"="Golden Ridge", "SR"="South Rig", "WL"="Whites Lump")))%>% 
  #  filter(Johns=="No")%>% # turn off if you add in john's data
  glimpse()

dat.pot.2<-gs_title("Data by Trip")%>% # For Brooke to use GoogleSheets
  gs_read_csv(ws = "Pot.var.trip2.csv")%>%
  mutate(day.trap=paste(Day,Pot.Number,sep="."))%>%
  mutate(Site.Name=str_replace_all(.$Site.Name,c("IR"="Irwin Reef","LH"="Little Horseshoe", "CHin1_"="Cliff Head IN1","CHin2_"="Cliff Head IN2","CHout1_" = "Cliff Head OUT1","CHout2_" = "Cliff Head OUT2", "JB"="Jim Bailey", "GR"="Golden Ridge", "SR"="South Rig", "WL"="Whites Lump")))%>% 
  #  filter(Johns=="No")%>% # turn off if you add in john's data
  glimpse()

dat.pot.2$Site.Name
write.csv(dat.pot.2,"dat.pot.csv")
dat.pot.2<-read.csv("dat.pot.csv")
unique(dat.pot.2$Trap.ID) # 97 pots

sites.2<-dat.pot.2%>%
  distinct(Trip,Day,Trap.ID,Site.Name)%>%
  mutate(Trap.ID=as.character(Trap.ID))%>%
  dplyr::rename(Site=Site.Name)%>%
  glimpse()


dat.length.2<-left_join(dat.length.2,sites.2)



# Checks---
unique(dat.pot.2$day.trap)
length(unique(dat.pot.2$day.trap)) # 169

# # Make Hist data and Summarise the length data to abundance for the all/legal/sublegal----
# Make factors----
dat.factors.2<-dat.pot.2%>%
  dplyr::select(Trap.ID,Pot.Number,Day,Site.Name,Date.Recovered)%>% 
  mutate(Site.Name=str_replace_all(.$Site.Name,c("IR"="Irwin Reef","LH"="Little Horseshoe","JB"="Jim Bailey", "GR"="Golden Ridge", "SR"="South Rig", "WL"="Whites Lump", "CHin1_"="Cliff Head IN1", "CHin2"="Cliff Head IN2", "CHout2_"="Cliff Head OUT2")))%>% 
  distinct()%>%
  glimpse() 

unique(dat.factors.2$Trap.ID) # 97 pots

dat.factors.2$Site.Name

# # # Make histogram data----
dat.hist.2<-dat.length.2%>%
  left_join(dat.factors.2,by=c("Trap.ID"))%>%
  #filter(!is.na(Site))%>% # Didn't work, not sure if we need this line anyway-AM.
  glimpse()

check<-dat.length.2%>% # this will be any that in lengths but not in pot.var
  dplyr::select(Trap.ID,Day)%>%
  anti_join(.,dat.pot.2)


# Make abundance of legal/sublegal----
dat.sublegal.2<-dat.length.2%>%
  left_join(dat.factors.2)%>%
  filter(Carapace.length<76)%>%
  group_by(Trap.ID,Date.Recovered)%>%
  summarize(Abundance=n())%>%
  right_join(dat.factors.2,by = c("Trap.ID", "Date.Recovered"))%>%
  replace_na(list(Abundance = 0))%>%
  mutate(Legal="Sublegal")%>%
  glimpse()

dat.legal.2<-dat.length.2%>%
  left_join(dat.factors.2)%>%
  filter(Carapace.length>76)%>%
  group_by(Trap.ID,Date.Recovered)%>%
  summarize(Abundance=n())%>%
  right_join(dat.factors.2,by = c("Trap.ID", "Date.Recovered"))%>%
  replace_na(list(Abundance = 0))%>%
  mutate(Legal="Legal")%>%
  glimpse()

dat.all.2<- bind_rows(dat.sublegal.2,dat.legal.2)

dat.all.2$Site.Name

# Check--
unique(dat.all.2$Legal)

# Plotting Themes ----
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=12),
    #legend.title = element_blank(),
    #legend.position = c(0.4, 0.8),
    text=element_text(size=10),
    strip.text.y = element_text(size = 10,angle = 270),
    axis.title.x=element_text(vjust=0.3, size=10),
    axis.title.y=element_text(vjust=0.6, angle=90, size=10),
    axis.text.x=element_text(size=10,colour="black"),
    axis.text.y=element_text(size=10,colour="black"),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.ticks.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank(),
    plot.background = element_blank()) 

theme_get()

#Order sites
dat.all.3$Site.Name

dat.all.3$Site.Name<-factor(dat.all.4$Site.Name, levels = c("Irwin Reef", "Whites Lump", "Jim Bailey","South Rig", "Little Horseshoe","Cliff Head OUT2","Cliff Head IN1", "Cliff Head IN2", "Golden Ridge"))

dat.all.3$Site.Name
# length.grouped<-dat.length%>%
#   mutate(length=ifelse(Carapace.length>=27.5&Carapace.length<32.5,30,
#                        ifelse(Carapace.length>=32.5&Carapace.length<37.5,35,
#                               ifelse(Carapace.length>=37.5&Carapace.length<42.5,40,
#                                      ifelse(Carapace.length>=42.5&Carapace.length<47.5,45,
#``                                             ifelse(Carapace.length>=47.5&Carapace.length<52.5,50,
#                                                    ifelse(Carapace.length>=52.5&Carapace.length<57.5,55,
#                                                           ifelse(Carapace.length>=57.5&Carapace.length<62.5,60,
#                                                                  ifelse(Carapace.length>=62.5&Carapace.length<67.5,65,
#                                                                         ifelse(Carapace.length>=67.5&Carapace.length<72.5,70,
#                                                                                ifelse(Carapace.length>=72.5&Carapace.length<77.5,75,
#                                                                                       ifelse(Carapace.length>=77.5&Carapace.length<82.5,80,
#                                                                                              ifelse(Carapace.length>=82.5&Carapace.length<87.5,85,                                                                                                     ifelse(Carapace.length>=77.5&Carapace.length<92.5,90,
#                                                                                                            Carapace.length))))))))))))))%>%
#   group_by(Trap.ID,length)%>%
#   dplyr::summarise(count=n())%>%
#   ungroup() 

summary(dat.all.4$Abundance)
dat.all.4$Site.Name 

#Jitter Plot

bar.location.all.jitter.4<-ggplot(data = dat.all.4, aes(x = Site.Name,y=Abundance)) + 
  stat_summary(fun.y=mean, geom="bar",fill="white",colour="black") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  geom_jitter(width = 0.25, height = 0,alpha=0.25)+
  geom_hline(aes(yintercept=0))+
  ggtitle ("Trip 4 (August)")+
  xlab("")+
  ylab("Lobster per pot (+/- SE)")+
  theme_bw()+Theme1+ 
  theme(axis.text.x = element_text(angle=90))+ 
  theme(axis.line.x=element_line(colour="white", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="white", size=0.5,linetype='solid'))

bar.location.all.jitter.4 



#Abundance plot
bar.location.all.4<-ggplot(data = dat.all.4, aes(x = Site.Name,y=Abundance)) + 
  stat_summary(fun.y=mean, geom="bar",fill="white",colour="black") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  geom_hline(aes(yintercept=0))+
  ggtitle ("Trip 4 (August)")+
  xlab("")+
  ylab("Lobster per pot (+/- SE)")+
  theme_bw()+Theme1+ # Brooke added bw
  theme(axis.text.x = element_text(angle=90))+ #AM
  theme(axis.line.x=element_line(colour="white", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="white", size=0.5,linetype='solid'))

bar.location.all.4 

#Bar graph: Free y, Distinguish by trip
dat.legal.4$Site.Name<-factor(dat.legal.4$Site.Name, levels = c("Irwin Reef","Whites Lump", "Jim Bailey", "South Rig", "Little Horseshoe","Cliff Head OUT2","Cliff Head IN1", "Cliff Head IN2", "Golden Ridge"))
glimpse(dat.legal.4)
dat.legal.4$Site.Name

# Rename sites

bar.location.legal.4<-ggplot(data = dat.all.4, aes(x = Site.Name,y=Abundance)) + 
  stat_summary(fun.y=mean, geom="bar",fill="white",colour="black") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  geom_hline(aes(yintercept=0))+
  ggtitle ("Trip 4 (August)")+
  xlab("")+
  ylab("Lobster per pot (+/- SE)")+
  theme_bw()+Theme1+ # Brooke added bw
  theme(axis.text.x = element_text(angle=90))+ #AM
  theme(axis.line.x=element_line(colour="white", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="white", size=0.5,linetype='solid'))+
  facet_grid(Legal~., scales = "free_y")
bar.location.legal.4

# plot of mean length ----
dat.length.4$Site<-factor(dat.length.4$Site, levels = c("Irwin Reef", "Whites Lump", "Jim Bailey","South Rig", "Little Horseshoe","Cliff Head OUT2","Cliff Head IN1", "Cliff Head IN2", "Golden Ridge"))
dat.length.4$Site

bar.length.location.4<-ggplot(data = dat.length.4, aes(x = Site,y=Carapace.length)) + 
  geom_point(alpha=0.05)+
  stat_summary(fun.y=mean, geom="point",fill="white",colour="red",size=3) +
  stat_summary(fun.ymin = sd.min, fun.ymax = sd.max, geom = "errorbar", width = 0.3,colour="red") +
  geom_hline(aes(yintercept=20))+
  ggtitle ("Trip 4 (August)")+
  xlab("")+ 
  ylab("Carapace length (+/- SD)")+
  theme_bw()+Theme1+
  theme(axis.text.x = element_text(angle=90))+
  theme(axis.line.x=element_line(colour="white", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="white", size=0.5,linetype='solid'))
bar.length.location.4


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

dat.map.legal<-dat.pot.3%>%
  dplyr::select(Trap.ID,Pot.Number,Day,Site.Name,Date.Recovered,Longitude.x,Latitude.y)%>%
  full_join(dat.legal.3)%>%
  filter(!Legal=="All")%>%
  rename(lon=Longitude.x,lat=Latitude.y)%>%
  mutate(lon=as.numeric(lon))%>%
  mutate(lat=as.numeric(lat))#%>%
#full_join(johns)%>%
#mutate(Johns=str_replace_all(.$Johns,c("Yes"="John's pot","No"="Empty pot")))

dat.map.sublegal<-dat.pot.3%>%
  dplyr::select(Trap.ID,Pot.Number,Day,Site.Name,Date.Recovered,Longitude.x,Latitude.y)%>%
  full_join(dat.sublegal.3)%>%
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

unique(dat.map$Trap.ID)
unique(dat.length$Trap.ID)
names(dat.length)
