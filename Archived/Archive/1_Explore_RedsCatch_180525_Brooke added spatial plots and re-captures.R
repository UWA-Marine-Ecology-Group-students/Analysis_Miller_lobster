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
library(rasterVis) # Brooked added for tif files

# Study name----
study<-"Reds.Catch"

# Set work directory----
#work.dir=("~/Google Drive/Projects/Project_WRL low-catch zone/Fieldwork and Reporting/03_Trapping/Analysis_WRL_Reds_2018")
work.dir=("C:/Users/00097191/Google Drive/MEG/Project_WRL low-catch zone/Fieldwork and Reporting/03_Trapping/Analysis_WRL_Reds_2018") # Brooke

# Set sub directories----
data.dir=paste(work.dir,"Data",sep="/")
plots.dir=paste(work.dir,"Plots",sep="/")
map.dir=paste(work.dir,"Map Layers",sep="/")

# Functions----
se <- function(x) sd(x) / sqrt(length(x))
se.min <- function(x) (mean(x)) - se(x)
se.max <- function(x) (mean(x)) + se(x)
sd.min <- function(x) (mean(x)) - sd(x)
sd.max <- function(x) (mean(x)) + sd(x)
scaleFUN <- function(x) sprintf("%.0f", x)

# Import and make data----
setwd(data.dir) 
dir()

# Import length data----
dat.length.gsheet <- gs_title("Lobsters_data_2018_Reds_20180511")%>%
  gs_read_csv(ws = "Lobster.var")%>%
  glimpse()

dat.length<-dat.length.gsheet%>%
  mutate(Count=1)%>%
  filter(!is.na(Carapace.length))%>%
  mutate(Carapace.length=as.numeric(Carapace.length))%>%
  mutate(day.trap=paste(Day,Trap.Number,sep="."))%>%
  glimpse()

write.csv(dat.length,"lob.dat.csv")
dat.length<-read.csv("lob.dat.csv")

unique(dat.length$Trap.ID) # 186 traps

# # Import pot data----
dat.pot <-gs_title("Lobsters_data_2018_Reds_20180511")%>%
   gs_read_csv(ws = "Pot.var")%>%
   mutate(day.trap=paste(Day,Pot.Number,sep="."))%>%
   #mutate(Site.Name=str_replace_all(.$Site.Name,c("SM"="Seven Mile","CHin(1)"="Cliff Head IN 1","CHin(2)"="Cliff Head IN 2","CHout2" = "Cliff Head OUT 1","CHout(2)" = "Cliff Head OUT 2", "WP"="Whitepoint", "RM"="River Mouth")))%>% 
   filter(Johns=="No")%>% # turn off if you add in john's data
   glimpse()

write.csv(dat.pot,"dat.pot.csv")
dat.pot<-read.csv("dat.pot.csv")
unique(dat.pot$Trap.ID) # 240 pots

sites<-dat.pot%>%
  distinct(Trip,Day,Trap.ID,Site.Name)%>%
  mutate(Trap.ID=as.character(Trap.ID))%>%
  dplyr::rename(Site=Site.Name)%>%
  glimpse()

dat.length<-left_join(dat.length,sites)

# Checks---
unique(dat.pot$day.trap)
length(unique(dat.pot$day.trap)) # 224
  

# # Make Hist data and Summarise the length data to abundance for the all/legal/sublegal----
# Make factors----
dat.factors<-dat.pot%>%
  dplyr::select(Trap.ID,Pot.Number,Day,Site.Name,Date.Recovered)%>% 
  distinct()%>%
  glimpse() 

unique(dat.factors$Trap.ID) # 240 pots

# # # Make histogram data----
dat.hist<-dat.length%>%
   left_join(dat.factors,by=c("Trap.ID"))%>%
   #filter(!is.na(Site))%>% # Didn't work, not sure if we need this line anyway-AM.
   glimpse()

check<-dat.length%>% # this will be any that in lengths but not in pot.var
  dplyr::select(Trap.ID,Day)%>%
  anti_join(.,dat.pot)

# Make abundance of all----
dat.all<-dat.length%>%
  left_join(dat.factors)%>%
  group_by(Trap.ID,Date.Recovered)%>%
  dplyr::summarize(Abundance=n())%>%
  right_join(dat.factors, by = c("Trap.ID", "Date.Recovered"))%>%
  replace_na(list(Abundance = 0))%>%
  mutate(Legal="All")%>%
  glimpse()


# Checks--
summary(dat.all$Abundance)
dat.all$Abundance

# Make abundance of legal/sublegal----
dat.sublegal<-dat.length%>%
  left_join(dat.factors)%>%
  filter(Carapace.length<76)%>%
  group_by(Trap.ID,Date.Recovered)%>%
  summarize(Abundance=n())%>%
  right_join(dat.factors,by = c("Trap.ID", "Date.Recovered"))%>%
  replace_na(list(Abundance = 0))%>%
  mutate(Legal="Sublegal")%>%
  glimpse()

dat.legal<-dat.length%>%
  left_join(dat.factors)%>%
  filter(Carapace.length>76)%>%
  group_by(Trap.ID,Date.Recovered)%>%
  summarize(Abundance=n())%>%
  right_join(dat.factors,by = c("Trap.ID", "Date.Recovered"))%>%
  replace_na(list(Abundance = 0))%>%
  mutate(Legal="Legal")%>%
  bind_rows(dat.sublegal,dat.all)%>%
  glimpse()

# Check--
unique(dat.legal$Legal)


# Plots-----
setwd(plots.dir)

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
    plot.background = element_blank()) # Brooke added

theme_get()

### Histograms ----

#Order sites
dat.all$Site.Name<-factor(dat.all$Site.Name, levels = c("SM","RM", "IR","LR","SR","WL","SD","LH","WP","CHout(2)","CHout(1)","DM","CHin(1)", "CHin(2)","GR"))

hist.location<-ggplot(data = dat.length, aes(x = Carapace.length)) + 
  geom_histogram( aes(y = ..density..),binwidth=5,fill="white",colour="black") + 
  #scale_y_continuous(name='Relative density',breaks = seq(-0.08,0.08,0.02),labels=abs(seq(-8,8,2))) +
  # coord_flip()+
  Theme1+
  #ylab("Relative density")+
  xlab("Carapace length (mm)")+
  geom_hline(aes(yintercept=0))+
  facet_grid(Site~.)+
  theme(strip.text.y = element_text(angle=360))+
  theme(axis.line.x=element_line(colour="white", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="white", size=0.5,linetype='solid'))+
  theme(axis.text.y=element_blank(),
       axis.ticks.y=element_blank())   
hist.location

#ggsave(hist.location,file=paste(study,"hist.location", "png",sep = "."), width = 10, height = 15,units = "cm")


hist.freq<-ggplot(data = dat.length, aes(x = Carapace.length)) + 
  geom_histogram( aes(y = ..count..),binwidth=5,fill="white",colour="black") + 
  #scale_y_continuous(name='Relative density', breaks = seq(50,100, 150),labels=abs(seq(50,100, 150))) +
  # coord_flip()+
  Theme1+
  xlab("Carapace length (mm)")+
  ylab("Relative density")+
  geom_hline(aes(yintercept=0))+
  geom_text(aes(x,y, label=lab))+
  facet_grid(Site~.)+
  theme(strip.text.y = element_text(angle=360))+
  theme(axis.line.x=element_line(colour="white", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="white", size=0.5,linetype='solid'))+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
hist.freq


length.grouped<-dat.length%>%
  mutate(length=ifelse(Carapace.length>=27.5&Carapace.length<32.5,30,
                       ifelse(Carapace.length>=32.5&Carapace.length<37.5,35,
                       ifelse(Carapace.length>=37.5&Carapace.length<42.5,40,
                       ifelse(Carapace.length>=42.5&Carapace.length<47.5,45,
                       ifelse(Carapace.length>=47.5&Carapace.length<52.5,50,
                       ifelse(Carapace.length>=52.5&Carapace.length<57.5,55,
                       ifelse(Carapace.length>=57.5&Carapace.length<62.5,60,
                       ifelse(Carapace.length>=62.5&Carapace.length<67.5,65,
                       ifelse(Carapace.length>=67.5&Carapace.length<72.5,70,
                       ifelse(Carapace.length>=72.5&Carapace.length<77.5,75,
                       ifelse(Carapace.length>=77.5&Carapace.length<82.5,80,
                       ifelse(Carapace.length>=82.5&Carapace.length<87.5,85,
                       ifelse(Carapace.length>=77.5&Carapace.length<92.5,90,
                              Carapace.length))))))))))))))%>%
  group_by(Trap.ID,length)%>%
  dplyr::summarise(count=n())%>%
  ungroup()

dat.add<-dat.pot%>%
  dplyr::select(Trap.ID,Site.Name)

#length.freq<-right_join(length.grouped,dat.add)%>%
#  complete(length, nesting(Site.Name, Trap.ID),fill = list(count = 0))%>%
#  filter(!is.na(length))

bar.test<-ggplot(data = length.freq, aes(x = length,y=count)) + 
  stat_summary(fun.y=mean, geom="bar",fill="white",colour="black") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  geom_hline(aes(yintercept=0))+
  xlab("Carapace length (mm)")+
  ylab("Lobster per pot (+/- SE)")+
  #theme_bw()+
  Theme1+ # Brooke added bw
  theme(axis.line.x=element_line(colour="white", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="white", size=0.5,linetype='solid'))+
  facet_grid(Site.Name~.,scales = "free_y")
bar.test

#ggsave(bar.test,file=paste(study,"hist.size", "png",sep = "."), width = 10, height = 15,units = "cm")


  
summary(dat.length)
names(dat.pot)
unique(dat.length$Carapace.length)
  
# hist.location.stage<-ggplot(data = dat.hist, aes(x = Carapace.length, fill = Colour)) + 
#   geom_histogram(data = dplyr::filter(dat.hist, Colour == "White"), aes(y = ..density..),col="grey30",binwidth=5) + 
#   geom_histogram(data = dplyr::filter(dat.hist, Colour == "Red"), aes(y = -..density..),col="grey30",binwidth=5) +
#   scale_y_continuous(name='Relative density',breaks = seq(-0.08,0.08,0.02),labels=abs(seq(-8,8,2))) +
#    coord_flip()+
#   Theme1+
#   xlab("Carapace length (mm)")+
#   scale_fill_manual(values = c("red","white"),name="Lobster colouration",labels=c("Red","White"))+
#   geom_hline(aes(yintercept=0))+
#   facet_grid(Location~.)+
#   theme(legend.position="top")+
#   theme(axis.text.y=element_blank(),
#         axis.ticks.y=element_blank())
# hist.location.stage
 
# ggsave(hist.location.stage,file=paste(study,"hist.location.stage", "png",sep = "."), width = 10, height = 15,units = "cm")



# Bar plots----


# Barplot of number----
summary(dat.all$Abundance)

bar.location.all.jitter<-ggplot(data = dat.all, aes(x = Site.Name,y=Abundance)) + 
  stat_summary(fun.y=mean, geom="bar",fill="white",colour="black") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  geom_jitter(width = 0.25, height = 0,alpha=0.25)+
  geom_hline(aes(yintercept=0))+
  xlab("")+
  ylab("Lobster per pot (+/- SE)")+
  theme_bw()+Theme1+ # Brooke added bw
  theme(axis.text.x = element_text(angle=45))+ # AM added
  theme(axis.line.x=element_line(colour="white", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="white", size=0.5,linetype='solid'))

bar.location.all.jitter
#ggsave(bar.location.all.jitter,file=paste(study,"bar.location.all.jitter", "png",sep = "."), width = 15, height = 15,units = "cm")


bar.location.all<-ggplot(data = dat.all, aes(x = Site.Name,y=Abundance)) + 
  stat_summary(fun.y=mean, geom="bar",fill="white",colour="black") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  geom_hline(aes(yintercept=0))+
  xlab("")+
  ylab("Lobster per pot (+/- SE)")+
  theme_bw()+Theme1+ # Brooke added bw
  theme(axis.text.x = element_text(angle=45))+ #AM
  theme(axis.line.x=element_line(colour="white", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="white", size=0.5,linetype='solid'))
bar.location.all

#ggsave(bar.location.all,file=paste(study,"bar.location.all", "png",sep = "."), width = 15, height = 15,units = "cm")

#dat.legal$Site.Name<-factor(dat.all$Site.Name, levels = c("Cliff Head IN","Cliff Head OUT", "LH","Whitepoint", "River Mouth","Seven Mile"))
dat.legal$Site.Name<-factor(dat.legal$Site.Name, levels = c("SM","RM", "IR","LR","SR","WL","SD","LH","WP","CHout(2)","CHout(1)","DM","CHin(1)", "CHin(2)","GR"))

bar.location.legal<-ggplot(data = dat.legal, aes(x = Site.Name,y=Abundance)) + 
  stat_summary(fun.y=mean, geom="bar",fill="white",colour="black") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  geom_hline(aes(yintercept=0))+
  xlab("")+
  ylab("Lobster per pot (+/- SE)")+
  theme_bw()+Theme1+ # Brooke added bw
  theme(axis.text.x = element_text(angle=45))+ #AM
  theme(axis.line.x=element_line(colour="white", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="white", size=0.5,linetype='solid'))+
  facet_grid(Legal~.)
bar.location.legal
#ggsave(bar.location.legal,file=paste(study,"bar.location.legal", "png",sep = "."), width = 15, height = 15,units = "cm")

# plot of mean length ----
glimpse(dat.length) #AM changed 'dat' to 'dat.length'

bar.length.location<-ggplot(data = dat.length, aes(x = Site,y=Carapace.length)) + 
  geom_point(alpha=0.05)+
  stat_summary(fun.y=mean, geom="point",fill="white",colour="red",size=3) +
  stat_summary(fun.ymin = sd.min, fun.ymax = sd.max, geom = "errorbar", width = 0.3,colour="red") +
  geom_hline(aes(yintercept=20))+
  xlab("")+ 
  ylab("Carapace length (+/- SD)")+
  Theme1+
  theme(axis.line.x=element_line(colour="white", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="white", size=0.5,linetype='solid'))
bar.length.location
ggsave(bar.length.location,file=paste(study,"bar.length.location", "png",sep = "."), width = 15, height = 15,units = "cm")


### Spatial Plots ----
# # Plot legal and sublegal spatially 
# setwd(map.dir)
# dir()
# au<-readOGR("aust_cd66states.shp")
# au<-fortify(au)

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

dat.map<-dat.pot%>%
  dplyr::select(Trap.ID,Pot.Number,Day,Site.Name,Date.Recovered,Longitude.x,Latitude.y)%>%
  full_join(dat.legal)%>%
  filter(!Legal=="All")%>%
  rename(lon=Longitude.x,lat=Latitude.y)%>%
  mutate(lon=as.numeric(lon))%>%
  mutate(lat=as.numeric(lat))%>%
  full_join(johns)%>%
  mutate(Johns=str_replace_all(.$Johns,c("Yes"="John's pot","No"="Empty pot")))

str(dat.map)
summary(dat.map)
## Make map ----
center<- c(114.9,-29.38)

# gg.map <- get_map(location = center, source = "stamen", maptype = "toner-lite", zoom = 11)
# gg.map<-ggmap(gg.map)
# gg.map


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

## plot - facetted by legal size 
legal.map2<-gplot(map, maxpixels =10000000000000) + #, maxpixels = 5e10
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable) +
  scale_fill_gradientn(colours=used_cols, guide=FALSE)+
  coord_equal()+
  Theme_spatial+
  geom_point(data=filter(dat.map,Abundance<1&Johns=="No"),aes(lon,lat),shape=4,colour="red",size=2)+
  geom_point(data=filter(dat.map,Abundance<1&Johns=="Yes"),aes(lon,lat),shape=2,colour="red",size=2)+
  geom_point(data=filter(dat.map,Abundance>0),aes(lon,lat,size=Abundance),shape=21,colour="black",fill="dodgerblue",alpha=0.3)+
  scale_size_continuous(range = c(1,14), breaks=seq(0,40, by=10), name = "Abundance per pot")+
  Theme_spatial+
  xlab('Longitude')+
  ylab('Latitude')+
  coord_cartesian(xlim = c(114.7,115.17),ylim=c(-29.6,-29)) +
  facet_wrap(~Legal,nrow=1)
legal.map2

str(dat.map)

legal.map.legal<-gplot(map, maxpixels = 1000000000000000) + #, maxpixels = 5e101000000000000000
  geom_tile(aes(fill = value)) +
  #facet_wrap(~ variable) +
  #theme(strip.background = element_blank(),strip.text.x = element_blank())+
  scale_fill_gradientn(colours=used_cols, guide=FALSE)+
  coord_equal()+
  Theme_spatial+
  geom_point(data=filter(dat.map,Abundance>0&Legal=="Legal"),aes(lon,lat,size=Abundance),shape=21,colour="black",fill="dodgerblue",alpha=0.3)+
  #geom_point(data=filter(dat.map,Abundance<1&Johns=="No"&Legal=="Legal"),aes(lon,lat),shape=4,colour="red",size=2)+
  #geom_point(data=filter(dat.map,Abundance<1&Johns=="Yes"&Legal=="Legal"),aes(lon,lat),shape=2,colour="red",size=2)+
  geom_point(data=filter(dat.map,Abundance<1&Legal=="Legal"),aes(lon,lat,shape=Johns),colour="black",fill="red",size=2,alpha=0.3)+
  scale_size_continuous(range = c(3,7), breaks=seq(0,5, by=1), name = "Abundance per pot")+
  scale_shape_manual(values=c(21,24), name = "")+
  #theme(legend.position="right")+
  Theme_spatial+
  xlab('Longitude')+
  ylab('Latitude')+
  coord_cartesian(xlim = c(114.7,115.17),ylim=c(-29.6,-29))
legal.map.legal
  
legal.map.sublegal<-gplot(map, maxpixels =1000000000000000) + #, maxpixels = 5e10
  geom_tile(aes(fill = value)) +
  #facet_wrap(~ variable) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())+
  scale_fill_gradientn(colours=used_cols, guide=FALSE)+
  coord_equal()+
  Theme_spatial+
  geom_point(data=filter(dat.map,Abundance>0&Legal=="Legal"),aes(lon,lat,size=Abundance),shape=21,colour="black",fill="dodgerblue",alpha=0.3)+
  #geom_point(data=filter(dat.map,Abundance<1&Johns=="No"&Legal=="Legal"),aes(lon,lat),shape=4,colour="red",size=2)+
  #geom_point(data=filter(dat.map,Abundance<1&Johns=="Yes"&Legal=="Legal"),aes(lon,lat),shape=2,colour="red",size=2)+
  geom_point(data=filter(dat.map,Abundance<1&Legal=="Legal"),aes(lon,lat,shape=Johns),colour="black",fill="red",size=2,alpha=0.3)+
  scale_size_continuous(range = c(3,7), breaks=seq(0,5, by=1), name = "Abundance per pot")+
  scale_shape_manual(values=c(21,24), name = "")+
  Theme_spatial+
  xlab('Longitude')+
  ylab('Latitude')+
  coord_cartesian(xlim = c(114.7,115.17),ylim=c(-29.6,-29))
legal.map.sublegal

cropped.legal<-legal.map.legal+
  coord_cartesian(xlim = c(114.8,115),ylim=c(-29.6,-29.2))

cropped.sublegal<-legal.map.sublegal+
  coord_cartesian(xlim = c(114.8,115),ylim=c(-29.6,-29.2))

ggsave(cropped.legal,file=paste(study,"legal.map.legal.cropped", "png",sep = "."), width = 15, height = 17,units = "cm")
ggsave(cropped.sublegal,file=paste(study,"legal.map.sublegal.cropped", "png",sep = "."), width = 15, height = 17,units = "cm")


setwd(plots.dir)
gc()
memory.limit()
memory.limit(size=56000)
ggsave(legal.map.legal,file=paste(study,"legal.map.legal", "png",sep = "."), width = 15, height = 17,units = "cm")
ggsave(legal.map.sublegal,file=paste(study,"legal.map.sublegal", "png",sep = "."), width = 15, height = 17,units = "cm")

ggsave(legal.map.legal,file=paste(study,"legal.map.legal.small", "png",sep = "."), width = 10, height = 12,units = "cm")
ggsave(legal.map.sublegal,file=paste(study,"legal.map.sublegal.small", "png",sep = "."), width = 10, height = 12,units = "cm")



ggsave(legal.map2,file=paste(study,"legal.spatial.plot.with.Johns.2", "png",sep = "."), width = 40, height = 20,units = "cm")


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
ggsave(legal.map,file=paste(study,"legal.spatial.plot.with.Johns", "png",sep = "."), width = 25, height = 15,units = "cm")

unique(dat.map$Trap.ID)
unique(dat.length$Trap.ID)
names(dat.length)

### new recaptured ----

names(dat.length)



total.recap<-dat.length%>%
  # left_join(.,sites)%>%
  # distinct(Site,Tag.number,Trip)%>%
  filter(Trip==2)%>%
  # group_by(Site,Tag.number)%>%
  # dplyr::summarise(min=min(Trip),max=max(Trip))%>%
  # filter(max>min)%>%
  # filter(!is.na(Tag.number))%>%
  # ungroup()%>%
  # group_by(Site)%>%
  # dplyr::summarise(Total.recaptured.June=n())%>%
  glimpse()
#1594

total.recap.june<-dat.length%>%
  # left_join(.,sites)%>%
  # distinct(Site,Tag.number,Trip)%>%
  filter(Trip==2)%>%
  distinct(Tag.number)%>%
  # group_by(Site,Tag.number)%>%
  # dplyr::summarise(min=min(Trip),max=max(Trip))%>%
  # filter(max>min)%>%
  # filter(!is.na(Tag.number))%>%
  # ungroup()%>%
  # group_by(Site)%>%
  # dplyr::summarise(Total.recaptured.June=n())%>%
  glimpse()




total.tagged<-dat.length%>%
  filter(!is.na(Tag.number))%>%
  group_by(Trip,Site)%>%
  dplyr::summarise(Total=n())

avg.position<-dat.pot%>%
  group_by(Site.Name)%>%
  summarise(lat=mean(Latitude.y),lon=mean(Longitude.x))%>%
  rename(Site=Site.Name)

table<-spread(total.tagged,Trip,Total)%>%
  rename(May="1",June="2")%>%
  left_join(total.recap)%>%
  replace_na(list(May = 0,June=0,Total.recaptured.June=0))%>%
  left_join(avg.position)%>%
  arrange(desc(lat))%>%
  mutate(Site = factor(Site, unique(Site)))%>% # orders names
  dplyr::select(-c(lat,lon))%>%
  mutate(Site=str_replace_all(Site,"[^[:alnum:]]", "."))%>% 
  dplyr::mutate(Site=str_replace_all(.$Site,c("SM"="Seven Mile","CHin.1."="Cliff Head In 1","CHin.2."="Cliff Head In 2","CHout.1." = "Cliff Head Out 1","CHout.2." = "Cliff Head Out 2", "WP"="Whitepoint", "RM"="River Mouth","IR"="Irwin reef","LR"="Long reef","WL"="White lumps","SR"="South Rig","LH"="Little Horseshoe","SD"="South Dummy","DM"="Davids marks","GR"="Golden ridge")))



dat<-dat.length%>%
  dplyr::select(Date,Trap.ID,Recapture,Count)%>%
  #full_join(dat.pot)%>%
  #dplyr::select(Date,Trap.ID,Recapture,Count,Site.Name)%>%
  replace_na(list(Count = 0,Recapture="FALSE"))

dat.recap<-dat%>%
  filter(Recapture=="TRUE")%>%
  group_by(Date,Trap.ID)%>%
  dplyr::summarise(total.recap=sum(Count))%>%
  ungroup()%>%
  rename(Date.Recovered=Date)%>%
  full_join(dat.pot)%>%
  dplyr::select(Date.Recovered,Trap.ID,total.recap)%>%
  replace_na(list(total.recap = 0))
names(dat.pot)
  
dat.total<-dat%>%
  group_by(Site.Name,Trap.ID)%>%
  dplyr::summarise(total=sum(Count))

####  Ash's attempt -----
dat.recap<-full_join(dat.recap,dat.total)
dat.recap$prop <- dat.recap$total.recap/dat.recap$total
dat.recap$prop[is.nan(dat.recap$prop)] <-0
dat.prop<-aggregate(dat.recap$prop, by=list(Site.Name=dat.recap$Site.Name), FUN=sum)
names(dat.prop)[2]<- 'total.prop'
names(dat.prop) #seven Mile says 1.16 proportion recaptured.

#piping wasn't working for me^                          

dat.recap<-dat.recap%>%
  mutate(Site.Name=fct_relevel(Site.Name,"Seven Mile","River Mouth","Whitepoint","LH","Cliff Head OUT","Cliff Head IN"))

##Plot
setwd(plots.dir)
dat.prop$Site.Name<-factor(dat.prop$Site.Name, levels = c("Cliff Head IN","Cliff Head OUT", "LH","Whitepoint", "River Mouth","Seven Mile"))

bar.total.prop<-ggplot(data = dat.recap, aes(x = Site.Name,y=prop)) + 
  stat_summary(fun.y=mean, geom="bar",fill="white",colour="black") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  geom_hline(aes(yintercept=0))+
  xlab("")+
  ylab("Proportion recaptured")+
  theme_bw()+Theme1+ 
  theme(axis.line.x=element_line(colour="white", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="white", size=0.5,linetype='solid'))
bar.total.prop
ggsave(bar.total.prop,file=paste(study,"bar.total.prop-draft-BG", "png",sep = "."), width = 18, height = 15,units = "cm")

#Doesn't look that great//incorrect-Not sure how to make it better?- AM