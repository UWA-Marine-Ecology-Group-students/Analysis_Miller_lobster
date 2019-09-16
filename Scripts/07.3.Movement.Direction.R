# Explore catch data ----
rm(list=ls()) #clear memory

# librarys ----
library(tidyr)
library(dplyr)
library(googlesheets)
library(stringr)
library(ggplot2)
library(readr)
# Study name ----
study<-"Recapture.Movement"

# Set work directory ----
work.dir=("Z://Analysis_Miller_lobster/Data") #for laptop
#work.dir=("C:/GitHub/Analysis_Miller_lobster") # For Brooke

# Set sub-directories ----
data.dir=paste(work.dir,"Data",sep="/")
plot.dir=paste(work.dir,"Plots",sep="/")

#Import movement data (created in '07.1.Movement.R' script)
dat.rr<- read.csv("Movement.Data.csv")%>%
  glimpse()

#Rename Locations
dat.rr%<>%
  mutate(Location=str_replace_all(.$Location, c("Little Horseshoe"="Golden Ridge", "White Point"="Irwin Reef")))%>% #Changed for fisher presentation
  mutate(Location=as.factor(Location))%>%
  filter(!Location=="Seven Mile")%>% #Remove for fisher presentation
  glimpse()

unique(dat.rr$Location)


#simplify data
dat.mm<- dat.rr%>%
  select(-c(Carapace.length, Carapace.length.recap, Sex, Sex.recap, Colour, Colour.recap, Total.damage, Total.damage.recap, Longitude, Longitude.recap, Latitude, Latitude.recap))%>%
  filter(distance>0)%>% #220 that have distance as zero-haven't moved
  glimpse()

#Plotting----
Theme1 <-
  theme( # use theme_get() to see available options
    #panel.grid.major = element_blank(), 
    #panel.grid.minor = element_blank(), 
    legend.background = element_blank(),
    legend.key = element_blank(),
    # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=12),
    legend.position = "none",
    text=element_text(size=12),
    axis.title.x=element_text(vjust=0.3, size=10),
    axis.title.y=element_text(vjust=0.6, angle=90, size=10),
    axis.text.x=element_text(size=10,colour="black"),
    axis.text.y=element_text(size=10,colour="black"),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.ticks.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())
    #plot.background = element_blank()) # Brooke added

#Order Location
dat.mm$Location<-factor(dat.mm$Location,levels=c("Irwin Reef","Cliff Head", "Golden Ridge")) #"Seven Mile", "Mid", "Boundary"

#Seperate into sublegals and Legals
#Legals----
dat.legal<-dat.mm%>%
  filter(sizeclass=='Legal')%>%
  glimpse()
#442 legals


#Legal-Density----
legal.density<-ggplot(data = dat.legal, aes(x = bearing)) + 
  geom_histogram( aes(y = ..density.., fill=Location),binwidth=15,colour="black") + #fill="maroon4"
  xlab("")+
  ylab("")+
  geom_hline(aes(yintercept=0))+
  theme_bw()+
  ggtitle("Legal-Density") +
  Theme1+
  theme(axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="black", size=0.5,linetype='solid'),
        strip.text = element_text(size=12))+
  coord_polar(theta = "x", start=0, direction = 1)+
  ylim(0,0.008)+
  facet_wrap(~Location)
legal.density


#Legal-Distance----
dat.resid<-dat.mm%>%
  filter(distance<0.5)%>%
  filter(sizeclass=='Legal')%>%
  glimpse()

legal.residential<-ggplot(data = dat.resid, aes(x = bearing, y=distance, fill=Location)) + 
  stat_summary(fun.y=mean, geom="bar",colour="black", width=15) + #,fill="maroon4"
  xlab("")+
  ylab("")+
  geom_hline(aes(yintercept=0))+
  ggtitle("Legal-Distance residential (<0.5km)") +
  theme_bw()+
  Theme1+
  theme(axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="black", size=0.5,linetype='solid'),
  strip.text = element_text(size=12))+
  coord_polar(theta = "x", start=0, direction = 1) +
  facet_wrap(~Location)
legal.residential

#Legal migratory-----
dat.migratory<-dat.mm%>%
  filter(distance>0.5)%>%
  filter(sizeclass=='Legal')%>%
  glimpse()

legal.migratory<-ggplot(data = dat.migratory, aes(x = bearing, y=distance, fill=Location)) + 
  stat_summary(fun.y=mean, geom="bar",colour="black", width=15) +
  xlab("")+
  ylab("")+
  geom_hline(aes(yintercept=0))+
  ggtitle("Legal-Distance migratory (>0.5km)") +
  theme_bw()+
  Theme1+
  theme(axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="black", size=0.5,linetype='solid'),
        strip.text = element_text(size=12))+
  coord_polar(theta = "x", start=0, direction = 1) +
  facet_wrap(~Location)
legal.migratory

#Sublegals----
dat.sublegal<-dat.mm%>%
  filter(sizeclass=='Sublegal')%>%
  glimpse()

#Density-sublegals
#1,022 sublegals
sublegal.density<-ggplot(data = dat.sublegal, aes(x = bearing)) + 
  geom_histogram( aes(y = ..density.., fill=Location),binwidth=15,colour="black") + 
  xlab("")+
  ylab("")+
  geom_hline(aes(yintercept=0))+
  theme_bw()+
  ggtitle("Sublegal-Density")+
  Theme1+
  theme(axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="black", size=0.5,linetype='solid'),
        strip.text = element_text(size=12))+
  coord_polar(theta = "x", start=0, direction = 1)+
  ylim(0,0.008)+
  facet_wrap(~Location)
sublegal.density

#Residential Distance-
sub.resid<-dat.mm%>%
  filter(distance<0.5)%>%
  filter(sizeclass=='Sublegal')%>%
  glimpse()

sublegal.residential<-ggplot(data = sub.resid, aes(x = bearing, y=distance, fill=Location)) + 
  stat_summary(fun.y=mean, geom="bar",colour="black", width=15) +
  xlab("")+
  ylab("")+
  geom_hline(aes(yintercept=0))+
  ggtitle("Sublegal-Distance residential (<0.5km)") +
  theme_bw()+
  Theme1+
  theme(axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="black", size=0.5,linetype='solid'),
        strip.text = element_text(size=12))+
  coord_polar(theta = "x", start=0, direction = 1) +
facet_wrap(~Location)
sublegal.residential
  
sub.migrat<-dat.mm%>%
  filter(distance>0.5)%>%
  filter(sizeclass=='Sublegal')%>%
  glimpse()

sublegal.migratory<-ggplot(data = sub.migrat, aes(x = bearing, y=distance, fill=Location)) + 
  stat_summary(fun.y=mean, geom="bar",colour="black", width=15) +
  xlab("")+
  ylab("")+
  ggtitle("Sublegal-Distance migratory (>0.5km)")+
  geom_hline(aes(yintercept=0))+
  theme_bw()+
  Theme1+
  theme(axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="black", size=0.5,linetype='solid'),
        strip.text = element_text(size=12))+
  coord_polar(theta = "x", start=0, direction = 1) +
  facet_wrap(~Location)
sublegal.migratory


  
#Trigonometry section----
glimpse(dat.rr)
#Angle is degrees not radians ugh-need to convert
deg2rad<-function(deg){(deg*pi)/(180)}

#seperate into quarters
#first quarter----
first.q <- dat.rr%>%
  filter(bearing<90)%>%
  dplyr::mutate(radian=deg2rad(bearing))%>%
  glimpse()

first.q%<>% #Convert  bearing to radian first
  dplyr::mutate(WE= (first.q$distance)*(sin(first.q$radian)))%>%
  dplyr::mutate(NS= (first.q$distance)*(cos(first.q$radian)))%>%
  glimpse()

#second quarter----
second.q <- dat.rr%>%
  filter(bearing>90 & bearing<180)%>%
  dplyr::mutate(angle=bearing-90)%>%
  dplyr::mutate(radian=deg2rad(angle))%>%
  glimpse()

second.q%<>%
  dplyr::mutate(WE=(second.q$distance)*(cos(second.q$radian)))%>%
  dplyr::mutate(NS=(second.q$distance)*(sin(second.q$radian)))%>%
  select(-c(angle))%>%
  glimpse()

#Third quarter----
third.q<-dat.rr%>%
  filter(bearing>180 & bearing<270)%>%
  dplyr::mutate(angle=bearing-180)%>%
  dplyr::mutate(radian=deg2rad(angle))%>%
  glimpse()

third.q%<>%
  dplyr::mutate(WE=(third.q$distance)*(sin(third.q$radian)))%>%
  dplyr::mutate(NS=(third.q$distance)*(cos(third.q$radian)))%>%
  select(-c(angle))%>%
  glimpse()

#Fourth quarter----
fourth.q<-dat.rr%>%
  filter(bearing>270)%>%
  dplyr::mutate(angle=bearing-270)%>%
  dplyr::mutate(radian=deg2rad(angle))%>%
  glimpse()

fourth.q%<>%
  dplyr::mutate(WE=(fourth.q$distance)*(cos(fourth.q$radian)))%>%
  dplyr::mutate(NS=(fourth.q$distance)*(sin(fourth.q$radian)))%>%
  select(-c(angle))%>%
  glimpse()

dat.all<- rbind(first.q, second.q, third.q, fourth.q)%>%
  glimpse() #1,412

glimpse(dat.rr) #1,6151 = approx 230 that didn't move. That's ok.


#Model----

setwd(data.dir)
write.csv(dat.all, "North.West.data.csv", row.names = F)  
