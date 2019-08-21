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
#work.dir=("Z://Analysis_Miller_lobster") #for laptop
work.dir=("C:/GitHub/Analysis_Miller_lobster") # For Brooke

# Set sub-directories ----
data.dir=paste(work.dir,"Data",sep="/")
plot.dir=paste(work.dir,"Plots",sep="/")

#Import movement data (created in '07.1.Movement.R' script)
dat.rr<- read.csv("Movement.Data.csv")%>%
  glimpse()

#simplify data
dat.mm<- dat.rr%>%
  select(-c(Carapace.length, Carapace.length.recap, Sex, Sex.recap, Colour, Colour.recap, Total.damage, Total.damage.recap, Longitude, Longitude.recap, Latitude, Latitude.recap))%>%
  filter(distance>0)%>% #220 that have distance as zero-haven't moved
  glimpse()

#Plotting----
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


#Seperate into sublegals and Legals
#Legals----
dat.legal<-dat.mm%>%
  filter(sizeclass=='Legal')%>%
  glimpse()
#442 legals

mean(dat.legal$bearing) #194.6148


polar.legal<-ggplot(data = dat.legal, aes(x = bearing)) + 
  geom_histogram( aes(y = ..density..),binwidth=5,fill="maroon4",colour="black") + 
  xlab("Bearing")+
  ylab("")+
  geom_hline(aes(yintercept=0))+
  theme_bw()+
  ggtitle("Legal") +
  theme(strip.text.y = element_text(angle=360))+
  theme(axis.line.x=element_line(colour="white", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="white", size=0.5,linetype='solid'))+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_segment(aes(x=194.6,xend=194.6, y=0, yend=0.006), colour="darkblue", size=1, arrow = arrow(length = unit(0.5, "cm")))+
  coord_polar(theta = "x", start=0, direction = 1)#+
  #facet_wrap(~Location)
polar.legal


#Sublegals----
dat.sublegal<-dat.mm%>%
  filter(sizeclass=='Sublegal')%>%
  glimpse()

mean(dat.sublegal$bearing)

#1,115 sublegals
polar.sublegal<-ggplot(data = dat.sublegal, aes(x = bearing)) + 
  geom_histogram( aes(y = ..density..),binwidth=5,fill="maroon4",colour="black") + 
  xlab("Bearing")+
  ylab("")+
  geom_hline(aes(yintercept=0))+
  theme_bw()+
  ggtitle("Sublegal") +
  theme(strip.text.y = element_text(angle=360))+
  theme(axis.line.x=element_line(colour="white", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="white", size=0.5,linetype='solid'))+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  coord_polar(theta = "x", start=0, direction = 1)+
  facet_wrap(~Location)
polar.sublegal


#Facet polar plot
polar.facet<-ggplot(data = dat.mm, aes(x = bearing)) + 
  geom_histogram( aes(y = ..density..),binwidth=5,fill="maroon4",colour="black") + 
  xlab("Bearing")+
  ylab("")+
  geom_hline(aes(yintercept=0))+
  theme_bw()+
  ggtitle("Density orientation plot") +
  theme(strip.text.y = element_text(angle=360))+
  theme(axis.line.x=element_line(colour="white", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="white", size=0.5,linetype='solid'))+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  coord_polar(theta = "x", start=0, direction = 1)+
  facet_wrap(sizeclass~.)
polar.facet



#Orientation and distance----
#focus on residential movement (<0.5km)
dat.resid<-dat.mm%>%
  filter(distance<0.5)%>%
  filter(!sizeclass=='Legal')%>%
  glimpse()

polar.distance<-ggplot(data = dat.resid, aes(x = bearing, y=distance)) + 
  #geom_smooth()+
  stat_summary(fun.y=mean, geom="bar",fill="maroon4",colour="black", width=2) +
  #stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  xlab("Bearing")+
  ylab("")+
  geom_hline(aes(yintercept=0))+
  theme_bw()+
  coord_polar(theta = "x", start=0, direction = 1) +
facet_wrap(~Location)
polar.distance
  
polar.distance<-ggplot(data = dat.sublegal, aes(x = bearing, y=distance)) + 
  #geom_bar( width=5)+
  stat_summary(fun.y=mean, geom="bar",fill="maroon4",colour="black", width=5) +
  #stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  xlab("Bearing")+
  ylab("")+
  geom_hline(aes(yintercept=0))+
  theme_bw()+
  coord_polar(theta = "x", start=0, direction = 1)
polar.distance


  

  


