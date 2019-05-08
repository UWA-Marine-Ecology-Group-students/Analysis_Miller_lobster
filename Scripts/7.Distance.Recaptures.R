#Spatial Distance
# Explore catch data----
rm(list=ls()) # Clears memory

# librarys----
library(geosphere)
library(dplyr)
library(ggplot2)

#Import data----
dat.rr <- read_csv("dat.rr.all.csv")%>%
  glimpse()

dat.rr <-dat.rr%>%
  select(Date, recap.Date,Tag.number, Location.int, mini.site.x, Lon1, Lat1, Lon2, lat2)%>%
  mutate(mini.site.x=str_replace_all(.$mini.site.x,c("Jim Bailey"="White Point", "Whitepoint"="White Point", "CHS"="Cliff Head South")))%>%
  glimpse()


#Removes Ben's Seven Mile as I don't have the lat and long for his data yet
dat <- dat.rr%>%
  filter(!is.na(Lon1))%>%
  filter(Tag.number!="K3211")%>% #An obvious outlier (land)
  filter(is.na(Location.int)| Location.int!="Rivermouth")%>%
  rename(Lat2=lat2)%>%
  glimpse()

dd <- dat%>%
  mutate(Lyrs=as.numeric(as.Date(recap.Date, '%d/%m/%Y')-as.Date(Date, '%d/%m/%Y'))/365)%>%
  filter(Lyrs!= 0)%>%
  mutate(rcmonth=format(as.Date(recap.Date, '%d/%m/%Y'),'%m'),rlmonth=format(as.Date(Date, '%d/%m/%Y'),'%m'))%>%
  glimpse()

dd <-dd%>%
  filter(rcmonth!="12")%>%
  filter(rcmonth!="01")%>%
  filter(rcmonth!="02")%>%
  #filter(rlmonth!="11")%>%
  filter(rlmonth!="12")%>%
  filter(Location.int!= "Seven Mile"|rlmonth!="11")%>%
  #filter(Location.int!= "Cliff Head" & rlmonth!="12")%>%
  glimpse()

unique(dd$rcmonth)
unique(dd$rlmonth)

glimpse(dat)

d1<-dd%>%
  select( Lon1, Lat1)%>% #Tag.number, Location.int,
  glimpse()

d2<-dd%>%
  select(Lon2, Lat2)%>% #Tag.number, Location.int,
  glimpse()


# create distance matrix----
mat <- distGeo(d1,d2)
dd$distance <- mat/1000
glimpse(dd)

#Plot All
dd$Location.int<-factor(dd$Location.int, levels = c("Seven Mile", "Irwin Reef", "Cliff Head", "Golden Ridge"))
plot<- ggplot(data=dd, aes(x =Location.int, y= distance), notch=FALSE,position = dodge1, outlier.shape = NA)+
  geom_boxplot(outlier.color = NA, notch=FALSE)+
  geom_jitter(width = 0.1, height = NULL, alpha=0.4)+
  guides(fill=FALSE)+
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)+
  theme(axis.line.x=element_line(colour="black", size=0.5,linetype='solid'))+
  theme(axis.line.y=element_line(colour="black", size=0.5,linetype='solid'))+
  theme(panel.background=element_blank())+
  ylim(0,10)+
  xlab("Location")+
  ylab("Distance (km)")
plot

help('dplyr')

library(tidyverse)
glimpse(dat)
dd %>%
  group_by(Location.int) %>%
  summarise(average = mean(distance))

dd %>% 
  group_by(mini.site.x) %>% 
  summarise(average = mean(distance))

dd%>%
  group_by(mini.site.x)%>%
  summarise(Total=n())


# Plotting Themes ----
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.background = element_blank(),
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
    strip.text.x = element_text(size = 10),
    plot.background = element_blank()) # Brooke added

#Plot----
plot<- ggplot(data=dd, aes(x =Lyrs, y= distance), notch=FALSE,position = dodge1, outlier.shape = NA)+
  geom_jitter(width = 0.1, height = NULL, alpha=0.4)+
  guides(fill=FALSE)+
  Theme1+
  theme(plot.title = element_text(hjust = 0, size=12, face = "bold"))+
  theme(axis.line.x=element_line(colour="black", size=0.5,linetype='solid'))+
  theme(axis.line.y=element_line(colour="black", size=0.5,linetype='solid'))+
  theme(panel.background=element_blank())+
  xlab("Time at liberty")+
  ylab("Distance (km)")+
  facet_wrap(~Location.int)
plot

# distGeo(c(0,0),c(90,90))
# distGeo(c(114.8707,-29.17767),c(114.7881,-29.18163), fun=distVincentyEllipsoid)
# 
# dist<- distGeo(d1,d2)%>%
#   glimpse()
# 
# dis <- distm(d1, d2)
# distm(dat$Lon1, dat$Lat1, dat$Lon2, dat$lat2, fun = distHaversine)
# 
# distm(c(114.8782, -29.17298), c(114.8773, -29.17255), fun = distHaversine)
# distGeo(c(114.8782, -29.17298),c(114.8773, -29.17255))
# 
# # create distance matrix
# mat <- distm(list1[,c('longitude','latitude')], list2[,c('longitude','latitude')], fun=distVincentyEllipsoid)
# 
# # assign the name to the point in list1 based on shortest distance in the matrix
# list1$locality <- list2$locality[max.col(-mat)]