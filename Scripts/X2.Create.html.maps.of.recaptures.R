# Create html leaflet maps to show fishers
rm(list=ls()) # Clears memory

# librarys----
library(stringr)
library(tidyverse)
library(leaflet)

# Set work directory ----
work.dir=("C:/GitHub/Analysis_Miller_lobster") # For Brooke

# Sub directories ----
data.dir<-paste(work.dir,"Data",sep="/")

# Read in data ----
setwd(data.dir)
dir()

length<-read.csv("length.csv")%>%
  dplyr::select(Source,Sample,Tag.number,Recapture,Sex,Colour,Outlier,Carapace.length)%>%
  #dplyr::filter(!Outlier%in%c("y"))%>%
  dplyr::filter(!Tag.number%in%c("K3211"))%>%
  #dplyr::filter(Tag.number%in%c("191410","191033","K0287","K0966","K0712","K0477"))%>% # Filter to see good test
  glimpse()

metadata<-read.csv("metadata.csv")%>%
  dplyr::filter(!Successful.count%in%c("No"))%>%
  dplyr::distinct(Source,Sample,Location,Latitude,Longitude,Date)%>%
  dplyr::filter(!Source%in%c("ben-seven-mile"))%>%
  glimpse()

# Join data together ----
dat<-full_join(length,metadata)%>%
  dplyr::filter(!is.na(Latitude))

# Create a releases and a recaptures dataframe ----
dat.recaps<-dat%>%
  dplyr::filter(Recapture%in%c("TRUE"))%>%
  dplyr::select(-c(Source,Sample,Recapture,Colour,Outlier,Location))

dat.releases<-dat%>%
  dplyr::filter(!Recapture%in%c("TRUE"))

dat.releases.caught.again<-semi_join(dat.releases,dat.recaps,by="Tag.number")%>%
  dplyr::select(-c(Source,Sample,Sex,Colour,Outlier,Location,Recapture,Carapace.length))%>%
  dplyr::filter(!is.na(Tag.number))

# Create data for map ----
# Good tag to check 191410 (to see if everything has worked)
dat.map.recaptures<-dat.releases.caught.again%>%
  dplyr::rename(Release.latitude=Latitude,Release.longitude=Longitude,Release.date=Date)%>% #
  left_join(dat.recaps)%>%
  bind_rows(dat.releases.caught.again)%>%
  arrange(Tag.number,Date)%>%
  dplyr::group_by(Tag.number)%>%
  dplyr::mutate(Release.latitude=lag(Latitude))%>%
  dplyr::mutate(Release.longitude=lag(Longitude))%>%
  dplyr::mutate(Release.date=lag(Date))%>%
  dplyr::filter(!is.na(Sex))%>%
  dplyr::rename(Recapture.latitude=Latitude,Recapture.longitude=Longitude,Recapture.date=Date)%>%
  dplyr::mutate(Days.between.captures = (as.Date(as.character(Recapture.date), format="%Y-%m-%d")) - (as.Date(as.character(Release.date))))%>%
  dplyr::mutate(Labels=paste("Tag ",Tag.number,". At liberty for ",Days.between.captures," days before being caught again on ",Recapture.date," (",Carapace.length," mm).",sep=""))%>%
  dplyr::ungroup()%>%
  glimpse()
  
# Need dots for initial releases
dat.map.releases<-semi_join(dat.releases,dat.recaps,by="Tag.number")%>%
  dplyr::select(-c(Source,Sample,Colour,Outlier,Location,Recapture))%>%
  dplyr::rename(Release.latitude=Latitude,Release.longitude=Longitude,Release.date=Date)%>%
  dplyr::mutate(Labels=paste("Tag ",Tag.number,". A ",Sex," lobster, first caught on  ",Release.date," (",Carapace.length," mm).",sep=""))%>%
  dplyr::filter(!is.na(Tag.number))%>%
  glimpse()

# Have pots with multiple dots on the same exact position ----
# So need to combine labels together, but also have releases and recaptures on the same point

# 1. make new data frame for both recaptures and releases with same lat and lon column
# 2. figure out double ups
# 3. paste together labels
# 4. Would be nice to have number caught at that point

glimpse(dat.map.releases)
glimpse(dat.map.recaptures)

dat.dots.recap<-dat.map.recaptures%>%
  dplyr::rename(Latitude=Recapture.latitude,Longitude=Recapture.longitude)%>%
  dplyr::select(Tag.number,Sex,Latitude,Longitude,Labels)

dat.dots.release<-dat.map.releases%>%
  dplyr::rename(Latitude=Release.latitude,Longitude=Release.longitude)%>%
  dplyr::select(Tag.number,Sex,Latitude,Longitude,Labels)

dat.dots<-bind_rows(dat.dots.recap,dat.dots.release)%>%
  dplyr::group_by(Latitude,Longitude)%>%
  dplyr::summarise(Labels=paste(strwrap(Labels), collapse = " <br> <br> "))%>%
  ungroup()

dat.dots.map<-bind_rows(dat.dots.recap,dat.dots.release)%>%
  select(-c(Labels))%>%
  left_join(.,dat.dots)

pal <- colorFactor(c( "hotpink","blue"), domain = c("Male", "Female"))


map.all.leaflet <- leaflet(dat.dots.map) %>% 
  fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude))%>%
  addTiles(group="Base map")%>% 
  addCircleMarkers(data=dat.dots.map,~Longitude,~Latitude,radius = 4,color = ~pal(Sex),stroke = FALSE, fillOpacity = 0.5,popup = ~as.character(Labels),group="All data")%>%
  addLegend("bottomright", pal = pal, values = ~Sex,title = "Sex",opacity = 1,group="All data")%>%
  
  addLayersControl(
    baseGroups = c("Base map"),
    overlayGroups = c("All data"),
    options = layersControlOptions(collapsed = FALSE)
  )

map.all.leaflet  

# Combine recaptures and releases together to make a dataframe that has all lines ----
dat.map.all<-bind_rows(dat.map.releases,dat.map.recaptures)%>%
  arrange(Tag.number,Release.date)%>%
  dplyr::mutate(Release.latitude=ifelse(((!Release.latitude==Recapture.latitude)&(!is.na(Recapture.latitude))),Recapture.latitude,Release.latitude))%>% # So if release and recapture are both not na and are different then release = recapture this is two get a line when only recaptured once
  dplyr::mutate(Release.longitude=ifelse(((!Release.longitude==Recapture.longitude)&(!is.na(Recapture.longitude))),Recapture.longitude,Release.longitude))%>%
  glimpse()

# Loop through tags to add to map ----
uniq <- unique(unlist(dat.map.all$Tag.number))
uniq

for (i in 1:length(uniq)){
  
  temp.dat <- subset(dat.map.all, Tag.number == uniq[i])
  tag<-temp.dat$Tag.number
  tag<-paste("currently plotting",unique(tag),sep=" ")
  print(tag)
  
  map.all.leaflet <- map.all.leaflet%>%
    addPolylines(data=temp.dat, ~Release.longitude,~Release.latitude, group = ~Tag.number,weight = 0.25,fillOpacity = 0.75,color = ~pal(Sex))
}

map.all.leaflet



  