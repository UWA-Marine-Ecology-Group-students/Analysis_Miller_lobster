# Explore catch data ----
rm(list=ls()) #clear memory

# librarys ----
library(tidyr)
library(dplyr)
library(googlesheets)
library(stringr)
library(ggplot2)
library(magrittr)
library(readr)
library(geosphere)

# Study name ----
study<-"Growth.Rate"

# Set work directory ----
#work.dir=("C:/GitHub/Analysis_Miller_lobster") # For Brooke
work.dir=("Z:/Analysis_Miller_lobster") # FOr Ash's laptop using Git

# Set sub-directories ----
data.dir=paste(work.dir,"Data",sep="/")
plot.dir=paste(work.dir,"Plots",sep="/")

# Import data ----
setwd(data.dir)
dir()

#Pot data
metadata<-read.csv("metadata.csv")%>%
  glimpse()

#Length data
length<-read.csv("length.csv")%>%
  glimpse()

names(metadata)
names(length)

# Filter out issues in metadata ----
metadata.clean<-metadata%>%
  filter(!Exclude.pots%in%c("Yes"))%>% # Remove bad pots. AM changed to "yes" from no?
  dplyr::filter(!Location%in%c("Rivermouth"))%>% # Filter out Rivermouth
  select(-c(Repeated.samples,Exclude.pots,Depth.lidar,Depth.fms,Fisher.email,Site.code))%>%
  glimpse()

# Filter out issues in length data ---

length.clean<-length%>%
  filter(!is.na(Tag.number))%>% # Filter out individuals without Tag
  filter(!is.na(Carapace.length))%>% #Filter out individuals with no length measurement
  semi_join(.,metadata.clean)%>% # Keep only those that have metadata
  select(-c(Outlier,Dead))%>%
  glimpse()

# Join datasets together ----
dat<-left_join(length.clean,metadata.clean)%>%
  select(Source,Sample, Trip, Date,Location,Site,Latitude, Longitude, Tag.number, Carapace.length, Sex, Colour,Total.damage)%>%
  dplyr::mutate(Date=lubridate::as_date(ymd(Date)))%>%
  glimpse()
  
#Get a list of recaptured tags----
recaps<- dat%>%
  select(Tag.number)%>%
  mutate(duplicates = duplicated(.) | duplicated(., fromLast = TRUE))%>%
  filter(duplicates=="TRUE")%>%
  glimpse()

#Join with other data----
all.recaps <- semi_join(dat, recaps)%>%
  glimpse()

#Find initial tag----
#Order by earliest date to latest
dat.order<- all.recaps%>%
  arrange(Date)%>% #Have to the arrange as a separate pipe for some reason
  glimpse()

just.initial<-dat.order%>%
  dplyr::distinct(Tag.number,.keep_all = TRUE)%>% 
  #filters out the duplicates (recaptues)
  select(Date, Location, Site,Tag.number, Carapace.length, Sex, Colour,Total.damage, Longitude, Latitude)%>% #Simplify
  glimpse()

# Find Only Recaptures (not intial tags)----
just.recaps <- dat.order[duplicated(dat.order$Tag.number), ]%>%
  select(Date, Location, Site,Tag.number, Carapace.length, Sex, Colour,Total.damage, Longitude, Latitude)%>% 
  glimpse()

#Bind to make a long dataframe of all recaptured----
total.recaptures<-rbind(just.initial, just.recaps)%>%
  arrange(Tag.number)%>%
  glimpse()

#rename variables in recapture dataframe----
names(just.recaps)
just.recaps.new<-just.recaps%>%
  dplyr::rename(Date.recap=Date)%>%
  dplyr::rename(Location.recap=Location)%>%
  dplyr::rename(Site.recap=Site)%>%
  dplyr::rename(Carapace.length.recap=Carapace.length)%>%
  dplyr::rename(Sex.recap=Sex)%>%
  dplyr::rename(Colour.recap=Colour)%>%
  dplyr::rename(Total.damage.recap=Total.damage)%>%
  dplyr::rename(Longitude.recap=Longitude)%>%
  dplyr::rename(Latitude.recap=Latitude)%>%
  glimpse()

#Combine Initial and Recaptures----
dat.rr<- dplyr::left_join(just.initial, just.recaps.new, by="Tag.number")%>%
  mutate(inc=Carapace.length.recap-Carapace.length)%>% #growth increment
  dplyr::mutate(Lyrs=Date.recap-Date)%>% #Days between capture
  glimpse()

#Save for kMovement Analysis
setwd(data.dir)
write.csv(dat.rr, "Recapture.Data.csv", row.names = F)

