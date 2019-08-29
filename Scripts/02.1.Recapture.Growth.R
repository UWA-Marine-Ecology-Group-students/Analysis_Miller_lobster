#Edited Recappture data for growth analysis (Fabens and Kmeans)
#Need to remove outliers, location change and inconsistent measurements (Bruce Cockmans) that don't need to be removed for the other recapture analysis (Movement).

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
library(lubridate)

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
  #filter(!Fisher%in%c("Bruce Cockman"))%>% # Filter out Bruce Cockmans data
  glimpse()

#Length data
length<-read.csv("length.csv")%>%
  #filter(is.na(Total.damage)|!Total.damage>2)%>% #removes 6
  #filter out 33 individuals in total that have moved Location (not neccarily site)
  #dplyr::filter(!Tag.number %in% c("K1054", "K1565", "K1604", "191162", "K2485", "K2446", "195768", "195655", "K2934", "195997", "195997", "195256", "195958", "195985", "196281", "K4249","K4248", "K4955", "197205", "198251"))%>%
  filter(!Tag.number%in%c("K2400","K1617","K1221","K0653","K0457","198428","196072","K4501","198821","K2519","K2402","K3805","K1019", "K1523"))%>% # Filter out outliers
  glimpse()

names(metadata)
names(length)

# Filter out issues in metadata ----
metadata.clean<-metadata%>%
  filter(!Exclude.pots%in%c("Yes"))%>% # Remove bad pots. AM changed to "yes" from no?
  dplyr::filter(!Location%in%c("Rivermouth"))%>% # Filter out Rivermouth
  select(Source, Sample, Trip, Day, Pot.number, Location, Site, Date, Day.pull, Latitude, Longitude, Pot.remarks, Pwo, Pwf)%>%
  dplyr::mutate(Date=lubridate::as_date(ymd(Date)))%>%
  glimpse()

# Filter out issues in length data ---
length.clean<-length%>%
  filter(!is.na(Tag.number))%>% # Filter out individuals without Tag
  filter(!is.na(Carapace.length))%>% #Filter out individuals with no length measurement
  semi_join(.,metadata.clean, by="Sample")%>% # Keep only those that have metadata
  select(-c(Outlier,Dead, Date,Count, Source, Trip, Cable.tie))%>%
  glimpse()

# Join datasets together ----
dat<-left_join(length.clean,metadata.clean, by="Sample")%>%
  select(Source,Sample, Trip, Date,Location,Site,Latitude, Longitude, Tag.number, Carapace.length, Sex, Colour,Total.damage)%>%
  #dplyr::mutate(Date=lubridate::as_date(ymd(Date)))%>%
  mutate(Location=str_replace_all(.$Location,c("Little HorseshoeS"="Little Horseshoe")))%>%
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
  dplyr::rename(Date.recap=Date, 
                Location.recap=Location, 
                Site.recap=Site, 
                Carapace.length.recap=Carapace.length,
                Sex.recap=Sex, 
                Colour.recap=Colour,
                Total.damage.recap=Total.damage, 
                Longitude.recap=Longitude,
                Latitude.recap=Latitude)%>%
    glimpse()

#Bind to make a long dataframe of all recaptured----
total.recaptures<-rbind(just.initial, just.recaps)%>%
  arrange(Tag.number)%>%
  glimpse()


#Combine Initial and Recaptures----
dat.rr<- dplyr::left_join(just.initial, just.recaps, by="Tag.number")%>%
  mutate(inc=Carapace.length.recap-Carapace.length)%>% #growth increment
  dplyr::mutate(Ldys=Date.recap-Date)%>% #Days between capture
  glimpse()

#Filter 0 = Ldys, weird number of repeats in Hebbos----
dat.rr%<>%
  filter(!Ldys=="0")%>%
  glimpse()
#119-mostly hebbos and a few of oscars

#Save for k-means and fabens----
setwd(data.dir)
write.csv(dat.rr, "Growth.Data.csv", row.names = F)

