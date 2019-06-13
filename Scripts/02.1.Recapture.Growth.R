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
  filter(!Fisher%in%c("Bruce Cockman"))%>% # Filter out Bruce Cockmans data
  glimpse()

#Length data
length<-read.csv("length.csv")%>%
  filter(is.na(Total.damage)|!Total.damage>2)%>% #removes 6
  #filter out 33 individuals in total that have moved Location (not neccarily site)
  dplyr::filter(!Tag.number %in% c("K1054", "K1565", "K1604", "191162", "K2485", "K2446", "195768", "195655", "K2934", "195997", "195997", "195256", "195958", "195985", "196281", "K4249","K4248", "K4955", "197205", "198251"))%>%
  filter(!Tag.number%in%c("K2400","K1617","K1221","K0653","K0457","198428","196072","K4501","198821","K2519","K2402","K3805","K1019", "K1523"))%>% # Filter out outliers
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

#Add in 2019 data---

# #Bring 2019 pot data
# potdata.19<-gs_title("Lobsters_data_2019_All")%>% 
#   gs_read_csv(ws = "Pot.var")%>%
#   mutate(Source = 'Dongara.Millers.Masters')%>%
#   mutate(Sample=paste(Trip,Day,Trap.ID,sep="."))%>% 
#   dplyr::rename(Latitude=Latitude.y, Longitude=Longitude.x,Date=Date.Recovered)%>%
#   dplyr::mutate(Date=as_date(dmy(Date)))%>%
#   mutate(Latitude=as.numeric(Latitude))%>%
#   mutate(Longitude=as.numeric(Longitude))%>%
#   mutate(Site=str_replace_all(.$Site,c("GR"="Golden Ridge", "WL"="Whites Lump", "CHS"="Cliff Head South", "CHM"= "Cliff Head Mid", "CHN"="Cliff Head North", "CHout2."="Cliff Head North", "LHS"= "Little Horseshoe", "SR"="South Rig", "WP"="White Point" )))%>% 
#   mutate(Location=str_replace_all(.$Site,c("Whites Lump"="White Point", "Cliff Head South"="Cliff Head","Cliff Head Mid"= "Cliff Head","Cliff Head North"= "Cliff Head","South Rig"="White Point")))%>%
#   select(Sample,Location,Site,Latitude, Longitude)%>%
#   glimpse()
# 
# 
# #Bring in lobster data 2019
# lengthdata.19<-gs_title("Lobsters_data_2019_All")%>% 
#   gs_read_csv(ws = "Lobster.var")%>%
#   mutate(Source = 'Dongara.Millers.Masters')%>%
#   mutate(Sample=paste(Trip,Day,Trap.ID,sep="."))%>% 
#   mutate(Total.damage=0)%>% #CHANGE LATER
#   dplyr::mutate(Date=as_date(dmy(Date)))%>%
#   filter(!is.na(Tag.number))%>% # Filter out individuals without Tag
#   filter(!is.na(Carapace.length))%>% #Filter out individuals with no length measurement
#   mutate(Colour=str_replace_all(.$Colour,c("W"="White", "R"="Red")))%>%
#   mutate(Sex=str_replace_all(.$Sex, c("M"="Male", "F"="Female","U"="Unknown")))%>%
#   mutate(Sex=if_else((!is.na(Colour)&!Sex%in%c("Female","Male")),"Unknown",Sex))%>%
#   select(Sample, Source, Trip, Date, Tag.number, Carapace.length, Sex, Colour, Total.damage)%>%
#   glimpse()
# 
# 
# length(unique(potdata.19$Sample))
# length(unique(lengthdata.19$Sample))
# 
# dat.19<- left_join(lengthdata.19,potdata.19, by="Sample")%>%
#   glimpse()
# 
# glimpse(dat)
# glimpse(dat.19)
# dat.1 <-rbind(dat, dat.19)

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

#Save for k-means and fabens


setwd(data.dir)
write.csv(dat.rr, "Growth.Data.csv", row.names = F)
