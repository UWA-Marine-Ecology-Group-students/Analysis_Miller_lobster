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

# Study name ----
study<-"Growth.Rate"

# Set work directory ----
work.dir=("C:/GitHub/Analysis_Miller_lobster") # For Brooke
# work.dir=("Z:/Analysis_Miller_lobster") # FOr Ash's laptop using Git

# Set sub-directories ----
data.dir=paste(work.dir,"Data",sep="/")
plot.dir=paste(work.dir,"Plots",sep="/")

# Import data ----
setwd(data.dir)
dir()

metadata<-read.csv("metadata.csv")
length<-read.csv("length.csv")

names(metadata)
names(length)

# Filter out issues in metadata ----
metadata.clean<-metadata%>%
  filter(Exclude.pots%in%c("No"))%>% # Remove bad pots
  filter(is.na(Pwo))%>% # Remove any pots with an Octopus
  filter(!Fisher%in%c("Bruce Cockman"))%>% # Filter out Bruce Cockmans data
  dplyr::filter(!Location%in%c("Rivermouth"))%>% # Filter out Rivermouth
  select(-c(Repeated.samples,Exclude.pots,Depth.echosounder,Distance.from.shore.perpendicular.kilometer,Distance.from.shore.nearestpoint.kilometer,Days.from.new.moon,Slope,Aspect,Curv.profile,Curvature,Curv.plan,Rugosity,Depth.lidar,Sd.10m,Sd.15m,Sd.20m,Sd.25m,Sd.50m,Sd.75m,Sd.100m,Sd.200m,Sd.500m,Sd.1000m,Puerulus.count.1k,Puerulus.count.2k,Puerulus.count.3k,Substrate,Depth.fms,Fisher.email,Site.code))%>%
  glimpse()

# Filter out issues in length data ---
length.clean<-length%>%
  filter(!Dead%in%c("Dead"))%>% # Remove any dead lobsters
  #filter(!Outlier%in%c("y"))%>% # Remove any that have been marked as an outlier
  filter(!is.na(Tag.number))%>% # Filter out individuals without Tag
  filter(!is.na(Carapace.length))%>% #Filter out individuals with no length measurement
  filter(!Tag.number%in%c("K2400","K1617","K1221","K0653","K0457","K1045","K0755","198428","196072","K4501","198821","K2519","K2402","K3805","K1019"))%>% # Filter out some more outliers
  semi_join(.,metadata.clean)%>% # Keep only those that have metadata
  select(-c(Outlier,Dead))%>%
  glimpse()

# Test for lobsters that have changed sex
changed.sex<-length%>%
  filter(!is.na(Tag.number))%>%
  group_by(Tag.number)%>%
  summarise(no.sex=length(unique(Sex)),no.times.caught=n())%>%
  filter(no.sex>1)

no.fem<-semi_join(length,changed.sex)%>%
  filter(Sex=="Female")%>%
  group_by(Tag.number)%>%
  summarise(no.fem=n())

no.male<-semi_join(length,changed.sex)%>%
  filter(Sex=="Male")%>%
  group_by(Tag.number)%>%
  summarise(no.male=n())

changed.sex<-left_join(changed.sex,no.fem)%>%
  left_join(.,no.male)


# Join datasets together ----
dat<-left_join(length.clean,metadata.clean)

# New recaptured ----
# All recaptures: including from same trip)
recaps<- dat%>%
  select(Tag.number)%>%
  mutate(duplicates = duplicated(.) | duplicated(., fromLast = TRUE))%>%
  filter(duplicates=="TRUE")%>%
  glimpse()

all.recaps<-semi_join(dat,recaps)%>% 
  group_by(Tag.number,Trip)%>%
  glimpse()

# For Seven Mile without months 1->4----

growth.sm<-dat.smb.edit%>%
  select(Trip,Date,Tag.number, Carapace.length, Site, Sex, Colour, Total.damage, Longitude, Latitude, mini.site)%>% 
  group_by(Tag.number)%>%
  mutate(Trip=paste("T",Trip,sep=""))%>%
  glimpse()

# For data including recaps from same trip----
growth.all<-All.recaps%>%
  select(Trip, Date,Tag.number, Carapace.length, Site, Sex, Colour, Total.damage, mini.site, Longitude, Latitude)%>% 
  group_by(Tag.number)%>%
  mutate(Trip=paste("T",Trip,sep=""))%>%
  glimpse()

# # #Combine growth.1 and growth.2 dataframes
# #And for growth data without month 1-4
recapture.data<- rbind(growth.3, growth.4)%>%
  glimpse()

# growth.comb.edit <- rbind(growth.1, growth.3)

#All recaptures----

#Save data for growth Models
# growth.all <- rbind(growth.2, growth.4)
# setwd(data.dir)
# write.csv(growth.all, "growth.all.csv")
# 
# #Save for sm no jan- april
# growth.edited <- rbind(growth.3, growth.4)
# setwd(data.dir)
# write.csv(growth.edited, "growth.edited.csv")


#For Edited SM data (months 1-4 removed)----
#Order by earliest date to latest
growth.sm<- growth.sm%>%
  arrange(Date)%>% #Have to the arrange as a separate pipe for some reason
  glimpse()

smb.initial <- growth.sm%>%
  select(Date, Tag.number, Carapace.length, Site, Sex, Colour,Trip,Total.damage, mini.site, Longitude, Latitude)%>%
  dplyr::distinct(Tag.number,.keep_all = TRUE)%>% #keeps only the first tag cl, filters out the duplicates (recaptues)
  dplyr::rename(initial.cl= Carapace.length)%>%
  glimpse()

# #Create dataframe for recaptures only

smb.recap <- growth.sm[duplicated(growth.sm$Tag.number), ]%>%
  select(Date, Tag.number, Carapace.length, Site, Sex, Colour,Trip, Total.damage, mini.site, Longitude, Latitude)%>%
  dplyr::rename(recap.Date = Date)%>%
  dplyr::rename(recap.cl=Carapace.length)%>%
  glimpse()


length(smb.recap$Tag.number)
#206
length(smb.initial$Tag.number)
#253
#Combine Initial captures and recaptures
#Not left join as now not all are recaptures cause we removed Jan-April
release.recap.sm <-inner_join(smb.initial, smb.recap, by="Tag.number")%>% 
  glimpse()


#Growth data #4 (All recaptures)-----
#Create dataframe for initial captures only
glimpse(growth.all)
2901-2838 # >2 removes 63 
2901-2860 # >3 removes 41
  
#Filter out damaged individuals more than 2----
growth.all<- growth.all%>%
  filter(!Total.damage>2)%>%
  glimpse()


all.initial <- growth.all%>%
  select(Date, Tag.number, Carapace.length, Site, Sex, Colour,Trip, Total.damage, mini.site, Longitude, Latitude)%>%
  arrange(Date)%>% #Order by date
  dplyr::distinct(Tag.number,.keep_all = TRUE)%>% #keeps only the first tag cl, filters out the duplicates (recaptues)
  dplyr::rename(initial.cl= Carapace.length)%>%
  glimpse()

# #Create dataframe for recaptures only
all.recap <- growth.all[duplicated(growth.all$Tag.number), ]%>%
  arrange(Date)%>%
  select(Date, Tag.number, Carapace.length, Site, Sex, Colour,Trip, Total.damage, mini.site, Longitude, Latitude)%>%
  dplyr::rename(recap.Date = Date)%>%
  dplyr::rename(recap.cl=Carapace.length)%>%
  glimpse()

length(all.recap$Tag.number)
#1565 recaptured
length(unique(all.initial$Tag.number))
#1288

#Combine Initial captures and recaptures for mine and oscars----
#filter out individuals that have moved sites
# 33 individuals in total that have moved Location (not neccarily site)
release.recap.all <- left_join(all.initial, all.recap, by="Tag.number")%>%
  dplyr::filter(!Tag.number %in% c("K1054", "K1565", "K1604", "191162", "K2485", "K2446", "195768", "195655", "K2934", "195997", "195997", "195256", "195958", "195985", "196281", "K4249","K4248", "K4955", "197205", "198251"))%>%
  glimpse()

#1574

#Combine my data with Edited SM data----

dat.rr.clean <- bind_rows(release.recap.all, release.recap.sm)%>% #Combine Ash's edited data with Ben's data no jan-apr
  dplyr::rename(Location.int = Site.x, Sex.int = Sex.x, Colour.int=Colour.x, Trip.int= Trip.x, Total.damage.int=Total.damage.x)%>%
  dplyr::rename(Location.rec = Site.y, Sex.rec = Sex.y, Colour.rec=Colour.y, Trip.rec= Trip.y, Total.damage.rec=Total.damage.y)%>%
  glimpse()

setwd(data.dir)
write.csv(dat.rr.clean, "dat.rr.clean.csv", row.names = F)


