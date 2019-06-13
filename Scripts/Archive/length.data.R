# Explore length data----
rm(list=ls()) # Clears memory

# librarys----
library(tidyr)
library(dplyr)
library(googlesheets)
library(stringr)
library(lubridate)
library(tidyverse)

# Study name----
study<-"Lobster.Data"

# Set work directory----

#For Desktop
# work.dir=("C:/Users/00097191/Google Drive/MEG/Projects/Projects_WRL/Project_WRL_low-catch zone/Fieldwork and Reporting/03_Trapping/Analysis_WRL_Reds_2018")

#For Laptop
# setwd("~/Google Drive/Analysis_WRL_Reds_2018/Data")
# setwd(work.dir)
# dir()

#For Tims Github 

work.dir=("~/GitHub/Analysis_Miller_WRL") #for Tim's github

work.dir=("~/workspace/Analysis_Miller_WRL") #for ecocloud server

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
# Bring in Ash and Oscar's data
length.uwa <-read_csv("dat.all.17-18.csv")%>%
  mutate(Location=Site)%>% 
  glimpse()


length.uwa%<>%
  mutate(Location=str_replace_all(.$Location,c("Seven Mile Beach"= "Seven Mile","Little Horseshoe"="Cliff Head", "Cliff Head North"="Cliff Head","Cliff Head Mid"= "Cliff Head","Cliff Head South"="Cliff Head","Cliff Head OUT1"= "Cliff Head","CHM"="Cliff Head", "Davids Marks"="Cliff Head","CHM"= "Cliff Head", "CHS"="Cliff Head", "CHN"="Cliff Head", "Jim Bailey"="Irwin Reef", "Long Reef"="Irwin Reef", "South Dummy"="Irwin Reef","South Rig"= "Irwin Reef","Whites Lump"= "Irwin Reef","WP"= "Irwin Reef","Whitepoint"="Irwin Reef")))%>% 
  mutate(Site=str_replace_all(.$Site, c("Jim Bailey"="White Point" ,"WP"="White Point" , "Whitepoint"="White Point" , "CHS"="Cliff Head South","CHM"="Cliff Head Mid","CHN"="Cliff Head North", "Seven Mile Beach"= "Seven Mile.out")))%>%
  select(Date, Tag.number, Carapace.length, Location, Sex, Longitude, Latitude, Site)%>%
  glimpse()


#Bring in Ben's Seven Mile data----

length.smb <- gs_title("Lobster_Data_Fisheries_SMB")%>%
  gs_read_csv("Dat.smb")%>%
  mutate(Tag.number=as.character(Tag.number))%>%
  mutate(Location=Site)%>% 
  glimpse()

length.smb%<>%
  mutate(Site=str_replace_all(.$Site,c("Seven Mile Beach"="Seven Mile.in")))%>%
  mutate(Location=str_replace_all(.$Location,c("Seven Mile Beach"="Seven Mile")))%>%
  mutate(Trip=paste("T",Trip,sep=""))%>%
  mutate(month=format(as.Date(Date),'%m'))%>%
  mutate(month=month((as_date(Date))))%>%
  filter(month%in%c(5:12))%>% #Remove Jan-April
  select(Date, Tag.number, Carapace.length, Location, Sex, Longitude, Latitude, Site)%>%
  glimpse()
  
  #Combine Bens (fisheries) and uwa data----


dat.all.length<- rbind(length.uwa, length.smb)%>%
  glimpse()


#SAVE DATA----
setwd(data.dir)
write.csv(dat.all.length, "dat.all.length.csv", row.names=F)

#IMPORT DATA AND COMBINE WITH SWELL DATA----

dat.all.length<-read_csv("dat.all.length.csv")%>%
  glimpse()

#Bring in Swell Data----

dat.swell<-read_csv("dat.swell.csv")%>%
  glimpse()

#combine----

length.swell<-left_join(dat.all.length, dat.swell)%>%
  glimpse()

#SAVE SWELL DATA AND LENGTH
setwd(data.dir)
write.csv(length.swell, "length.swell.csv", row.names=F)

#IMPORT LENGTH DATA----

#GET DATA READY FOR MATTS SST FUNCTION (NOT CURRENTLY WORKING)
glimpse(length.swell)

length.sst<-length.swell%>%
  select(Date, Longitude, Latitude)%>%
  dplyr::rename("Lat"="Latitude", "Long"="Longitude")%>%
  # mutate(Lat=as.numeric(Lat))%>%
  # mutate(Long=as.numeric(Long))%>%
  unite(SiteNo, c(Lat,Long), sep = "", remove = F)%>% #Okay can't figure out how to 
    glimpse()

#USE MATTS SST FUNCTION----
length.sst$sst<-get.sst(length.sst)

#yyyyyyyyyyyaaaaaaaaaaaaaaaaaaaaaaaaaassssssssssssss it worked!

glimpse(length.sst)

av.length.sst<-length.sst%>% #Some have the same lats and longs and sst 
  select(Date, SiteNo, sst)%>%
  group_by(Date, SiteNo)%>%
  summarise_all(funs(mean))%>%
  glimpse()

length.swell%<>%
  unite(SiteNo, c(Latitude,Longitude), sep ="" , remove = F)%>% #Okay can't figure out how to 
  glimpse()

length.sw.sst <- left_join(test, av.length.sst, by=c("SiteNo", "Date"))%>%
  glimpse()

#Fucking finally~~~~~~holy moly!!!!

setwd(data.dir)
write.csv(length.sw.sst, "length.sw.sst.csv", row.names = F)

