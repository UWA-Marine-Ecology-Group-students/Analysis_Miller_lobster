# Explore catch data----
rm(list=ls()) # Clears memory

# Study name----
study<-"spatially.explicit"

# librarys----
library(tidyr)
library(dplyr)
# library(googlesheets)
library(stringr)
# library(measurements)
library(lubridate)
library(secr)
library(readr)

# Set work directory----

work.dir=("~/GitHub/Analysis_Miller_WRL") #for Tim's github
work.dir=("~/workspace/Analysis_Miller_WRL") #for ecocloud server

setwd("~/workspace/Analysis_Miller_WRL")

## Sub directories ----
data.dir<-paste(work.dir,"Data",sep="/")
map.dir<-paste(work.dir,"Map Layers",sep="/")
plots.dir<-paste(work.dir,"Plots",sep="/")

#Bring in Data----

secr.dat <- read_csv("Data/Dat.Combined.1.csv")%>%
  glimpse()

#Create capture data

secr.capture <- secr.dat%>%
  select(X1, Trap.ID, Date, Tag.number, Carapace.length, Sex, Colour)%>%
  rename(Detector = Trap.ID, ID=Tag.number)%>%
  filter(!is.na(Carapace.length))%>%
  filter(!is.na(Sex))%>%
  glimpse()

days<- yday(secr.capture$Date)%>% #assigns each date a number from 1 to 365
  glimpse()

# help("yday")

secr.capture <- secr.capture%>%
  mutate(Days= days)%>% 
  select(X1, Detector, ID, Days,Carapace.length, Sex, Colour)%>%
  rename(occasion= Days, session=X1)%>%
  glimpse()

write.table(secr.capture, "captfile.txt", sep="/t")

#Create detector layout format

secr.detector <- secr.dat%>%
  select(Trap.ID, Latitude, Longitude)%>%
  rename(Detector=Trap.ID, X= Latitude, Y= Longitude)%>%
  glimpse()

write.table(secr.detector, "trapfile.txt", sep = "/t")

captfile <-read.txt(file = "captfile")

??read.txt
help("read.txt")
help("system.file")
help("read.capthist")
