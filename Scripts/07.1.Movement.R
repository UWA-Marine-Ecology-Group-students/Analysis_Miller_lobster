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
study<-"Recapture.Distance"

# Set work directory ----
#work.dir=("Z://Analysis_Miller_lobster") #for laptop
work.dir=("C:/GitHub/Analysis_Miller_lobster") # For Brooke


# Set sub-directories ----
data.dir=paste(work.dir,"Data",sep="/")
plot.dir=paste(work.dir,"Plots",sep="/")

#Import recapture data (created in '02.Recapture.Data.190515.R' script)
#dat.rr<- read.csv("Recapture.Data.csv")%>%
dat.rr<- read.csv("Growth.Data.csv")%>%
  glimpse()
  
#Add distance to data----

dist.1<-dat.rr%>%
  select( Longitude, Latitude)%>% #Tag.number, Location.int,
  glimpse()

dist.2<-dat.rr%>%
  select(Longitude.recap, Latitude.recap)%>% #Tag.number, Location.int,
  glimpse()

# create distance matrix----
mat <- distGeo(dist.1,dist.2)
dat.rr$distance <- mat/1000 #Add coloum of distance (km)
glimpse(dat.rr)

#Add Bearing----
dat.rr$bearing<-bearingRhumb(dist.1, dist.2)
glimpse(dat.rr)

#Allocate size class for recaptures-Ones that changes sublegal->legal can fall into the Legal category
dat.movement<-dat.rr%>%
  mutate(sizeclass= ifelse(Carapace.length.recap>=76.0,"Legal", "Sublegal"))%>%
  glimpse()

#Save for GAMM
setwd(data.dir)
write.csv(dat.movement, "Movement.Data.csv", row.names = F)  

