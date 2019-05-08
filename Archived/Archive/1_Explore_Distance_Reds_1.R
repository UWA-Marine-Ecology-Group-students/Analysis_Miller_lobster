# Explore catch data----
rm(list=ls()) #clear memory

# librarys----
library(tidyr)
library(dplyr)
options(dplyr.width = Inf) #enables head() to display all coloums
library(forcats)
library(readxl)
library(googlesheets)
library(stringr)
library(ggmap)
library(ggplot2)
library(cowplot)
library(argosfilter)

# Calculate the distance from boat ramp for each sample----
# Ramps
ids <- factor(c("Exmouth",
                "Bundegi",
                "Onslow",
                "Pilbara",
                "Dampier",
                "WithnellBay",
                "BreadonCreek",
                "Tantabiddi",
                "Coral Bay",
                "Fortescue River"
                #                 "Warroora",
                #                 "Gnaraloo"
)) 
ramps <- data.frame(
  id = rep(ids, each = 1),
  
  y = c(-21.97970240551842,
        -21.84371102054585,
        -21.69503107602818,
        -21.0840122708165,
        -20.66562571673585,
        -20.539744728225,
        -21.648725,
        -21.912580,
        -23.155828,
        -21.028040)
  #         -23.485963,
  #         -23.873282)
  ,x = c(114.1461956058358,
         114.1882002543887,
         114.9237761395207,
         115.9306982155436,
         116.6746382564552,
         116.7980700539128,
         115.131243,
         113.978251,
         113.767124,
         116.029232))
#                          113.772874,
#                          113.497430))
head(ramps,15)

head(bruv.combined)
distance.to.ramp<-bruv.combined%>%
  select(OpCode,Latitude,Longitude)%>%
  mutate(To.Exmouth=distance(lat1=ramps[1,2],lat2=.$Latitude,lon1=ramps[1,3],lon2=.$Longitude))%>%
  mutate(To.Bundegi=distance(lat1=ramps[2,2],lat2=.$Latitude,lon1=ramps[2,3],lon2=.$Longitude))%>%
  mutate(To.Onslow=distance(lat1=ramps[3,2],lat2=.$Latitude,lon1=ramps[3,3],lon2=.$Longitude))%>%
  mutate(To.Pilbara=distance(lat1=ramps[4,2],lat2=.$Latitude,lon1=ramps[4,3],lon2=.$Longitude))%>%
  mutate(To.Dampier=distance(lat1=ramps[5,2],lat2=.$Latitude,lon1=ramps[5,3],lon2=.$Longitude))%>%
  mutate(To.WithnellBay=distance(lat1=ramps[6,2],lat2=.$Latitude,lon1=ramps[6,3],lon2=.$Longitude))%>%
  mutate(To.BreadonCreek=distance(lat1=ramps[7,2],lat2=.$Latitude,lon1=ramps[7,3],lon2=.$Longitude))%>%
  mutate(To.Tantabiddi=distance(lat1=ramps[8,2],lat2=.$Latitude,lon1=ramps[8,3],lon2=.$Longitude))%>%
  mutate(To.CoralBay=distance(lat1=ramps[9,2],lat2=.$Latitude,lon1=ramps[9,3],lon2=.$Longitude))%>%
  mutate(To.Fortescue=distance(lat1=ramps[10,2],lat2=.$Latitude,lon1=ramps[10,3],lon2=.$Longitude))%>%
  #   mutate(To.Warroora=distance(lat1=ramps[11,2],lat2=.$Latitude,lon1=ramps[11,3],lon2=.$Longitude))%>%
  #   mutate(To.Gnaraloo=distance(lat1=ramps[12,2],lat2=.$Latitude,lon1=ramps[12,3],lon2=.$Longitude))%>%
  mutate(Distance.to.ramp=do.call(pmin, .[,4:13]))%>%
  select(OpCode,Distance.to.ramp)%>%
  distinct() #need to be distinct otherwise joins dont work
head(distance.to.ramp)
names((distance.to.ramp))