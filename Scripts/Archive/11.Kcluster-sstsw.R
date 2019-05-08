# Explore catch data----
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

setwd(data.dir)
dir()
k.dat <-read_csv("k.data.csv")%>%
  dplyr::filter(!is.na(moult))%>% #These are the recaptures who's length bin didn't have enough data to form moult clustering
  glimpse()

setwd(data.dir)
write.csv(k.dat, "kcluster.csv", row.names = F)

#Find Averages GPS per Location so can find average sst per block
glimpse(k.dat)

av.lat.lon <- k.dat%>%
  select(Location.int, long.int, lat.int)%>%
  



test<- av.lat.long




#Bring in SST data (Thanks Matt)----

dat.sst<-gs_title("dat.all.sst")%>% # To use GoogleSheets
  gs_read_csv(ws = "Sheet1", row.names=FALSE)%>%
  select(Date, Location, Site, sst, SiteNo)%>%
  distinct()%>%
  glimpse()

#Easiest way to join sst data & k-means kluster data is by either:
#1. lat and long (SiteNo) will need two columns: 1 for initial capture location and one for recapture location
#OR
#Find average per site and join by site...and date, will still need two columns, hmm

dat.sst%<>%
  select(Date, Site, sst)%>%
  group_by(Date, Site) %>%
  summarise_all(funs(mean))%>%
  glimpse()

glimpse(dat.sst)

#Create data frame to intial tag data----

k.dat.int<-k.dat%>%
  select(Date, Site)%>%
  glimpse()

k.dat.int.sst <- left_join(k.dat.int, dat.sst)%>%
  glimpse() #Don't have all sst data- need to ask Matt!

#Join sst data with Kmeans cluster data----

k.dat.sst<- left_join(k.dat, dat.sst)%>%
  glimpse()







#Bring in swell data----

dat.swell.18 <-gs_title("JDW2018")%>%
  gs_read_csv(ws="Sheet1", header=TRUE)%>%
  mutate(Date=as.Date(Date,format= "%d/%m/%Y"))%>%
  fill(2:12, .direction = c("down"))%>%
  group_by(Date) %>%
  dplyr::summarise_all(funs(mean))%>%
  distinct()%>%
  dplyr::rename("Hs.m.sw"="Hs(m).sw",
                "Hs.m.sea"="Hs(m).sea",
                "T1.s.sw"="T1(s).sw",
                "T1.s.sea"="T1(s).sea")%>%
  select(Date, Hs.m.sw, Hs.m.sea, T1.s.sw, T1.s.sea)%>%
  ungroup()%>%
  glimpse()

#2017 data----
#Damn. Forgot about that. Added as sheet 2 to 2018 data. 

dat.swell.17 <-gs_title("JDW2018")%>%
  gs_read_csv(ws="Sheet2", header=TRUE)%>%
  mutate(Date=as.Date(Date,format= "%d/%m/%Y"))%>%
  #mutate(Date= lubridate::ymd_hm(Date))%>%
  fill(2:12, .direction = c("down"))%>%
  group_by(Date) %>%
  summarise_all(funs(mean))%>%
  distinct()%>%
  dplyr::rename("Hs.m.sw"="Hs(m).sw",
                "Hs.m.sea"="Hs(m).sea",
                "T1.s.sw"="T1(s).sw",
                "T1.s.sea"="T1(s).sea")%>%
  select(Date, Hs.m.sw, Hs.m.sea, T1.s.sw, T1.s.sea)%>%
  ungroup()%>%
  glimpse()

#combine 2017 & 2018 swell data----  
dat.swell<- rbind(dat.swell.17, dat.swell.18)%>%
  glimpse()

#Want to find the min, max and average value for Hs.m and T1.s for each recapture...soooo...
#let's reduce the data to only the sampling time-frame: Between 27-11-2017 to 05-12-2018

dat.swell%<>%
  filter(Date >= as.Date("2017-11-27") & Date<= as.Date("2018-12-05"))%>%
  glimpse()

setwd(data.dir)
write.csv(dat.swell, "dat.swell.csv", row.names = F)


#Initial data----
k.dat.int<-k.dat%>%
  select(Date)%>%
  left_join(dat.swell)%>%
  dplyr::rename("Hs.m.sw.int"="Hs.m.sw",
                "Hs.m.sea.int"="Hs.m.sea",
                "T1.s.sw.int"="T1.s.sw",
                "T1.s.sea.int"="T1.s.sea")%>%
  dplyr::rename("date.int"="Date")%>%
  glimpse()

#Recap date data----

k.dat.recap<-k.dat%>%
  select(recap.Date)%>%
  dplyr::rename("Date"="recap.Date")%>% #change recap.date to 'Date' so can join with swell data
  left_join(dat.swell)%>%
  dplyr::rename("Hs.m.sw.rec"="Hs.m.sw",
                "Hs.m.sea.rec"="Hs.m.sea",
                "T1.s.sw.rec"="T1.s.sw",
                "T1.s.sea.rec"="T1.s.sea")%>%
  dplyr::rename("date.rec"="Date")%>%
  
  glimpse()

#Join k.dat dat with both sets of swell data
glimpse(k.dat)

k.dat.sw<- k.dat%>%
  dplyr::rename("date.int"="Date", "date.rec"="recap.Date")%>%
  left_join(k.dat.int, by="date.int")%>%
  left_join(k.dat.recap, by="date.rec")%>%
  glimpse()
  
#Save just swell data for now, will eventually have sst from Matt!!
setwd(data.dir)
write_csv(k.dat.sw, "k.dat.sw.csv")




