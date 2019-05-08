# Explore catch data----
rm(list=ls()) #clears memory

# librarys----
library(tidyr)
library(dplyr)
library(googlesheets)
library(stringr)
library(ggplot2)
library(magrittr)
library(readr)
library(lubridate)

# Study name----
study<-"rule5.moulted"


# Set work directory----

#work.dir=("C:/Users/00097191/Google Drive/MEG/Projects/Projects_WRL/Project_WRL_low-catch zone/Fieldwork and Reporting/03_Trapping/Analysis_WRL_Reds_2018") 
# setwd("~/Documents/University/Masters Project/Plots/Plot per recapture")

work.dir=("~/GitHub/Analysis_Miller_WRL") #for Tim's github
work.dir=("~/workspace/Analysis_Miller_WRL") #for ecocloud server

# Set sub-directories----

data.dir=paste(work.dir,"Data",sep="/")
plot.dir=paste(work.dir,"Plots",sep="/")

#Bring in recapture data from ecocloud

setwd("~/workspace/Analysis_Miller_WRL/Data")

dat.growth<-read_csv("growth.comb.csv")%>%
  glimpse()


#Filter out errors from Seven Mile data
#Four tags that have negative growth (more than -7)
#Filter out fisher returns errors
r5<-dat.growth%>%
  filter(Tag.number!="190428" & Tag.number!="190188" & Tag.number!="190124" &Tag.number!="190443")%>%
  filter(Tag.number!="K2400"&Tag.number!="K1617"&Tag.number!="K1221")%>%
  filter(is.na(Site)| Site!="Rivermouth")%>% #Filter out Rivermouth but not NAs
  glimpse()


#Add a column for TRUE if duplicated more than 2 times
r5<- r5%>%
  dplyr::group_by(Tag.number)%>%
  mutate(dupe=n()>2)%>%
  glimpse()

#Filter out recaptures only caught twice:
r5.dupe <-r5%>%
  filter(dupe=="TRUE")%>%
  glimpse()

#Order data by tag number ~ easier to read
r5.dupe <-r5.dupe[order(r5.dupe$Tag.number),]%>%
  glimpse()


setwd(data.dir)
write_csv(r5.dupe, "r.5.moult.csv")

