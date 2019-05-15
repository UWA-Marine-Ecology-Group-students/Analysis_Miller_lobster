# Explore catch data----

rm(list=ls()) #clear memory

# librarys----
library(tidyr)
library(dplyr)
library(googlesheets)
library(stringr)
library(ggplot2)
library(magrittr)
library(readr)

# Study name----
study<-"Growth.Rate"

# Set work directory----

work.dir=("~/GitHub/Analysis_Miller_WRL") #for Tim's github
work.dir=("~/workspace/Analysis_Miller_WRL") #for ecocloud server

# Set sub-directories----
data.dir=paste(work.dir,"Data",sep="/")
plot.dir=paste(work.dir,"Plots",sep="/")


#### Import data from googlesheets ----

# # For Rstudio Server
options(httr_oob_default=TRUE)
# options(httr_oob_default=FALSE) #for desktop
# gs_auth(new_user = TRUE) #only run once

Dat.Combined<-gs_title("Lobster_Data_All_Combined")%>% 
  gs_read_csv(ws = "Sheet1" )%>%
  filter(!is.na(Tag.number))%>% #Filter out individuals with no tag.no
  filter(Tag.number!="CT")%>%   #Filter out weird 'CT' tags from Oscars data  
  filter(!is.na(Carapace.length))%>% #Filter out individuals with no length measurements
  mutate(mini.site=Site)%>% #Create mini.site= Later this will be 'site' 
  mutate(Site=str_replace_all(.$Site,c("Seven Mile Beach"= "Seven Mile","Little Horseshoe"="Cliff Head", "Cliff Head North"="Cliff Head","Cliff Head Mid"= "Cliff Head","Cliff Head South"="Cliff Head","Cliff Head OUT1"= "Cliff Head","CHM"="Cliff Head", "Davids Marks"="Cliff Head","CHM"= "Cliff Head", "CHS"="Cliff Head", "CHN"="Cliff Head", "Jim Bailey"="Irwin Reef", "Long Reef"="Irwin Reef", "South Dummy"="Irwin Reef","South Rig"= "Irwin Reef","Whites Lump"= "Irwin Reef","WP"= "Irwin Reef","Whitepoint"="Irwin Reef")))%>% 
  mutate(mini.site=str_replace_all(.$mini.site, c("Jim Bailey"="White Point" ,"WP"="White Point" , "Whitepoint"="White Point" , "CHS"="Cliff Head South","CHM"="Cliff Head Mid","CHN"="Cliff Head North", "Seven Mile Beach"= "Seven Mile.out")))%>%
  glimpse()

#filter out Bruce Cockmans data, Rive mouth Data & obvious outliers----

Dat.Combined <- Dat.Combined%>%  
  dplyr::filter(is.na(Fisher) | Fisher!="Bruce Cockman")%>%
  dplyr::filter(Tag.number!="K2400"&Tag.number!="K1617"&Tag.number!="K1221"&Tag.number!="K0653" & Tag.number!="K0457" & Tag.number!="K1045"& Tag.number!="K0755")%>% 
  #Obvious fisher return measures (more than -10 growth) & Oscars recaptures
  dplyr::filter(Tag.number!="198428" & Tag.number!="196072" & Tag.number!="K4501")%>% #Outliers from Cliff Head 
  dplyr::filter(Tag.number!="198821" & Tag.number!="K2519" & Tag.number!="K2402")%>% #Outliers from Irwin Reef
  dplyr::filter(Tag.number!="K3805")%>% 
  dplyr:: filter(is.na(Site)| Site!="Rivermouth")%>% 
  glimpse()

  
#Bring in Ben's Seven Mile data----

dat.smb <- gs_title("Lobster_Data_Fisheries_SMB")%>%
  gs_read_csv("Dat.smb")%>%
  mutate(Tag.number=as.character(Tag.number))%>%
  mutate(Site=str_replace_all(.$Site,c("Seven Mile Beach"="Seven Mile")))%>%
  filter(Tag.number!="190428" & Tag.number!="190188" & Tag.number!="190124" &Tag.number!="190443")%>% #four tags have more than -7 growth
  mutate(Trip=paste("T",Trip,sep=""))%>%
  mutate(mini.site="Seven Mile.in")%>%
  glimpse()


#Create another Dataframe for SM with Jan-April removed----
dat.smb.edit<-dat.smb%>%
     mutate(month=format(as.Date(Date),'%m'))%>%
     mutate(month=month((as_date(Date))))%>%
     filter(month%in%c(5:12))%>% #Remove Jan-April
     select(Date, Tag.number, Carapace.length, Sex, Total.damage, Trip, Source, Fisher, Colour, Recapture, Longitude, Latitude, Site, mini.site)%>%
     glimpse()



### new recaptured ----

#All recaptures: including from same trip)
recaps<- Dat.Combined%>%
  select(Tag.number)%>%
  mutate(duplicates = duplicated(recaps) | duplicated(recaps, fromLast = TRUE))%>%
  filter(duplicates=="TRUE")%>%
  glimpse()


All.recaps<-semi_join(Dat.Combined,recaps)%>% 
  group_by(Tag.number,Trip)%>%
  glimpse()


#For Seven Mile without months 1->4----

growth.sm<-dat.smb.edit%>%
  select(Trip,Date,Tag.number, Carapace.length, Site, Sex, Colour, Total.damage, Longitude, Latitude, mini.site)%>% 
  group_by(Tag.number)%>%
  mutate(Trip=paste("T",Trip,sep=""))%>%
  glimpse()

#For data including recaps from same trip----
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


