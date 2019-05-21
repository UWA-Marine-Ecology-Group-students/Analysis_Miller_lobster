# Export catch data to GlobalArchive ----
rm(list=ls()) # Clears memory

# librarys----
library(tidyr)
library(dplyr)
library(stringr)
library(readr)

# Set work directory ----
work.dir=("C:/GitHub/Analysis_Miller_lobster") # For Brooke

# Sub directories ----
data.dir<-paste(work.dir,"Data",sep="/")

# Import data ----
setwd(data.dir)
dir()

metadata<-read.csv("metadata.csv")%>%
  filter(Source%in%c("oscar-doncel-canons-masters","ash-millers-masters"))%>%
  mutate(Source=str_replace_all(.$Source,c("oscar-doncel-canons-masters"="Dongara.Canons.Masters","ash-millers-masters"="Dongara.Millers.Masters")))%>%
  mutate(CampaignID=paste(str_sub(Date,1,7),Source,"Trapping",sep="_")) # Make a more GlobalArchive-y CampaignID

length<-read.csv("length.csv")%>%
  select(-c(Source))%>%
  rename(Length=Carapace.length)%>%
  mutate(Family="Palinuridae",Genus="Panulirus",Species="cygnus")

# Split data into campaigns based on trip ---
# Metadata must have: Sample,	Latitude,	Longitude,	Date,	Time,	Location,	Status,	Site,	Depth,	Observer,	Successful.count	Succussful.length
# Length file must have: Sample, Family, Genus, Species, Count, Length
# Count file must have: Sample,	Family,	Genus,	Species,	Count
