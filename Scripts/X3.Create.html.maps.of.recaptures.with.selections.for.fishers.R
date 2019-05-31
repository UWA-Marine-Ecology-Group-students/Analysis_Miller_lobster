# Create html leaflet maps to show fishers
rm(list=ls()) # Clears memory

# librarys----
library(stringr)
library(tidyverse)
library(leaflet)

# Set work directory ----
work.dir=("C:/GitHub/Analysis_Miller_lobster") # For Brooke

# Sub directories ----
data.dir<-paste(work.dir,"Data",sep="/")

# Read in data ----
setwd(data.dir)
dir()

length<-read.csv("length.csv")%>%
  dplyr::select(Source,Sample,Tag.number,Recapture,Sex,Colour,Outlier,Carapace.length)%>%
  #dplyr::filter(!Outlier%in%c("y"))%>%
  dplyr::filter(!Tag.number%in%c("K3211"))%>%
  #dplyr::filter(Tag.number%in%c("191410","191033","K0287","K0966","K0712","K0477"))%>% # Filter to see good test
  glimpse()

metadata<-read.csv("metadata.csv")%>%
  dplyr::filter(!Successful.count%in%c("No"))%>%
  dplyr::distinct(Source,Sample,Location,Latitude,Longitude,Date,Fisher)%>%
  dplyr::filter(!Source%in%c("ben-seven-mile"))%>%
  glimpse()

# Join data together ----
dat<-full_join(length,metadata)%>%
  dplyr::filter(!is.na(Latitude))%>%
  glimpse()

# Create Anonymous fisher names ----
fishers<-dat%>%
  distinct(Fisher)%>%
  filter(!is.na(Fisher))%>%
  mutate(Number=1:nrow(.))%>%
  mutate(Letter=LETTERS[Number])%>%
  mutate(Anonymous.fisher=paste("Fisher",Letter,sep=" "))%>%
  glimpse()

# Join anonymous names back to dat ----
dat<-left_join(dat,fishers)%>%
  dplyr::select(-c(Number,Fisher,Letter))%>%
  glimpse()

# Create a releases and a recaptures dataframe ----
dat.recaps<-dat%>%
  dplyr::filter(Recapture%in%c("TRUE"))%>%
  dplyr::select(-c(Source,Sample,Recapture,Colour,Outlier,Location))%>%
  glimpse()

# Fisher recaptures to join back in names ----
dat.fishers<-dat.recaps%>%
  distinct(Date,Tag.number,Anonymous.fisher)%>%
  dplyr::rename(Recapture.date=Date)%>%
  replace_na(list(Anonymous.fisher="UWA"))%>%
  dplyr::filter(!is.na(Anonymous.fisher))%>%
  glimpse()

dat.releases<-dat%>%
  dplyr::filter(!Recapture%in%c("TRUE"))

dat.releases.caught.again<-semi_join(dat.releases,dat.recaps,by="Tag.number")%>%
  dplyr::select(-c(Source,Sample,Sex,Colour,Outlier,Location,Recapture,Carapace.length,Anonymous.fisher))%>%
  dplyr::filter(!is.na(Tag.number))

# Create data for map ----
# Good tag to check 191410 (to see if everything has worked)
dat.map.recaptures<-dat.releases.caught.again%>%
  dplyr::rename(Release.latitude=Latitude,Release.longitude=Longitude,Release.date=Date)%>% #
  left_join(dat.recaps)%>%
  bind_rows(dat.releases.caught.again)%>%
  arrange(Tag.number,Date)%>%
  dplyr::group_by(Tag.number)%>%
  dplyr::mutate(Release.latitude=lag(Latitude))%>%
  dplyr::mutate(Release.longitude=lag(Longitude))%>%
  dplyr::mutate(Release.date=lag(Date))%>%
  dplyr::filter(!is.na(Sex))%>%
  dplyr::rename(Recapture.latitude=Latitude,Recapture.longitude=Longitude,Recapture.date=Date)%>%
  dplyr::mutate(Days.between.captures = (as.Date(as.character(Recapture.date), format="%Y-%m-%d")) - (as.Date(as.character(Release.date))))%>%
  dplyr::mutate(Labels=paste("Tag ",Tag.number,". At liberty for ",Days.between.captures," days before being caught again on ",Recapture.date," (",Carapace.length," mm).",sep=""))%>%
  dplyr::ungroup()%>%
  dplyr::select(-c(Anonymous.fisher))%>%
  left_join(.,dat.fishers)%>%
  glimpse()

# Need dots for initial releases
dat.map.releases<-semi_join(dat.releases,dat.recaps,by="Tag.number")%>%
  dplyr::select(-c(Source,Sample,Colour,Outlier,Location,Recapture))%>%
  dplyr::rename(Release.latitude=Latitude,Release.longitude=Longitude,Release.date=Date)%>%
  dplyr::mutate(Labels=paste("Tag ",Tag.number,". A ",Sex," lobster, first caught on  ",Release.date," (",Carapace.length," mm).",sep=""))%>%
  dplyr::filter(!is.na(Tag.number))%>%
  glimpse()

# Have pots with multiple dots on the same exact position ----
# So need to combine labels together, but also have releases and recaptures on the same point

# 1. make new data frame for both recaptures and releases with same lat and lon column
# 2. figure out double ups
# 3. paste together labels
# 4. Would be nice to have number caught at that point

glimpse(dat.map.releases)
glimpse(dat.map.recaptures)

dat.dots.recap<-dat.map.recaptures%>%
  dplyr::rename(Latitude=Recapture.latitude,Longitude=Recapture.longitude)%>%
  dplyr::select(Tag.number,Sex,Latitude,Longitude,Labels,Carapace.length,Anonymous.fisher)

dat.dots.release<-dat.map.releases%>%
  dplyr::rename(Latitude=Release.latitude,Longitude=Release.longitude)%>%
  dplyr::select(Tag.number,Sex,Latitude,Longitude,Labels)

dat.dots<-bind_rows(dat.dots.recap,dat.dots.release)%>%
  dplyr::group_by(Latitude,Longitude)%>%
  dplyr::summarise(Labels=paste(strwrap(Labels), collapse = " <br> <br> "))%>%
  ungroup()

dat.dots.map<-bind_rows(dat.dots.recap,dat.dots.release)%>%
  select(-c(Labels))%>%
  left_join(.,dat.dots)%>%
  replace_na(list(Anonymous.fisher="UWA"))


library(RColorBrewer)

n <- 26
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))

pal <- colorFactor(sample(col_vector, n),domain = dat.dots.map$Anonymous.fisher)



# http://tools.medialab.sciences-po.fr/iwanthue/
# pal <- colorFactor(c("#d04756",
#                      "#c88b88",
#                      "#7d342c",
#                      "#d64527",
#                      "#372c27",
#                      "#d17c3f",
#                      "#d8ae45",
#                      "#7d6833",
#                      "#cbcc9e",
#                      "#cfe14d",
#                      "#8ba23c",
#                      "#6fd958",
#                      "#488d42",
#                      "#436547",
#                      "#76d8a8",
#                      "#77c0ca",
#                      "#5a6d87",
#                      "#6881d1",
#                      "#6c46cf",
#                      "#36254e",
#                      "#c4b4d4",
#                      "#5a328b",
#                      "#c882c9",
#                      "#cc49c7",
#                      "#823765",
#                      "#d84789"),domain=dat.dots.map$Anonymous.fisher)

map.all.leaflet <- leaflet(dat.dots.map) %>% 
  fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude))%>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Black and white")%>%
  addTiles(group="Open street map")%>% 
  # All Lobsters
  addCircleMarkers(data=dat.dots.map,~Longitude,~Latitude,radius = 4,color = ~pal(Anonymous.fisher),stroke = FALSE, fillOpacity = 0.5,popup = ~as.character(Labels),group="All data")%>%
  
  addCircleMarkers(data=filter(dat.dots.map,Carapace.length<76),~Longitude,~Latitude,radius = 4,color = ~pal(Anonymous.fisher),stroke = FALSE, fillOpacity = 0.5,popup = ~as.character(Labels),group="Sub-legal")%>% # Sub legal
  
  addCircleMarkers(data=filter(dat.dots.map,Carapace.length>=76),~Longitude,~Latitude,radius = 4,color = ~pal(Anonymous.fisher),stroke = FALSE, fillOpacity = 0.5,popup = ~as.character(Labels),group="Legal")%>% # Legal
  addScaleBar()%>%
  addLegend("bottomright", pal = pal, values = ~Anonymous.fisher,title = "Fisher",opacity = 1)%>%
addLayersControl(
  baseGroups = c("Open street map","Black and white"),
  overlayGroups = c("All data","Legal","Sub-legal"),
  options = layersControlOptions(collapsed = FALSE)
)

map.all.leaflet  

# Combine recaptures and releases together to make a dataframe that has all lines ----
dat.map.all<-bind_rows(dat.map.releases,dat.map.recaptures)%>%
  arrange(Tag.number,Release.date)%>%
  dplyr::mutate(Release.latitude=ifelse(((!Release.latitude==Recapture.latitude)&(!is.na(Recapture.latitude))),Recapture.latitude,Release.latitude))%>% # So if release and recapture are both not na and are different then release = recapture this is two get a line when only recaptured once
  dplyr::mutate(Release.longitude=ifelse(((!Release.longitude==Recapture.longitude)&(!is.na(Recapture.longitude))),Recapture.longitude,Release.longitude))%>%
  mutate(num=1:nrow(.))%>%
  mutate(Progress=round((num/nrow(.))*100,digits=2))%>%
  glimpse()

# Loop through tags to add to map ----
uniq <- unique(unlist(dat.map.all$Tag.number))
uniq

pal <- colorFactor(c( "hotpink","blue"), domain = c("Male", "Female"))

for (i in 1:length(uniq)){
  
  temp.dat <- subset(dat.map.all, Tag.number == uniq[i])
  print(paste("Plotting ",unique(temp.dat$Tag.number),", ",unique(temp.dat$Progress),"% finished",sep=""))
  
  map.all.leaflet <- map.all.leaflet%>%
    addPolylines(data=filter(temp.dat,Carapace.length<76), ~Release.longitude,~Release.latitude,weight = 0.25,fillOpacity = 0.75,color = ~pal(Sex),group="Sub-legal")%>%
    addPolylines(data=filter(temp.dat,Carapace.length>=76), ~Release.longitude,~Release.latitude,weight = 0.25,fillOpacity = 0.75,color = ~pal(Sex),group="Legal")%>%
    
    addPolylines(data=temp.dat, ~Release.longitude,~Release.latitude,weight = 0.25,fillOpacity = 0.75,color = ~pal(Sex),group="All data")
}

map.all.leaflet<-map.all.leaflet%>%
  addLegend("bottomleft", pal = pal, values = ~Sex,title = "Sex",opacity = 1)

map.all.leaflet

