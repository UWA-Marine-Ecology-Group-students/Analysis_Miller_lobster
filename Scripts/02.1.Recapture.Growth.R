#Edited Recappture data for growth analysis (Fabens and Kmeans)
#Need to remove outliers, location change and inconsistent measurements (Bruce Cockmans) that don't need to be removed for the other recapture analysis (Movement).

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
library(lubridate)

# Study name ----
study<-"Growth.Rate"

# Set work directory ----
work.dir=("C:/GitHub/Analysis_Miller_lobster") # For Brooke
#work.dir=("Z:/Analysis_Miller_lobster") # FOr Ash's laptop using Git

# Set sub-directories ----
data.dir=paste(work.dir,"Data",sep="/")
plot.dir=paste(work.dir,"Plots",sep="/")

# Import data ----
setwd(data.dir)
dir()

#Pot data
metadata<-read.csv("metadata.csv")%>%
  filter(!Fisher%in%c("Bruce Cockman"))%>% # Filter out Bruce Cockmans data
  glimpse()

#Length data
length<-read.csv("length.csv")%>%
  filter(is.na(Total.damage)|!Total.damage>2)%>% #removes 6
  #filter out 33 individuals in total that have moved Location (not neccarily site)
  dplyr::filter(!Tag.number %in% c("K1054", "K1565", "K1604", "191162", "K2485", "K2446", "195768", "195655", "K2934", "195997", "195997", "195256", "195958", "195985", "196281", "K4249","K4248", "K4955", "197205", "198251"))%>%
  filter(!Tag.number%in%c("K2400","K1617","K1221","K0653","K0457","198428","196072","K4501","198821","K2519","K2402","K3805","K1019", "K1523"))%>% # Filter out outliers
  glimpse()

names(metadata)
names(length)

# Filter out issues in metadata ----
metadata.clean<-metadata%>%
  filter(!Exclude.pots%in%c("Yes"))%>% # Remove bad pots. AM changed to "yes" from no?
  dplyr::filter(!Location%in%c("Rivermouth"))%>% # Filter out Rivermouth
  select(-c(Repeated.samples,Exclude.pots,Depth.lidar,Depth.fms,Fisher.email,Site.code))%>%
  glimpse()

# Filter out issues in length data ---
length.clean<-length%>%
  filter(!is.na(Tag.number))%>% # Filter out individuals without Tag
  filter(!is.na(Carapace.length))%>% #Filter out individuals with no length measurement
  semi_join(.,metadata.clean)%>% # Keep only those that have metadata
  select(-c(Outlier,Dead))%>%
  glimpse()

# Join datasets together ----
dat<-left_join(length.clean,metadata.clean)%>%
  select(Source,Sample, Trip, Date,Location,Site,Latitude, Longitude, Tag.number, Carapace.length, Sex, Colour,Total.damage)%>%
  dplyr::mutate(Date=lubridate::as_date(ymd(Date)))%>%
  glimpse()

#Add in 2019 data----

# #Bring 2019 pot data
potdata.2019<-gs_title("Lobsters_data_2019_All")%>%
  gs_read_csv(ws = "Pot.var")%>%
  mutate(Source = 'Dongara.Millers.Masters')%>%
  mutate(Sample=paste(Trip,Day,Trap.ID,sep="."))%>%
  dplyr::rename(Latitude=Latitude.y, Longitude=Longitude.x,Date=Date.Recovered)%>%
  mutate(Site=str_replace_all(.$Site.Name,c("IR"="Irwin Reef","JB"= "Jim Bailey", "GR"="Golden Ridge", "WL"="Whites Lump", "CHS"="Cliff Head South", "CHM"= "Cliff Head Mid", "CHN"="Cliff Head North", "CHout2_"="Cliff Head North", "LHS"= "Little Horseshoe", "SR"="South Rig", "WP"="White Point" )))%>%
  mutate(Location=str_replace_all(.$Site,c("Cliff Head North"="Cliff Head","Cliff Head Mid"= "Cliff Head","Cliff Head South"="Cliff Head","Cliff Head OUT1"= "Cliff Head","CHM"="Cliff Head", "Davids Marks"="Cliff Head","CHM"= "Cliff Head", "CHS"="Cliff Head", "CHN"="Cliff Head", "Jim Bailey"="White Point", "Long Reef"="Irwin Reef", "South Rig"= "White Point","Whites Lump"= "White Point","WP"= "White Point","Whitepoint"="White Point")))%>% 
  select(Source,Sample,Day,Pot.Number,Location,Site,Latitude,Longitude,Pot.Type, PositionFormat)%>% # Trap.ID,
  glimpse()

# Change format of Longitude and Latitude
dm <- potdata.2019%>%
  filter(PositionFormat%in%c("Decimal Minutes"))%>%
  dplyr::mutate(Latitude=measurements::conv_unit(.$Latitude, from = 'deg_dec_min', to = 'dec_deg'))%>%
  dplyr::mutate(Longitude=measurements::conv_unit(.$Longitude, from = 'deg_dec_min', to = 'dec_deg'))%>%
  dplyr::mutate(Latitude=as.numeric(Latitude))%>%
  dplyr::mutate(Longitude=as.numeric(Longitude))%>%
  glimpse()

#filter decimla degrees
dd <- potdata.2019%>%
  filter(!PositionFormat%in%c("Decimal Minutes"))%>%
  dplyr::mutate(Latitude=as.numeric(Latitude))%>%
  dplyr::mutate(Longitude=as.numeric(Longitude))%>%
  glimpse()

#combine together
potdata.2019 <- rbind(dd, dm)%>%
  glimpse()


#Bring in lobster data 2019
lengthdata.2019<-gs_title("Lobsters_data_2019_All")%>%
  gs_read_csv(ws = "Lobster.var")%>%
  mutate(Sample=paste(Trip,Day,Trap.ID,sep="."))%>%
  mutate(Total.damage=0)%>% #CHANGE LATER
  dplyr::mutate(Date=as_date(dmy(Date)))%>%
  filter(!is.na(Tag.number))%>% # Filter out individuals without Tag
  filter(!is.na(Carapace.length))%>% #Filter out individuals with no length measurement
  mutate(Colour=str_replace_all(.$Colour,c("W"="White", "R"="Red")))%>%
  mutate(Sex=str_replace_all(.$Sex, c("M"="Male", "F"="Female","U"="Unknown")))%>%
  mutate(Sex=if_else((!is.na(Colour)&!Sex%in%c("Female","Male")),"Unknown",Sex))%>%
  select(Sample, Trip, Date, Tag.number, Carapace.length, Sex, Colour, Total.damage)%>%
  glimpse()
 
#Combine pot and lobster data 2019
dat.2019<- left_join(lengthdata.2019,potdata.2019, by="Sample")%>%
  select(Source, Sample, Trip, Date, Location, Site, Latitude, Longitude, Tag.number, Carapace.length, Sex, Colour, Total.damage)%>%
  glimpse()
 
 glimpse(dat)
 glimpse(dat.2019)
 
 #Combine All data (2017, 2018, 2019)----
 dat.all <-rbind(dat, dat.2019)%>%
   glimpse()
 

#Get a list of recaptured tags----
recaps<- dat.all%>%
  select(Tag.number)%>%
  mutate(duplicates = duplicated(.) | duplicated(., fromLast = TRUE))%>%
  filter(duplicates=="TRUE")%>%
  glimpse()

#Join with other data----
all.recaps <- semi_join(dat.all, recaps)%>%
  glimpse()

#Find initial tag----
#Order by earliest date to latest
dat.order<- all.recaps%>%
  arrange(Date)%>% #Have to the arrange as a separate pipe for some reason
  glimpse()

just.initial<-dat.order%>%
  dplyr::distinct(Tag.number,.keep_all = TRUE)%>% 
  #filters out the duplicates (recaptues)
  select(Date, Location, Site,Tag.number, Carapace.length, Sex, Colour,Total.damage, Longitude, Latitude)%>% #Simplify
  glimpse()

# Find Only Recaptures (not intial tags)----
just.recaps <- dat.order[duplicated(dat.order$Tag.number), ]%>%
  select(Date, Location, Site,Tag.number, Carapace.length, Sex, Colour,Total.damage, Longitude, Latitude)%>% 
  dplyr::rename(Date.recap=Date, 
                Location.recap=Location, 
                Site.recap=Site, 
                Carapace.length.recap=Carapace.length,
                Sex.recap=Sex, 
                Colour.recap=Colour,
                Total.damage.recap=Total.damage, 
                Longitude.recap=Longitude,
                Latitude.recap=Latitude)%>%
    glimpse()

#Bind to make a long dataframe of all recaptured----
total.recaptures<-rbind(just.initial, just.recaps)%>%
  arrange(Tag.number)%>%
  glimpse()


#Combine Initial and Recaptures----
dat.rr<- dplyr::left_join(just.initial, just.recaps, by="Tag.number")%>%
  mutate(inc=Carapace.length.recap-Carapace.length)%>% #growth increment
  dplyr::mutate(Ldys=Date.recap-Date)%>% #Days between capture
  glimpse()

#Filter 0 = Ldys, weird number of repeats in Hebbos----
dat.rr%<>%
  filter(!Ldys=="0")%>%
  glimpse()
#119-mostly hebbos and a few of oscars

#Save for k-means and fabens----
setwd(data.dir)
write.csv(dat.rr, "Growth.Data.csv", row.names = F)
