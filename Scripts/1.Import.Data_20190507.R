# Explore catch data----
rm(list=ls()) # Clears memory

# librarys----
library(tidyr)
library(dplyr)
library(googlesheets)
library(stringr)
library(measurements)
library(lubridate)
library(readr)

# Study name----
study<-"Lobster.Data"

# Set work directory----
# work.dir = ("~/Google Drive/Analysis_WRL_Reds_2018/Data") # for Laptop
# work.dir=("~/GitHub/Analysis_Miller_WRL") #for Tim's github
# work.dir=("~/workspace/Analysis_Miller_WRL") #for ecocloud server
work.dir=("Z:/Analysis_Miller_lobster") # FOr Ash's laptop using Git

# Sub directories ----
data.dir<-paste(work.dir,"Data",sep="/")
map.dir<-paste(work.dir,"Map Layers",sep="/")
plots.dir<-paste(work.dir,"Plots",sep="/")

# Import and make data----

# For Rstudio Server
# options(httr_oob_default=TRUE)
# options(httr_oob_default=FALSE) #for desktop
# gs_auth(new_user = TRUE) #only run once

# Import 2018 length data----
dat.length.2018<-gs_title("Lobsters_data_2018_All")%>% 
  gs_read_csv(ws = "Lobster.var")%>%
  mutate(Count=1)%>% # Create count for abundance calcs
  # I think we need to keep NA's in Carapace.length and tag.number BG 07/05/19
  filter(!(is.na(Carapace.length)&is.na(Tag.number)&is.na(Colour)))%>% # now filters out where all are blank, 35 individuals have colour but no other details
  mutate(Carapace.length=as.numeric(as.character(Carapace.length)))%>%
  mutate(trip.day.trap=paste(Trip,Day,Trap.ID,sep="."))%>%  #Trap.ID
  mutate(Colour=str_replace_all(.$Colour,c("W"="White", "R"="Red")))%>%
  mutate(Sex=str_replace_all(.$Sex, c("M"="Male", "F"="Female")))%>%
  mutate(Source = 'Ash')%>%
  mutate(Fisher = NA)%>%
  mutate(ID=1:nrow(.))%>%
  # group_by(ID)%>%
  replace_na(list(Damage.old.a = 0, Damage.old.L = 0,Damage.new.a = 0, Damage.new.L = 0))%>%
  mutate(Total.damage=sum(Damage.old.a+Damage.old.L+Damage.new.a+Damage.new.L,na.rm = TRUE))%>%
  # select(Trip, Trap.ID, Day, Date, Tag.number, Recapture, Trap.Number, Carapace.length, Sex, Colour, trip.day.trap, Source, Fisher,Total.damage)%>%
  glimpse()

unique(dat.length.2018$Setose.state)
length(unique(dat.length.2018$Tag.number)) # 7537
length(dat.length.2018$Carapace.length) # 9614

# Import 2018 pot data----
dat.pot.2018<-gs_title("Lobsters_data_2018_All")%>% # To use GoogleSheets
  gs_read_csv(ws = "Pot.var")%>%
  mutate(trip.day.trap=paste(Trip,Day,Trap.ID,sep="."))%>% 
  mutate(Site.Name=str_replace_all(.$Site.Name,c( "SM"="Seven Mile", "DM"="Davids Marks",  "RM"="Rivermouth", "IR"="Irwin Reef", "LR"="Long Reef", "SD"="South Dummy", "LH"="Little Horseshoe", "CHin1_"="Cliff Head Mid","CHin2_"="Cliff Head South","CHout1_" = "Cliff Head OUT1","CHout2_" = "Cliff Head North", "JB"="Jim Bailey", "GR"="Golden Ridge", "SR"="South Rig", "WL"="Whites Lump")))%>% 
  filter(Johns=="No")%>% # turn off when I add in john's data (if ever)
  dplyr::rename(Latitude=Latitude.y, Longitude=Longitude.x)%>%
  mutate(Latitude=as.numeric(Latitude))%>%
  mutate(Longitude=as.numeric(Longitude))%>%
  #select(Trip, Site.Name, Pot.Number, Trap.ID, Day, Date.Recovered, Longitude, Latitude, trip.day.trap)%>%
  glimpse()

#Create "sites" for 2018 Data----
sites.2018<-dat.pot.2018%>%
  distinct(Trap.ID,Site.Name)%>% #Keeps only distinct rows (Trap.ID & Site.Name)
  mutate(Trap.ID=as.character(Trap.ID))%>%
  dplyr::rename(Site=Site.Name)%>%
  glimpse()

#Add a "Site" column by Trap.ID to dat.length----
dat.length.2018<-left_join(dat.length.2018,sites, by="Trap.ID") 

#Check for missing sites: ones in dat.length but not in dat.pot
#missing.site.1<-anti_join(dat.length.1,sites)  # 0

#Join Lobster and pot 2018 data----
dat.2018<- dplyr::left_join(dat.length.2018, dat.pot.2018)%>%
  select(Trip, Trap.ID, Day, Date, Tag.number, Recapture, Trap.Number, Carapace.length, Sex, Colour, Site, Longitude, Latitude, Source, Fisher, Total.damage)%>%
  glimpse()

#Check data that's in dat.length.all but not in dat.pot
# missing.dat.1 <-anti_join(dat.length.1, dat.pot.1)  #0

glimpse(dat.1)
#Import 2017 length data----

dat.length.2017<-gs_title("Lobsters_data_20180214")%>% # To use GoogleSheets
  gs_read_csv(ws = "Lobster.var" )%>%
  mutate(Count=1)%>%        #Count for Abundance
  #filter(!is.na(Carapace.length))%>% #Turn on if you want to filter out NA CL
  filter(!is.na(Tag.number))%>% #A lot not tagged, filtered out
  mutate(Carapace.length=as.numeric(as.character(Carapace.length)))%>%
  mutate(day.trap=paste(Day,Trap.number,sep="."))%>%  #New column for Day and trap.number
  mutate(Recapture=NA)%>%
  mutate(Trip=0)%>%
  mutate(Source='Oscar')%>%
  mutate(Fisher='')%>%
  select(Trip, Day, Trap.number, Carapace.length, Sex, Colour, Tag.number,Recapture, day.trap, Source, Fisher)%>%
  glimpse()

#Import 2017 pot data----
dat.pot.2017<-gs_title("Lobsters_data_20180214")%>% # To use GoogleSheets
  gs_read_csv(ws = "Pot.var")%>%
  mutate(day.trap=paste(Day,Trap.number,sep="."))%>% 
  mutate(Trip=0)%>%
  select(Trip,Day, Date, Trap.number, Location, Number,Longitude.original, Latitude.original, day.trap)%>%
  glimpse()

#Create "sites" for 2017 Data----
sites.2017<-dat.pot.2017%>%
  distinct(Trap.number,Location)%>% #Keeps only distinct rows (Trap.ID & Site.Name)
  dplyr::rename(Site=Location)%>%
  glimpse()

#Add a "Site" column by Trap.ID to dat.length----
dat.length.2017<-left_join(dat.length.2017,sites.2017, by="Trap.number") 

#Check for missing sites: ones in dat.length but not in dat.pot
# missing.site.2<-anti_join(dat.length.2,sites.2)  # 0

#Join 2017 Lobster and pot data----

dat.2017 <- dplyr::left_join(dat.length.2017, dat.pot.2017)%>%
  dplyr::rename(Longitude=Longitude.original, Latitude=Latitude.original, Trap.ID=Trap.number, Trap.Number=Number)%>% 
  mutate(Latitude=as.numeric(Latitude))%>%
  mutate(Longitude=as.numeric(Longitude))%>%
  select(Trip, Day, Trap.ID, Carapace.length, Sex, Colour, Tag.number, Recapture,Site, Date, Trap.Number, Longitude, Latitude, Source, Fisher)%>%
  glimpse()

length(unique(dat.2017$Tag.number))
#1683

#Check data that's in dat.length.all but not in dat.pot
# missing.dat.2 <-anti_join(dat.length.2, dat.pot.2) #0!

#Combine 2017 and 2018 data ----

dat.all <- bind_rows(dat.2018, dat.2017)%>%
  dplyr::mutate(Date=as_date(dmy(Date)))%>%
  glimpse()

# Write data
setwd(data.dir)
write.csv(dat.all, "dat.all.17-18.csv")

#Import Tag Return data sent to Fisheries----

dat.returns.1 <- gs_title("Fisheries.Tag.Returns")%>%
  gs_read_csv(ws="Sheet1")%>%
  filter(!is.na(SpeciesTag))%>% 
  mutate(Recapture=TRUE)%>%
  mutate(Trip=9)%>%
  mutate(Colour=NA)%>%
  mutate(Source= "Fisheries")%>%
  mutate(Sex=str_replace_all(.$Sex, c("M"="Male", "F"="Female")))%>%
  dplyr::rename(Tag.number=SpeciesTag, Carapace.length=SpeciesSize, Date=ReportCreatedDate, Fisher=ReportersUserName)%>%
  dplyr::mutate(Date=as_date(mdy(Date)))%>%
  select(Trip, Colour, Date, Tag.number, Sex, Carapace.length, Recapture, Latitude, Longitude, Source, Fisher, PositionFormat)%>%
  glimpse()


length(dat.returns.1$Tag.number)

#Change format of Longitude and Latitude

dd <- dat.returns.1%>%
  filter(PositionFormat%in%c("Decimal Minutes"))%>%
  mutate(Latitude=measurements::conv_unit(.$Latitude, from = 'deg_dec_min', to = 'dec_deg'))%>%
  mutate(Longitude=measurements::conv_unit(.$Longitude, from = 'deg_dec_min', to = 'dec_deg'))%>%
  glimpse()

dd1 <- dat.returns.1%>%
  filter(PositionFormat%in%c("Decimal Degrees"))%>%
  glimpse()


dat.returns.1 <- bind_rows(dd, dd1)%>%
  select(Trip, Colour, Date, Tag.number, Sex, Carapace.length, Recapture, Latitude, Longitude, Source, Fisher)%>%
  glimpse()

dat.returns.1 <-dat.returns.1%>%
  mutate(Latitude=as.numeric(Latitude))%>%
  mutate(Longitude=as.numeric(Longitude))%>%
  glimpse()

#Import Tag Return data sent to UWA----

dat.returns.2 <- gs_title("UWA.Tag.Returns")%>%
  gs_read_csv(ws="Sheet1")%>%
  mutate(Recapture=TRUE)%>%
  mutate(Trip=9)%>%
  mutate(Source = "UWA")%>%
  mutate(Sex=str_replace_all(.$Sex, c("M"="Male", "F"="Female")))%>%
  mutate(Colour=str_replace_all(.$Colour, c("W"="White", "R"="Red")))%>%
  dplyr::rename(Fisher= Reporter, Latitude=`Latitude(deg_dec_min)`, Longitude=`Longitude(deg_dec_min)`)%>%
  dplyr::mutate(Date=as_date(dmy(Date)))%>%
  select(Trip, Date, Tag.number, Carapace.length,Recapture, Sex, Colour, Longitude, Latitude, Source, Fisher)%>%
  glimpse()



#Change format of Longitude and Latitude

dat.returns.2 <- dat.returns.2%>%
  mutate(Latitude=measurements::conv_unit(.$Latitude, from = 'deg_dec_min', to = 'dec_deg'))%>%
  mutate(Longitude=measurements::conv_unit(.$Longitude, from = 'deg_dec_min', to = 'dec_deg'))%>%
  mutate(Latitude=as.numeric(Latitude))%>%
  mutate(Longitude=as.numeric(Longitude))%>%
  glimpse()

#Combine data ----

Dat.Combined <- bind_rows(dat.all, dat.returns.1, dat.returns.2)%>%
  glimpse()


length(unique(Dat.Combined$Tag.number))

# Write data
setwd(data.dir)
write.csv(Dat.Combined, "Dat.Combined.csv")
