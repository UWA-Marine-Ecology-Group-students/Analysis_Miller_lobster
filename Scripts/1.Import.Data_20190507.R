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
work.dir=("C:/GitHub") # For Brooke
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
dat.2018<-gs_title("Lobsters_data_2018_All")%>% 
  gs_read_csv(ws = "Lobster.var",col_types = "nccncccccccccccccnnnncccccc")%>%
  mutate(trip.day.trap=paste(Trip,Day,Trap.ID,sep="."))%>%  #Trap.ID
  mutate(Source = 'ash-millers-masters')%>%
  glimpse()

dat.length.2018<-dat.2018%>%
  mutate(Count=1)%>% # Create count for abundance calcs
  # I think we need to keep NA's in Carapace.length and tag.number BG 07/05/19
  filter(!(is.na(Carapace.length)&is.na(Tag.number)&is.na(Colour)))%>% # now filters out where all are blank, 35 individuals have colour but no other details
  mutate(Carapace.length=as.numeric(as.character(Carapace.length)))%>%
  mutate(Colour=str_replace_all(.$Colour,c("W"="White", "R"="Red")))%>%
  mutate(Sex=str_replace_all(.$Sex, c("M"="Male", "F"="Female","U"="Unknown")))%>%
  mutate(Sex=if_else((!is.na(Colour)&!Sex%in%c("Female","Male")),"Unknown",Sex))%>%
  # Fix Damaged data
  mutate(Damage.old.a=if_else(Damage.new.a==9,0,Damage.old.a))%>%
  mutate(Damage.new.L=if_else(Damage.new.a==9,0,Damage.new.L))%>%
  mutate(Damage.old.L=if_else(Damage.new.a==9,0,Damage.old.L))%>%
  mutate(Damage.new.a=if_else(Damage.new.a==9,0,Damage.new.a))%>%
  replace_na(list(Damage.old.a = 0, Damage.old.L = 0,Damage.new.a = 0, Damage.new.L = 0,Sex="Unknown",Colour="Unknown"))%>%
  mutate(Total.damage=(Damage.old.a+Damage.old.L+Damage.new.a+Damage.new.L))%>%
  dplyr::select(-c(Date,Trap.Type,Day.Pull,Pot.Remarks,PWO,WKW,PWF,Source))%>%
  glimpse()

# Check formating of Lobster data
unique(dat.length.2018$Recapture) #  NA TRUE
unique(dat.length.2018$Sex) # "Female" "Male"   "Unknown"
unique(dat.length.2018$Colour) # "Red"   "White" "Unknown"  
unique(dat.length.2018$Outlier) #  NA  "y"
unique(dat.length.2018$Setose.state) # "I" NA  "E" "M"
unique(dat.length.2018$Dead) # NA     "Dead"
unique(dat.length.2018$Individual.Remarks)

# Damage data
unique(dat.length.2018$Damage.new.a) # 0 1 2 - I have fixed when more than 2 antennas damaged
unique(dat.length.2018$Total.damage)%>%sort()

# Counts
length(unique(dat.length.2018$Tag.number)) # 7537 tagged individuals
length(dat.length.2018$Carapace.length) # 9318 total indiviuals caught
length(unique(dat.length.2018$trip.day.trap)) # 1163 pots

# Things that should actually be in the metadata not the lobster data
additional.info<-dat.2018%>%
  dplyr::select(Trip,Location,Trap.ID,Day,Date,Trap.Number,Trap.Type,Day.Pull,Pot.Remarks,PWO,WKW,PWF,trip.day.trap,Source)%>%
  distinct()

length(unique(additional.info$trip.day.trap)) # 1440 

duplicate.remarks<-additional.info%>%
  group_by(trip.day.trap)%>%
  summarise(n=n())%>%
  filter(n>1)

unique(additional.info$Pot.Remarks)

# Import 2018 pot data----
dat.pot.2018<-gs_title("Lobsters_data_2018_All")%>% # To use GoogleSheets
  gs_read_csv(ws = "Pot.var",col_types = "nccnccnnccnnnc")%>%
  mutate(trip.day.trap=paste(Trip,Day,Trap.ID,sep="."))%>% 
  mutate(Site=str_replace_all(.$Site.Name,c( "SM"="Seven Mile", "DM"="Davids Marks",  "RM"="Rivermouth", "IR"="Irwin Reef", "LR"="Long Reef", "SD"="South Dummy", "LH"="Little Horseshoe", "CHin1_"="Cliff Head Mid","CHin2_"="Cliff Head South","CHout1_" = "Cliff Head OUT1","CHout2_" = "Cliff Head North", "JB"="Jim Bailey", "GR"="Golden Ridge", "SR"="South Rig", "WL"="Whites Lump")))%>% 
  mutate(Location=str_replace_all(.$Site,c("Seven Mile Beach"= "Seven Mile","Little Horseshoe"="Cliff Head", "Cliff Head North"="Cliff Head","Cliff Head Mid"= "Cliff Head","Cliff Head South"="Cliff Head","Cliff Head OUT1"= "Cliff Head","CHM"="Cliff Head", "Davids Marks"="Cliff Head","CHM"= "Cliff Head", "CHS"="Cliff Head", "CHN"="Cliff Head", "Jim Bailey"="Irwin Reef", "Long Reef"="Irwin Reef", "South Dummy"="Irwin Reef","South Rig"= "Irwin Reef","Whites Lump"= "Irwin Reef","WP"= "Irwin Reef","Whitepoint"="Irwin Reef")))%>% 
  mutate(Site=str_replace_all(.$Site, c("Jim Bailey"="White Point" ,"WP"="White Point" , "Whitepoint"="White Point" , "CHS"="Cliff Head South","CHM"="Cliff Head Mid","CHN"="Cliff Head North", "Seven Mile Beach"= "Seven Mile.out")))%>%
  filter(Johns=="No")%>% # turn off when I add in john's data (if ever)
  dplyr::rename(Latitude=Latitude.y, Longitude=Longitude.x)%>%
  mutate(Latitude=as.numeric(Latitude))%>%
  mutate(Longitude=as.numeric(Longitude))%>%
  filter(!Comment%in%c("Duplicate"))%>%
  select(-c(Johns,Comment))%>%
  dplyr::rename(Site.Code=Site.Name)%>%
  glimpse()

# Locations
  # Seven Mile Beach
  # Cliff Head
  # Irwin Reef


unique(dat.pot.2018$Location)%>%sort() # "Cliff Head"   "Golden Ridge" "Irwin Reef"   "Rivermouth"   "Seven Mile"  
unique(dat.pot.2018$Site)%>%sort() # Would be nice to rename "Cliff Head OUT1"
unique(dat.pot.2018$Site.Code)%>%sort()

length(unique(dat.pot.2018$trip.day.trap)) # 1461

# Two problems
# 1. duplicates in trip.day.trap in dat.pot - fixed
# 2. does not match additional info number of samples

missing.from.lobster<-anti_join(additional.info,dat.pot.2018, by = c("Trip",  "Trap.ID", "Day", "trip.day.trap")) #"Location",
# Location is what causes the errors

missing.field.info<-anti_join(dat.pot.2018,additional.info, by = c("Trip", "Trap.ID", "Day", "trip.day.trap"))
# 21 pots that aren't in the lobster data

## Pot numbers
# 1461 pots in total
# 1163 non-empty pots
# 21 are empty with out addtional info
# 277 are empty with additional info
277+21
# 298 empty pots in total

21+277+1163 # correct number

duplicate.pots<-dat.pot.2018%>%
  group_by(trip.day.trap)%>%
  dplyr::summarise(n=n())%>%filter(n>1) # Fixed
 
# Create "sites" for 2018 Data ----
# Not sure we need this????
sites.2018<-dat.pot.2018%>%
  distinct(Trap.ID,Location,Site)%>% #Keeps only distinct rows (Trap.ID & Site.Name)
  mutate(Trap.ID=as.character(Trap.ID))%>%
  glimpse()

# Import 2017 length data----
dat.2017<-gs_title("Lobsters_data_20180214")%>% # To use GoogleSheets
  gs_read_csv(ws = "Lobster.var",col_types = "nncncccccccnnnn")%>%
  mutate(Source='oscar-doncel-canons-masters')%>%
  glimpse()

unique(dat.2017$Carapace.length) 
unique(dat.2017$Sex) # "Female"  "Male"    "UNKNOWN"
unique(dat.2017$Colour) # "Red"     "White"   "UNKNOWN"
unique(dat.2017$Setose.state) # NA       "SETOSE"
unique(dat.2017$Egg.stage) # NA  "2"
unique(dat.2017$Tagged) # "NO"           "YES"          "EXISTING.OLD" "EXISTING"  
unique(dat.2017$Moult.stage) # NA

dat.length.2017<-dat.2017%>%
  mutate(Count=1)%>%        # Count for Abundance
  mutate(Colour=str_replace_all(.$Colour,c("UNKNOWN"="Unknown")))%>%
  mutate(Sex=if_else((!is.na(Colour)&!Sex%in%c("Female","Male")),"Unknown",Sex))%>%
  mutate(Carapace.length=as.numeric(as.character(Carapace.length)))%>%
  mutate(day.trap=paste(Day,Trap.number,sep="."))%>%  #New column for Day and trap.number
  #mutate(Recapture=NA)%>% # some of Oscar's are recaptures from the same trip and some are old tags
  mutate(Recapture=if_else(Tagged%in%c("EXISTING","EXISTING.OLD"),TRUE,NA))%>%
  mutate(Cable.Tie=if_else(Tag.number%in%c("CT"),TRUE,NA))%>% # Move cable tie data into a different column
  mutate(Tag.number=ifelse(Tag.number%in%c("CT"),NA,as.character(Tag.number)))%>%
  mutate(Trip=0)%>%
  select(-c(Tagged))%>%
  #select(Trip, Day, Trap.number, Carapace.length, Sex, Colour, Tag.number,Recapture, day.trap, Source, Fisher)%>%
  glimpse()

names(dat.length.2017)

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
