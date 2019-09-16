# Clean catch data----
rm(list=ls()) # Clears memory

# librarys----
library(googlesheets)
library(stringr)
library(measurements)
library(lubridate)
library(devtools)
install_github("UWAMEGFisheries/GlobalArchive", dependencies = TRUE) # to check for updates
library(GlobalArchive)
library(tidyverse)

# Set work directory----
# work.dir=("~/GitHub/Analysis_Miller_WRL") #for Tim's github
# work.dir=("~/workspace/Analysis_Miller_WRL") #for ecocloud server
work.dir=("C:/GitHub/Analysis_Miller_lobster") # For Brooke
# work.dir=("Z:/Analysis_Miller_lobster") # FOr Ash's laptop using Git

# Sub directories ----
data.dir<-paste(work.dir,"Data",sep="/")
uploads<-paste(data.dir,"Uploads",sep="/")

# Functions to remove csv unfriendly thingos
remove.commas <- function(c){
  ( gsub(",", '.', c))
}

remove.colon <- function(c){
  ( gsub(";", '.', c))
}

remove<-function(c){
  ( gsub(" ", " ", c))
}

#Import 2019 data----
# Two steps to keep additional info to join back into 
dat.2019<-gs_title("Lobsters_data_2019_All")%>%
  gs_read_csv(ws="Lobster.var", col_types="nccncccnnnncccccnnnncccccc")%>%
  mutate(Sample=paste(Trip, Day, Trap.ID, sep="."))%>%
  mutate(Source='Dongara.Millers.Masters')%>%
  dplyr::mutate(Date=lubridate::as_date(dmy(Date)))%>%
  glimpse()

length.2019<-dat.2019%>%
  filter(!(is.na(Carapace.length)&is.na(Tag.number)&is.na(Colour)))%>%
  mutate(Trip=ifelse(Trip%in%c(1),11,12))%>%
  mutate(Sample=paste(Trip,Day,Trap.ID,sep="."))%>% 
  # now filters out where all are blank (empty pots)
  mutate(Colour=str_replace_all(.$Colour,c("W"="White", "R"="Red")))%>%
  mutate(Sex=str_replace_all(.$Sex, c("M"="Male", "F"="Female","U"="Unknown")))%>%
  mutate(Sex=if_else((!is.na(Colour)&!Sex%in%c("Female","Male")),"Unknown",Sex))%>%
  mutate(Setose.state=ifelse(Setose.state%in%c("I"),NA,Setose.state))%>%
  mutate(Setose.state=ifelse(Setose.state%in%c("E"),"Immature",Setose.state))%>%
  mutate(Setose.state=ifelse(Setose.state%in%c("M"),"Mature",Setose.state))%>%
  mutate(Egg.stage=ifelse(Egg.stage%in%c("E","I"),"Early",Egg.stage))%>%
  mutate(Egg.stage=ifelse(Egg.stage%in%c("M"),"Mid",Egg.stage))%>%
  dplyr::select( Date,Source,Trip,Sample,Tag.number,Recapture,Carapace.length,Sex,Colour,Setose.state,Egg.stage,Moult.stage,Damage.old.a,Damage.old.L,Damage.new.a,Damage.new.L,Dead,Individual.Remarks)%>%
  
  glimpse()

#Get 2019 info
names(dat.2019)
info.2019<-dat.2019%>%
  dplyr::select(Trip,Sample, Day,Trap.ID, Trap.Number, Day.Pull, Pot.Remarks,PWO,PWF, WKW)%>% # Location, Date,
  distinct()%>%
  glimpse()

# Import 2019 metadata----
metadata.2019<-gs_title("Lobsters_data_2019_All")%>%# To use GoogleSheets
  gs_read_csv(ws = "Pot.var",col_types = "ncncnccncccc")%>% #
  mutate(Source = 'Dongara.Millers.Masters')%>%
  mutate(Trip=ifelse(Trip%in%c(1),11,12))%>%
  mutate(Sample=paste(Trip,Day,Trap.ID,sep="."))%>% 
  mutate(Site=str_replace_all(.$Site.Name,c( "SM"="Seven Mile", "DM"="Davids Marks",  "RM"="Rivermouth", "IR"="Irwin Reef", "LR"="Long Reef", "SD"="South Dummy", "LH"="Little Horseshoe", "CHin1_"="Cliff Head Mid","CHin2_"="Cliff Head South","CHout1_" = "Cliff Head OUT1","CHout2_" = "Cliff Head North", "JB"="Jim Bailey", "GR"="Golden Ridge", "SR"="South Rig", "WL"="Whites Lump")))%>% 
  mutate(Location=str_replace_all(.$Site,c("Seven Mile Beach"= "Seven Mile", "Cliff Head North"="Cliff Head","Cliff Head Mid"= "Cliff Head","Cliff Head South"="Cliff Head","Cliff Head OUT1"= "Cliff Head","CHM"="Cliff Head", "Davids Marks"="Cliff Head","CHM"= "Cliff Head", "CHS"="Cliff Head", "CHN"="Cliff Head", "Jim Bailey"="Irwin Reef", "Long Reef"="Irwin Reef", "South Dummy"="White Point","South Rig"= "White Point","Whites Lump"= "White Point","WP"= "White Point","Whitepoint"="White Point")))%>% 
  mutate(Site=str_replace_all(.$Site, c("Jim Bailey"="White Point" ,"WP"="White Point" , "Whitepoint"="White Point" , "CHS"="Cliff Head South","CHM"="Cliff Head Mid","CHN"="Cliff Head North", "Seven Mile Beach"= "Seven Mile South", "Cliff Head OUT1"="Cliff Head North", "Davids Marks"="Cliff Head North")))%>%
  dplyr::rename(Latitude=Latitude.y, Longitude=Longitude.x,Date=Date.Recovered)%>%
  #mutate(Latitude=as.numeric(Latitude))%>%
  #mutate(Longitude=as.numeric(Longitude))%>%
  filter(!Comment%in%c("Duplicate"))%>%
  select(-c(Comment,Creation.Date))%>%
  dplyr::rename(Site.Code=Site.Name)%>%
  mutate(Site.Code=str_replace_all(.$Site.Code,c("_"="")))%>% # Remove trailing underscores from site code
  left_join(.,info.2019)%>%
  mutate(Day.Pull=as.numeric(Day.Pull))%>%
  dplyr::mutate(Date=as_date(dmy(Date)))%>%
  select(Source,Sample,Trip,Day,Site.Code,Pot.Number,Location,Site,Date,Day.Pull,Latitude,Longitude,Pot.Type,Pot.Remarks,PWO,PWF,WKW, PositionFormat)%>% # Trap.ID,
  
  glimpse()

# Convert Brian's data GPS points to decimal degrees
dm <- metadata.2019%>%
  filter(PositionFormat%in%c("Decimal Minutes"))%>%
  dplyr::mutate(Latitude=measurements::conv_unit(.$Latitude, from = 'deg_dec_min', to = 'dec_deg'))%>%
  dplyr::mutate(Longitude=measurements::conv_unit(.$Longitude, from = 'deg_dec_min', to = 'dec_deg'))%>%
  glimpse()

dd <- metadata.2019%>%
  filter(is.na(PositionFormat))%>% #%in%c("Decimal Degrees"))%>%
  glimpse()

metadata.2019 <- bind_rows(dd, dm)%>%
  dplyr::mutate(Latitude=as.numeric(Latitude))%>%
  dplyr::mutate(Longitude=as.numeric(Longitude))%>%
  glimpse()

# Import 2018 data----
# Two steps to keep additional info to join back into 
dat.2018<-gs_title("Lobsters_data_2018_All")%>% 
  gs_read_csv(ws = "Lobster.var",col_types = "nccnccccccnccccccnnnncccccc")%>%
  mutate(Sample=paste(Trip,Day,Trap.ID,sep="."))%>%  # trip.day.trap is now "Sample"
  mutate(Source = 'Dongara.Millers.Masters')%>%
  dplyr::mutate(Date=as_date(dmy(Date)))%>%
  glimpse()

length.2018<-dat.2018%>%
  filter(!(is.na(Carapace.length)&is.na(Tag.number)&is.na(Colour)))%>%  
  # now filters out where all are blank (empty pots)
  mutate(Colour=str_replace_all(.$Colour,c("W"="White", "R"="Red")))%>%
  mutate(Sex=str_replace_all(.$Sex, c("M"="Male", "F"="Female","U"="Unknown")))%>%
  mutate(Sex=if_else((!is.na(Colour)&!Sex%in%c("Female","Male")),"Unknown",Sex))%>%
  mutate(Damage.old.a=if_else(Damage.new.a==9,0,Damage.old.a))%>% # Fix Damaged data
  mutate(Damage.new.L=if_else(Damage.new.a==9,0,Damage.new.L))%>%
  mutate(Damage.old.L=if_else(Damage.new.a==9,0,Damage.old.L))%>%
  mutate(Damage.new.a=if_else(Damage.new.a==9,0,Damage.new.a))%>%
  mutate(Setose.state=ifelse(Setose.state%in%c("I"),NA,Setose.state))%>%
  mutate(Setose.state=ifelse(Setose.state%in%c("E"),"Immature",Setose.state))%>%
  mutate(Setose.state=ifelse(Setose.state%in%c("M"),"Mature",Setose.state))%>%
  mutate(Egg.stage=ifelse(Egg.stage%in%c("E","I"),"Early",Egg.stage))%>%
  mutate(Egg.stage=ifelse(Egg.stage%in%c("M"),"Mid",Egg.stage))%>%
  dplyr::select( Date,Source,Trip,Sample,Tag.number,Recapture,Carapace.length,Sex,Colour,Setose.state,Egg.stage,Moult.stage,Damage.old.a,Damage.old.L,Damage.new.a,Damage.new.L,Dead,Outlier,Individual.Remarks)%>%
  glimpse()

# Check formating of Lobster data
names(length.2018)
unique(length.2018$Outlier) #  NA  "y"
unique(length.2018$Setose.state) # "I" NA  "E" "M" # Need to fix these up 
unique(length.2018$Egg.stage) #  NA  "I" "M" "E" # Need to fix these up 
unique(length.2018$Dead) # NA     "Dead"
unique(length.2018$Individual.Remarks) 
unique(length.2018$Recapture)
# Counts
length(unique(length.2018$Tag.number)) # 7565 tagged individuals
length(length.2018$Carapace.length) # 9318 total indiviuals caught
length(unique(length.2018$Sample)) # 1163 pots with crays

names(dat.2018)
info.2018<-dat.2018%>%
  dplyr::select(Trip,Sample, Day,Trap.ID, Trap.Number, Day.Pull, Pot.Remarks,PWO,PWF, WKW)%>% # Location, Date,
  distinct()%>%
  glimpse()

unique(info.2018$Pot.Remarks)
unique(info.2018$PWO)

duplicates<-info.2018%>%
  group_by(Sample)%>%
  summarise(n=n())%>%
  filter(n>1)
#####################################################
# #Count recaptures per trip---
# glimpse(length.2018) #9318
# test<-length.2018%>%
#   dplyr::mutate(Date=as_date(dmy(Date)))%>%
#   select(Sample, Date, Trip, Tag.number, Carapace.length)%>%
#   glimpse()
# 
# glimpse(test)
# meta<-metadata.2018%>%
#   select(Sample, Location)%>%
#   glimpse()
# 
# catch<- left_join(test, meta, by="Sample")%>%
#   mutate(sizeclass= ifelse(Carapace.length>=76.0,"Legal", "Sublegal"))%>% 
#   filter(!is.na(Carapace.length))%>%
#   filter(!is.na(Tag.number))%>%
#   mutate(Count=1)%>%
#   glimpse()
# 
# #order by date
# catch%<>%
#   arrange(Date)%>%
#   glimpse()
# 
# #filter to initial tags
# int<-catch%>%
#   dplyr::distinct(Tag.number,.keep_all = TRUE)%>% 
#   #filters out the duplicates (recaptues)
#   filter(!Trip=="8")%>% #Added !Trip=="1" & 
#   glimpse()
# 
# unique(int$Trip)
# tapply(int$Count, list(int$Location, int$sizeclass), length)
# 
# #Thin data to recaps
# rec <- catch[duplicated(catch$Tag.number), ]%>%
#   filter(!Trip=="8")%>% #Added
#   glimpse()
# 
# tapply(rec$Count, list(rec$Location, rec$sizeclass), length)
# 
# #Join with other data----
# all.recaps <- semi_join(test, recaps)%>%
#   glimpse()
# 
# #isolate recaps
# rec <- all.recaps[duplicated(all.recaps$Tag.number), ]%>%
#   glimpse()
# #join recaps
# int.rec <-full_join(int, rec, by="Tag.number")%>%
#   dplyr::rename(int.date=Date.x,
#                 int.trip=Trip.x,
#                 rec.date=Date.y,
#                 rec.trip=Trip.y)%>%
#   glimpse()
# 
# 
# #count of recaptures per trip-----
# count<-int.rec%>%
#   group_by(int.trip)%>%
#   summarise(Sum=sum(n()))%>%
#   glimpse()
# 
# trip<-int.rec%>%
#   filter(rec.trip=="8")%>%
#   glimpse()
# 
# trip%<>%
#   group_by(int.trip)%>%
#   summarise(Sum=sum(n()))%>%
#   glimpse()

#######################################################

# Import 2018 metadata----
metadata.2018<-gs_title("Lobsters_data_2018_All")%>% # To use GoogleSheets
  gs_read_csv(ws = "Pot.var",col_types = "nccnccncccnnnc")%>%
  mutate(Source = 'Dongara.Millers.Masters')%>%
  mutate(Sample=paste(Trip,Day,Trap.ID,sep="."))%>% 
  mutate(Site=str_replace_all(.$Site.Name,c( "SM"="Seven Mile", "DM"="Davids Marks",  "RM"="Rivermouth", "IR"="Irwin Reef", "LR"="Long Reef", "SD"="South Dummy", "LH"="Little Horseshoe", "CHin1_"="Cliff Head Mid","CHin2_"="Cliff Head South","CHout1_" = "Cliff Head OUT1","CHout2_" = "Cliff Head North", "JB"="Jim Bailey", "GR"="Golden Ridge", "SR"="South Rig", "WL"="Whites Lump")))%>% 
  # mutate(Location=str_replace_all(.$Site,c("Seven Mile Beach"= "Seven Mile","Little Horseshoe"="Cliff Head", "Cliff Head North"="Cliff Head","Cliff Head Mid"= "Cliff Head","Cliff Head South"="Cliff Head","Cliff Head OUT1"= "Cliff Head","CHM"="Cliff Head", "Davids Marks"="Cliff Head","CHM"= "Cliff Head", "CHS"="Cliff Head", "CHN"="Cliff Head", "Jim Bailey"="Irwin Reef", "Long Reef"="Irwin Reef", "South Dummy"="Irwin Reef","South Rig"= "Irwin Reef","Whites Lump"= "Irwin Reef","WP"= "Irwin Reef","Whitepoint"="Irwin Reef")))%>%
  #Change of Locations- AM
  mutate(Location=str_replace_all(.$Site,c("Seven Mile Beach"= "Seven Mile", "Cliff Head North"="Cliff Head","Cliff Head Mid"= "Cliff Head","Cliff Head South"="Cliff Head","Cliff Head OUT1"= "Cliff Head","CHM"="Cliff Head", "Davids Marks"="Cliff Head","CHM"= "Cliff Head", "CHS"="Cliff Head", "CHN"="Cliff Head", "Jim Bailey"="Irwin Reef", "Long Reef"="Irwin Reef", "South Dummy"="White Point","South Rig"= "White Point","Whites Lump"= "White Point","WP"= "White Point","Whitepoint"="White Point")))%>% 
  mutate(Site=str_replace_all(.$Site, c("Jim Bailey"="White Point" ,"WP"="White Point" , "Whitepoint"="White Point" , "CHS"="Cliff Head South","CHM"="Cliff Head Mid","CHN"="Cliff Head North", "Seven Mile Beach"= "Seven Mile South", "Cliff Head OUT1"="Cliff Head North", "Davids Marks"="Cliff Head North")))%>%
  filter(Johns=="No")%>% # turn off when I add in john's data (if ever) -  I am not uploading John's data to GlobalArchive BG
  dplyr::rename(Latitude=Latitude.y, Longitude=Longitude.x,Date=Date.Recovered)%>%
  mutate(Latitude=as.numeric(Latitude))%>%
  mutate(Longitude=as.numeric(Longitude))%>%
  filter(!Comment%in%c("Duplicate"))%>%
  select(-c(Johns,Comment,Creation.Date))%>%
  dplyr::rename(Site.Code=Site.Name)%>%
  mutate(Site.Code=str_replace_all(.$Site.Code,c("_"="")))%>% # Remove trailing underscores from site code
  left_join(.,info.2018)%>%
  mutate(Day.Pull=as.numeric(Day.Pull))%>%
  dplyr::mutate(Date=as_date(dmy(Date)))%>%
  select(Source,Sample,Trip,Day,Site.Code,Pot.Number,Location,Site,Date,Day.Pull,Latitude,Longitude,Depth.lidar,Pot.Type,Pot.Remarks,PWO,PWF,WKW)%>% # Trap.ID,
  glimpse()


names(metadata.2018)
unique(metadata.2018$Date)
unique(metadata.2018$Location)%>%sort() # "Cliff Head"   "Golden Ridge" "Irwin Reef"   "Rivermouth"   "Seven Mile", "Little Horseshoe", "White Point"
unique(metadata.2018$Site)%>%sort() 
unique(metadata.2018$Site.Code)%>%sort()
length(unique(metadata.2018$Sample)) # 1461 (1163 non-empty, 298 empty)

duplicate.pots<-metadata.2018%>%
  group_by(Sample)%>%
  dplyr::summarise(n=n())%>%filter(n>1) # No longer any duplicates

unique(metadata.2018$PWO)

pwo<-metadata.2018%>%
  dplyr::filter(PWO%in%c("1", "2", "X"))%>% #&PWO=="2"PWO=="X")%>%
  mutate(Count=1)%>%
  glimpse()

tapply(pwo$Count, list(pwo$Location), length)


# Clean up environment
rm(info.2018,duplicate.pots,dat.2018)

# Import 2017 length data----
# Using original 2017 Data -----
dat.2017<-gs_title("1_Lobsters_data_171210.xlsx")%>% # To use GoogleSheets
  gs_read_csv(ws = "Sheet1",col_types = "nccccnccccccccnnnnccccc")%>% 
  mutate(Trap.ID=str_replace_all(.$Trap.number,c("CH6F6"="CH6C6")))%>%
  mutate(Pot.type=ifelse(Trap.ID%in%c("CH6C6"),"C",Pot.type))%>%
  mutate(Sample=paste(Day,Trap.ID,sep="."))%>%  #New column for Day and trap.number
  mutate(Source='Dongara.Canons.Masters')%>%
  dplyr::mutate(Date=as_date(mdy(Date)))%>%
  glimpse()

length.2017<-dat.2017%>%
  filter(!(is.na(Carapace.length)&is.na(Sex)&is.na(Colour)))%>%
  mutate(Sex=ga.capitalise(Sex))%>%
  mutate(Sex=if_else((!is.na(Colour)&!Sex%in%c("Female","Male")),"Unknown",Sex))%>%
  mutate(Colour=ga.capitalise(Colour))%>%
  mutate(Colour=if_else(Colour%in%c("Unkown"),"Unknown",Colour))%>%
  mutate(Cable.Tie=ifelse(Tag.number%in%c("CT"),TRUE,FALSE))%>%
  mutate(Recapture=ifelse(Tagged%in%c("EXISTING"),TRUE,NA))%>%
  mutate(Tag.number=ifelse(Tag.number%in%c("CT"),NA,Tag.number))%>%
  select(-c(Day,Trap.number,Trap.ID,Location,Pot.type,Date,Pot.Remarks,PWO,PWF,Tagged))%>%
  dplyr::select(Source,Sample,Tag.number,Recapture,Carapace.length,Sex,Colour,Damage.old.a,Damage.old.L,Damage.new.a,Damage.new.L,Dead,Individual.Remarks,everything())%>%
  mutate(Recapture=as.character(Recapture))%>%
  mutate(Trip=0)%>%
  #dplyr::mutate(Date=as_date(dmy(Date)))%>%
  glimpse()

length(unique(length.2017$Sample)) #323
unique(length.2017$Carapace.length) # numeric
unique(length.2017$Sex) # Character
unique(length.2017$Colour) # character
unique(length.2017$Dead)
unique(length.2017$Individual.Remarks)
length(length.2017$Carapace.length) #4133
length(unique(length.2017$Tag.number)) #1683




info.2017<-dat.2017%>%
  dplyr::select(Sample,Day,Trap.ID,Pot.Remarks,PWO,PWF)%>% # Location, Date,
  distinct()%>%
  glimpse()

unique(info.2017$Pot.Remarks)
unique(info.2017$PWO)

duplicates<-info.2017%>%
  group_by(Sample)%>%
  summarise(n=n())%>%
  filter(n>1)

# Import 2017 pot data - from NEW sheet ----
metadata.2017<-gs_title("Lobsters_data_20180214")%>% # To use GoogleSheets
  gs_read_csv(ws = "Pot.var")%>%
  mutate(Source='Dongara.Canons.Masters')%>%
  mutate(Sample=paste(Day,Trap.number,sep="."))%>% 
  mutate(Location=str_replace_all(.$Location,c("Seven Mile Beach"= "Seven Mile")))%>% 
  mutate(Trip=0)%>%
  select(-c(ID,Notes,Code,Latitude,Longitude,Longitude.original,Latitude.original))%>%
  dplyr::rename(Longitude=Longitud.relocated,Latitude=Latitude.relocated,Day.Pull=Soking.time.days,Trap.ID=Trap.number,Pot.Number=Number,Pot.Type=Pot.type)%>%
  left_join(.,info.2017)%>%
  mutate(Exclude.pots=ga.capitalise(Exclude.pots))%>%
  mutate(Site=str_replace_all(.$Names.to.display,c("7 Mile"="Seven Mile Mid")))%>%
  mutate(Site=str_replace_all(.$Site, c("Cliff Head Sand Strips"="Cliff Head","Cliff Head"="Cliff Head North","South Whites Lump"= "White Point", "White Point Reef"="White Point", "South West Dummy"="South Dummy", "Big Horseshoe"="Little Horseshoe", "Sand Hole"="Little Horseshoe", "Darwin"="White Point", "Dry Shallows"="Little Horseshoe", "Power Pack"="Little Horseshoe", "Outside CH1"="Kevin Healy", "Outside CH2"="Cliff Head North")))%>%
  mutate(Location=str_replace_all(.$Site,c("Seven Mile Mid"="Seven Mile", "Cliff Head North"="Cliff Head", "Cliff Head"="Cliff Head", "White Point"="White Point", "Little Horseshoe"="Little Horseshoe", "South Dummy"="White Point", "Kevin Healy"="Cliff Head", "Whites Lump"="White Point", "Long Reef"="Irwin Reef")))%>%
  separate(Trap.ID,into=c("Site.Code","Extra"),sep=-2)%>%
  mutate(Site.Code=str_replace_all(.$Site.Code,c("F"="","5C"="5","0C"="0","7C"="7","2C"="2","4C"="4","3C"="3","6C"="6")))%>%
  select(-c(John.site.names,Names.to.display,Extra))%>%
  mutate(Pot.Type=as.character(Pot.Type))%>%
  select(Source,Sample,Trip,Day,Site.Code,Pot.Number,Location,Site,Date,Day.Pull,Latitude,Longitude,Depth.lidar,Pot.Type,Pot.Remarks,PWO,PWF,everything())%>%
  dplyr::mutate(Date=as_date(dmy(Date)))%>%
  glimpse()

# Check for pots in length that aren't in the pot data ----
missing.pot.var<-anti_join(length.2017,metadata.2017) # Two pots that arent in the original

# Clean up enviroment
rm(missing.pot.var,duplicates,info.2017,dat.2017)

unique(metadata.2017$Site)
unique(metadata.2017$Location)
length(unique(metadata.2017$Sample)) #467 total pot pulls
unique(metadata.2018$Site)

# Tidy names of data frames ----
names(length.2017)<-ga.capitalise(names(length.2017))
names(length.2018)<-ga.capitalise(names(length.2018))
names(length.2019)<-ga.capitalise(names(length.2019))


names(metadata.2017)<-ga.capitalise(names(metadata.2017))
names(metadata.2018)<-ga.capitalise(names(metadata.2018))
names(metadata.2019)<-ga.capitalise(names(metadata.2019))

# Combine data ----

metadata<-bind_rows(metadata.2017,metadata.2018, metadata.2019)%>%
  replace_na(list(Exclude.pots="No"))%>%
  glimpse()


length<-bind_rows(length.2017,length.2018, length.2019)%>%
  replace_na(list(Damage.old.a = 0, Damage.old.l = 0,Damage.new.a = 0, Damage.new.l = 0))%>%
  mutate(Total.damage=(Damage.old.a+Damage.old.l+Damage.new.a+Damage.new.l))%>%
  replace_na(list(Sex="Unknown",Colour="Unknown",Cable.tie="FALSE",Dead="Alive"))%>%
  mutate(Count=1)%>% # Count for Abundance
  glimpse()

names(metadata)

unique(metadata$Site)
unique(metadata$Location)
unique(metadata$Source)
unique(metadata$Trip)
unique(metadata$Pot.type) # "C" "F" "13" NA 
unique(metadata$Pot.remarks)
unique(metadata$Date) 
unique(metadata$Exclude.pots) # "No" "Yes" NA

names(length)

unique(length$Source)
unique(length$Recapture) # NA     "TRUE"
unique(length$Sex)
unique(length$Colour)
unique(length$Dead)
unique(length$Individual.remarks)
unique(length$Reproductive.stage)
unique(length$Setose.state)
unique(length$Egg.stage)
unique(length$Moult.stage)
unique(length$Cable.tie)
unique(length$Outlier)


# Remove lobsters that have changed sex
changed.sex<-length%>%
  filter(!is.na(Tag.number))%>%
  group_by(Tag.number)%>%
  dplyr::summarise(no.sex=length(unique(Sex)),no.times.caught=n())%>%
  filter(no.sex>1)

cant.keep<-changed.sex%>%
  filter(no.times.caught==2)%>%
  distinct(Tag.number)

#36

list.tags <- as.vector(changed.sex$Tag.number) # change to cant.keep if filtered out only those caught twice

changes<-length%>%
  filter(Tag.number%in%c(list.tags))

length<-length%>%
  filter(!Tag.number%in%c(list.tags))

# Write data
setwd(data.dir)
dir()

write.csv(metadata, "metadata.csv",row.names = FALSE)
write.csv(length, "length.csv",row.names = FALSE)

# Brooke for global----
metadata.raw<-metadata%>%
  dplyr::rename(Comment=Pot.remarks)%>%
  mutate(Successful.count=ifelse(Exclude.pots%in%c("Yes"),"No","Yes"))%>%
  mutate(Successful.length=ifelse(Exclude.pots%in%c("Yes"),"No","Yes"))%>%
  mutate(Successful.count=ifelse(Comment%in%c("Pot Broke"),"No",Successful.count))%>%
  mutate(Successful.length=ifelse(Comment%in%c("Pot Broke"),"No",Successful.length))%>%
  select(-c(Exclude.pots))%>%
  mutate(Time="12:00:00")%>%
  mutate(Status="Fished")%>%
  mutate(Depth=ifelse(!is.na(Depth.lidar),Depth.lidar,0))%>%
  mutate(Observer=ifelse(Source%in%c("Dongara.Canons.Masters"),"Oscar Doncel Canon","Ash Miller"))%>%
  glimpse()

length.raw<-length%>%
  dplyr::rename(Length=Carapace.length)%>%
  mutate(Family="Palinuridae",Genus="Panulirus",Species="cygnus")%>%
  replace_na(list(Length=(-9999)))%>%
  #select(-c(Date))%>%
  semi_join(metadata.raw, by = c("Source", "Sample", "Trip"))%>%
  glimpse()

# Make a more GlobalArchive-y CampaignID
campaigns<-metadata.raw%>%
  distinct(Trip,Source,Date)%>%
  group_by(Trip,Source)%>%
  arrange(desc(Date))%>%
  slice(n())%>%
  ungroup()%>%
  mutate(CampaignID=paste(str_sub(Date,1,7),Source,"Trapping",sep="_"))%>%
  select(-c(Date))

# Add campaigns to metadata and length ----
metadata.cam<-left_join(metadata.raw,campaigns)%>%
  mutate(Date=str_replace_all(.$Date,c("-"=""))) # fix format of date here - cant do above as need it to make campaignid

length.cam<-left_join(length.raw,campaigns)

# Remove csv unfriendly tings (e.g. commas) from metadata ----
metadata.cam[] <- sapply(metadata.cam, remove.commas)
metadata.cam[] <- sapply(metadata.cam, remove.colon)
metadata.cam[] <- sapply(metadata.cam, remove)

# Split data into campaigns based on trip ----
# Loop through to save individuals campaigns ----
setwd(uploads)
dir()

campaigns <- unique(unlist(metadata.cam$CampaignID))

for (i in 1:length(campaigns)){
  # Metadata
  temp.met <- subset(metadata.cam, CampaignID == campaigns[i])
  id<-temp.met$CampaignID
  # Remove CampaignID from dataframe and then remove all columns where all are NA
  temp.met2<-temp.met%>%select(-c(CampaignID))
  temp.met3<-temp.met2[,which(unlist(lapply(temp.met2, function(x)!all(is.na(x)))))]
  # Make an empty data frame to bring back in any columns that were removed but need to be in the data
  columns<-c("Sample","Latitude","Longitude","Date","Time","Location","Status","Site","Depth","Observer","Successful.count","Successful.length","Comment")%>%map_dfr( ~tibble(!!.x := logical() ) )
  # Add back in cols that may have been deleted
  temp.met4<-bind_rows(columns, temp.met3)
  # write file
  write.csv(temp.met4, file=paste(unique(id),"_Metadata.csv",sep=""), quote=FALSE,row.names = FALSE,na = "")
  
  # Length
  temp.length <- subset(length.cam, CampaignID == campaigns[i])%>%select(-c(CampaignID)) # Remove CampaignID from dataframe
  write.csv(temp.length, file=paste(unique(id),"_Length.csv",sep=""), quote=FALSE,row.names = FALSE,na = "") # write file
}


test<-metadata%>%
  group_by(Sample,Trip)%>%
  summarise(n=n())
