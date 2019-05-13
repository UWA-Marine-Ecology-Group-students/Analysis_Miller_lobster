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
library(GlobalArchive)

# Study name----
study<-"Lobster.Data"

# Set work directory----
# work.dir=("~/GitHub/Analysis_Miller_WRL") #for Tim's github
# work.dir=("~/workspace/Analysis_Miller_WRL") #for ecocloud server
work.dir=("C:/GitHub") # For Brooke
# work.dir=("Z:/Analysis_Miller_lobster") # FOr Ash's laptop using Git

# Sub directories ----
data.dir<-paste(work.dir,"Data",sep="/")

# Import 2018 data----
# Two steps to keep additional info to join back into 
dat.2018<-gs_title("Lobsters_data_2018_All")%>% 
  gs_read_csv(ws = "Lobster.var",col_types = "nccncccccccccccccnnnncccccc")%>%
  mutate(Sample=paste(Trip,Day,Trap.ID,sep="."))%>%  # trip.day.trap is now "Sample"
  mutate(Source = 'ash-millers-masters')%>%
  glimpse()

dat.length.2018<-dat.2018%>%
  filter(!(is.na(Carapace.length)&is.na(Tag.number)&is.na(Colour)))%>% # now filters out where all are blank (empty pots)
  mutate(Colour=str_replace_all(.$Colour,c("W"="White", "R"="Red")))%>%
  mutate(Sex=str_replace_all(.$Sex, c("M"="Male", "F"="Female","U"="Unknown")))%>%
  mutate(Sex=if_else((!is.na(Colour)&!Sex%in%c("Female","Male")),"Unknown",Sex))%>%
  mutate(Damage.old.a=if_else(Damage.new.a==9,0,Damage.old.a))%>% # Fix Damaged data
  mutate(Damage.new.L=if_else(Damage.new.a==9,0,Damage.new.L))%>%
  mutate(Damage.old.L=if_else(Damage.new.a==9,0,Damage.old.L))%>%
  mutate(Damage.new.a=if_else(Damage.new.a==9,0,Damage.new.a))%>%
  dplyr::select(Source,Sample,Tag.number,Recapture,Carapace.length,Sex,Colour,Setose.state,Egg.stage,Moult.stage,Damage.old.a,Damage.old.L,Damage.new.a,Damage.new.L,Dead,Outlier,Individual.Remarks)%>%
  glimpse()

# Check formating of Lobster data
names(dat.length.2018)
unique(dat.length.2018$Outlier) #  NA  "y"
unique(dat.length.2018$Setose.state) # "I" NA  "E" "M" # Need to fix these up 
unique(dat.length.2018$Egg.stage) #  NA  "I" "M" "E" # Need to fix these up 
unique(dat.length.2018$Dead) # NA     "Dead"
unique(dat.length.2018$Individual.Remarks) 

# Counts
length(unique(dat.length.2018$Tag.number)) # 7537 tagged individuals
length(dat.length.2018$Carapace.length) # 9318 total indiviuals caught
length(unique(dat.length.2018$Sample)) # 1163 pots

# Things that should actually be in the metadata not the lobster data
additional.info.2018<-dat.2018%>%
  dplyr::select(Sample,Date,Day.Pull,Pot.Remarks,PWO,WKW,PWF)%>% # Trap.Number,
  distinct()%>%
  glimpse()

length(unique(additional.info.2018$Sample)) # 1440 

duplicate.remarks<-additional.info.2018%>%
  group_by(Sample)%>%
  summarise(n=n())%>%
  filter(n>1)

unique(additional.info.2018$Pot.Remarks)

# Import 2018 pot data----
dat.pot.2018<-gs_title("Lobsters_data_2018_All")%>% # To use GoogleSheets
  gs_read_csv(ws = "Pot.var",col_types = "nccnccnnccnnnc")%>%
  mutate(Source = 'ash-millers-masters')%>%
  mutate(Sample=paste(Trip,Day,Trap.ID,sep="."))%>% 
  mutate(Site=str_replace_all(.$Site.Name,c( "SM"="Seven Mile", "DM"="Davids Marks",  "RM"="Rivermouth", "IR"="Irwin Reef", "LR"="Long Reef", "SD"="South Dummy", "LH"="Little Horseshoe", "CHin1_"="Cliff Head Mid","CHin2_"="Cliff Head South","CHout1_" = "Cliff Head OUT1","CHout2_" = "Cliff Head North", "JB"="Jim Bailey", "GR"="Golden Ridge", "SR"="South Rig", "WL"="Whites Lump")))%>% 
  mutate(Location=str_replace_all(.$Site,c("Seven Mile Beach"= "Seven Mile","Little Horseshoe"="Cliff Head", "Cliff Head North"="Cliff Head","Cliff Head Mid"= "Cliff Head","Cliff Head South"="Cliff Head","Cliff Head OUT1"= "Cliff Head","CHM"="Cliff Head", "Davids Marks"="Cliff Head","CHM"= "Cliff Head", "CHS"="Cliff Head", "CHN"="Cliff Head", "Jim Bailey"="Irwin Reef", "Long Reef"="Irwin Reef", "South Dummy"="Irwin Reef","South Rig"= "Irwin Reef","Whites Lump"= "Irwin Reef","WP"= "Irwin Reef","Whitepoint"="Irwin Reef")))%>% 
  mutate(Site=str_replace_all(.$Site, c("Jim Bailey"="White Point" ,"WP"="White Point" , "Whitepoint"="White Point" , "CHS"="Cliff Head South","CHM"="Cliff Head Mid","CHN"="Cliff Head North", "Seven Mile Beach"= "Seven Mile.out")))%>%
  filter(Johns=="No")%>% # turn off when I add in john's data (if ever) -  I am not uploading John's data to GlobalArchive BG
  dplyr::rename(Latitude=Latitude.y, Longitude=Longitude.x)%>%
  mutate(Latitude=as.numeric(Latitude))%>%
  mutate(Longitude=as.numeric(Longitude))%>%
  filter(!Comment%in%c("Duplicate"))%>%
  select(-c(Johns,Comment,Creation.Date,Date.Recovered))%>%
  dplyr::rename(Site.Code=Site.Name)%>%
  mutate(Site.Code=str_replace_all(.$Site.Code,c("_"="")))%>% # Remove trailing underscores from site code
  left_join(.,additional.info.2018)%>%
  select(Source,Sample,Trip,Day,Site.Code,Pot.Number,Location,Site,Date,Day.Pull,Latitude,Longitude,Depth.lidar,Pot.Type,Pot.Remarks,PWO,PWF,WKW)%>% # Trap.ID,
  glimpse()

# Don't need Trap.ID

names(dat.pot.2018)

unique(dat.pot.2018$Location)%>%sort() # "Cliff Head"   "Golden Ridge" "Irwin Reef"   "Rivermouth"   "Seven Mile"  
unique(dat.pot.2018$Site)%>%sort() # Would be nice to rename "Cliff Head OUT1"
unique(dat.pot.2018$Site.Code)%>%sort()

length(unique(dat.pot.2018$Sample)) # 1461

# Two problems
# 1. duplicates in trip.day.trap in dat.pot - fixed
# 2. does not match additional info number of samples

missing.from.lobster<-anti_join(additional.info.2018,dat.pot.2018, by = c("Trip",  "Trap.ID", "Day", "Sample")) # Location is what causes the errors

missing.field.info<-anti_join(dat.pot.2018,additional.info.2018, by = c("Trip", "Trap.ID", "Day", "Sample"))
# 21 pots that aren't in the lobster data

# 1461 pots (1163 non-empty, 298 empty)

duplicate.pots<-dat.pot.2018%>%
  group_by(Sample)%>%
  dplyr::summarise(n=n())%>%filter(n>1) # No longer any duplicates

# Clean up environment
rm(additional.info.2018,duplicate.pots,duplicate.remarks,missing.field.info,missing.from.lobster,dat.2018)

# Import 2017 length data----
# Using original 2017 Data -----
dat.2017<-gs_title("1_Lobsters_data_171210.xlsx")%>% # To use GoogleSheets
  gs_read_csv(ws = "Sheet1",col_types = "nccccnccccccnnnnccccc")%>% #
  mutate(Trap.ID=str_replace_all(.$Trap.number,c("CH6F6"="CH6C6")))%>%
  mutate(Pot.type=ifelse(Trap.ID%in%c("CH6C6"),"C",Pot.type))%>%
  mutate(Sample=paste(Day,Trap.ID,sep="."))%>%  #New column for Day and trap.number
  mutate(Source='oscar-doncel-canons-masters')%>%
  glimpse()

dat.length.2017<-dat.2017%>%
  filter(!(is.na(Carapace.length)&is.na(Sex)&is.na(Colour)))%>%
  mutate(Sex=capitalise(Sex))%>%
  mutate(Sex=if_else((!is.na(Colour)&!Sex%in%c("Female","Male")),"Unknown",Sex))%>%
  mutate(Colour=capitalise(Colour))%>%
  mutate(Colour=if_else(Colour%in%c("Unkown"),"Unknown",Colour))%>%
  select(-c(Location,Pot.type,Date,Pot.Remarks,PWO,PWF,Source))%>%
  glimpse()

length(unique(dat.length.2017$Sample))
unique(dat.length.2017$Day) # numeric
unique(dat.length.2017$Trap.ID) # character
unique(dat.length.2017$Carapace.length) # numeric
unique(dat.length.2017$Sex) # Character
unique(dat.length.2017$Colour) # character
unique(dat.length.2017$Dead)
unique(dat.length.2017$Individual.Remarks)

names(dat.2017)

additional.info.2017<-dat.2017%>%
  dplyr::select(Sample,Day,Trap.ID,Pot.Remarks,PWO,PWF)%>% # Location, Date,
  distinct()%>%
  glimpse()

unique(additional.info.2017$Location) # character
unique(additional.info.2017$Pot.type) # character
unique(additional.info.2017$Date) #character
unique(additional.info.2017$Pot.Remarks)
unique(additional.info.2017$PWO)

duplicates<-additional.info.2017%>%
  group_by(Sample)%>%
  summarise(n=n())%>%
  filter(n>1)

# Import 2017 pot data - from NEW sheet ----
dat.pot.2017<-gs_title("Lobsters_data_20180214")%>% # To use GoogleSheets
  gs_read_csv(ws = "Pot.var")%>%
  mutate(Source='oscar-doncel-canons-masters')%>%
  mutate(Sample=paste(Day,Trap.number,sep="."))%>% 
  mutate(Location=str_replace_all(.$Location,c("Seven Mile Beach"= "Seven Mile","Whitepoint"="Irwin Reef")))%>% 
  mutate(Trip=0)%>%
  select(-c(ID,Notes,Code,Latitude,Longitude,Longitude.original,Latitude.original))%>%
  dplyr::rename(Longitude=Longitud.relocated,Latitude=Latitude.relocated,Day.Pull=Soking.time.days,Trap.ID=Trap.number,Pot.Number=Number,Pot.Type=Pot.type)%>%
  left_join(.,additional.info.2017)%>%
  mutate(Exclude.pots=capitalise(Exclude.pots))%>%
  mutate(Site=str_replace_all(.$Names.to.display,c("7 Mile"="Seven Mile")))%>%
  separate(Trap.ID,into=c("Site.Code","Extra"),sep=-2)%>%
  mutate(Site.Code=str_replace_all(.$Site.Code,c("F"="","5C"="5","0C"="0","7C"="7","2C"="2","4C"="4","3C"="3","6C"="6")))%>%
  select(-c(John.site.names,Names.to.display,Extra))%>%
  select(Source,Sample,Trip,Day,Site.Code,Pot.Number,Location,Site,Date,Day.Pull,Latitude,Longitude,Depth.lidar,Pot.Type,Pot.Remarks,PWO,PWF,everything())%>%
  glimpse()

names(dat.pot.2017)
names(dat.pot.2018)

unique(dat.pot.2017$Site.Code)


# Check for pots in length that aren't in the pot data ----
missing.pot.var<-anti_join(dat.length.2017,dat.pot.2017) # Two pots that arent in the original

# Clean up enviroment


rm(missing.pot.var,test,duplicates,additional.info.2017)

unique(dat.pot.2018$Location)%>%sort() # "Cliff Head"   "Golden Ridge" "Irwin Reef"   "Rivermouth"   "Seven Mile"  
unique(dat.pot.2017$Location)%>%sort() # "Cliff Head" "Irwin Reef" "Seven Mile"












# Import Tag Return data sent to Fisheries----

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



## Brooke combining data

# Things that need to happen to both 2017 and 2018 data

metadata<-
dplyr::mutate(Date=as_date(dmy(Date)))%>%
  
  
length<-
  mutate(Total.damage=(Damage.old.a+Damage.old.L+Damage.new.a+Damage.new.L))%>%
  mutate(Count=1)%>% # Count for Abundance
  mutate(Carapace.length=as.numeric(as.character(Carapace.length)))%>%
  replace_na(list(Damage.old.a = 0, Damage.old.L = 0,Damage.new.a = 0, Damage.new.L = 0,Sex="Unknown",Colour="Unknown"))%>%
  
  
  
