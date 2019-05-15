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
library(devtools)
#install_github("UWAMEGFisheries/GlobalArchive", dependencies = TRUE) # to check for updates
library(GlobalArchive) # Have to add how to download for Ash

# Study name----
study<-"Lobster.Data"

# Set work directory----
# work.dir=("~/GitHub/Analysis_Miller_WRL") #for Tim's github
# work.dir=("~/workspace/Analysis_Miller_WRL") #for ecocloud server
work.dir=("C:/GitHub/Analysis_Miller_lobster") # For Brooke
work.dir=("Z:/Analysis_Miller_lobster") # FOr Ash's laptop using Git

# Sub directories ----
data.dir<-paste(work.dir,"Data",sep="/")

# Import 2018 data----
# Two steps to keep additional info to join back into 
dat.2018<-gs_title("Lobsters_data_2018_All")%>% 
  gs_read_csv(ws = "Lobster.var",col_types = "nccnccccccnccccccnnnncccccc")%>%
  mutate(Sample=paste(Trip,Day,Trap.ID,sep="."))%>%  # trip.day.trap is now "Sample"
  mutate(Source = 'ash-millers-masters')%>%
  glimpse()

length.2018<-dat.2018%>%
  filter(!(is.na(Carapace.length)&is.na(Tag.number)&is.na(Colour)))%>% # now filters out where all are blank (empty pots)
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
  dplyr::select(Source,Sample,Tag.number,Recapture,Carapace.length,Sex,Colour,Setose.state,Egg.stage,Moult.stage,Damage.old.a,Damage.old.L,Damage.new.a,Damage.new.L,Dead,Outlier,Individual.Remarks)%>%
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
length(unique(length.2018$Tag.number)) # 7537 tagged individuals
length(length.2018$Carapace.length) # 9318 total indiviuals caught
length(unique(length.2018$Sample)) # 1163 pots

# Things that should actually be in the metadata not the lobster data
info.2018<-dat.2018%>%
  dplyr::select(Sample,Day.Pull,Pot.Remarks,PWO,WKW,PWF)%>% # Trap.Number, Date
  distinct()%>%
  glimpse()

length(unique(info.2018$Sample)) # 1440 

duplicate.remarks<-info.2018%>%
  group_by(Sample)%>%
  summarise(n=n())%>%
  filter(n>1)

unique(info.2018$Pot.Remarks)

# Import 2018 metadata----
metadata.2018<-gs_title("Lobsters_data_2018_All")%>% # To use GoogleSheets
  gs_read_csv(ws = "Pot.var",col_types = "nccnccncccnnnc")%>%
  mutate(Source = 'ash-millers-masters')%>%
  mutate(Sample=paste(Trip,Day,Trap.ID,sep="."))%>% 
  mutate(Site=str_replace_all(.$Site.Name,c( "SM"="Seven Mile", "DM"="Davids Marks",  "RM"="Rivermouth", "IR"="Irwin Reef", "LR"="Long Reef", "SD"="South Dummy", "LH"="Little Horseshoe", "CHin1_"="Cliff Head Mid","CHin2_"="Cliff Head South","CHout1_" = "Cliff Head OUT1","CHout2_" = "Cliff Head North", "JB"="Jim Bailey", "GR"="Golden Ridge", "SR"="South Rig", "WL"="Whites Lump")))%>% 
  mutate(Location=str_replace_all(.$Site,c("Seven Mile Beach"= "Seven Mile","Little Horseshoe"="Cliff Head", "Cliff Head North"="Cliff Head","Cliff Head Mid"= "Cliff Head","Cliff Head South"="Cliff Head","Cliff Head OUT1"= "Cliff Head","CHM"="Cliff Head", "Davids Marks"="Cliff Head","CHM"= "Cliff Head", "CHS"="Cliff Head", "CHN"="Cliff Head", "Jim Bailey"="Irwin Reef", "Long Reef"="Irwin Reef", "South Dummy"="Irwin Reef","South Rig"= "Irwin Reef","Whites Lump"= "Irwin Reef","WP"= "Irwin Reef","Whitepoint"="Irwin Reef")))%>% 
  mutate(Site=str_replace_all(.$Site, c("Jim Bailey"="White Point" ,"WP"="White Point" , "Whitepoint"="White Point" , "CHS"="Cliff Head South","CHM"="Cliff Head Mid","CHN"="Cliff Head North", "Seven Mile Beach"= "Seven Mile.out")))%>%
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

unique(metadata.2018$Location)%>%sort() # "Cliff Head"   "Golden Ridge" "Irwin Reef"   "Rivermouth"   "Seven Mile"  
unique(metadata.2018$Site)%>%sort() # Would be nice to rename "Cliff Head OUT1"
unique(metadata.2018$Site.Code)%>%sort()

length(unique(metadata.2018$Sample)) # 1461 (1163 non-empty, 298 empty)

duplicate.pots<-metadata.2018%>%
  group_by(Sample)%>%
  dplyr::summarise(n=n())%>%filter(n>1) # No longer any duplicates

# Clean up environment
rm(info.2018,duplicate.pots,duplicate.remarks,missing.field.info,missing.from.lobster,dat.2018)

# Import 2017 length data----
# Using original 2017 Data -----
dat.2017<-gs_title("1_Lobsters_data_171210.xlsx")%>% # To use GoogleSheets
  gs_read_csv(ws = "Sheet1",col_types = "nccccnccccccnnnnccccc")%>% #
  mutate(Trap.ID=str_replace_all(.$Trap.number,c("CH6F6"="CH6C6")))%>%
  mutate(Pot.type=ifelse(Trap.ID%in%c("CH6C6"),"C",Pot.type))%>%
  mutate(Sample=paste(Day,Trap.ID,sep="."))%>%  #New column for Day and trap.number
  mutate(Source='oscar-doncel-canons-masters')%>%
  glimpse()

length.2017<-dat.2017%>%
  filter(!(is.na(Carapace.length)&is.na(Sex)&is.na(Colour)))%>%
  mutate(Sex=capitalise(Sex))%>%
  mutate(Sex=if_else((!is.na(Colour)&!Sex%in%c("Female","Male")),"Unknown",Sex))%>%
  mutate(Colour=capitalise(Colour))%>%
  mutate(Colour=if_else(Colour%in%c("Unkown"),"Unknown",Colour))%>%
  mutate(Cable.Tie=ifelse(Tag.number%in%c("CT"),TRUE,FALSE))%>%
  mutate(Recapture=ifelse(Tagged%in%c("EXISTING"),TRUE,NA))%>%
  mutate(Tag.number=ifelse(Tag.number%in%c("CT"),NA,Tag.number))%>%
  select(-c(Day,Trap.number,Trap.ID,Location,Pot.type,Date,Pot.Remarks,PWO,PWF,Tagged))%>%
  dplyr::select(Source,Sample,Tag.number,Recapture,Carapace.length,Sex,Colour,Damage.old.a,Damage.old.L,Damage.new.a,Damage.new.L,Dead,Individual.Remarks,everything())%>%
  mutate(Recapture=as.character(Recapture))%>%
  glimpse()

length(unique(length.2017$Sample)) #323
unique(length.2017$Carapace.length) # numeric
unique(length.2017$Sex) # Character
unique(length.2017$Colour) # character
unique(length.2017$Dead)
unique(length.2017$Individual.Remarks)
names(dat.2017)

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
  mutate(Source='oscar-doncel-canons-masters')%>%
  mutate(Sample=paste(Day,Trap.number,sep="."))%>% 
  mutate(Location=str_replace_all(.$Location,c("Seven Mile Beach"= "Seven Mile","Whitepoint"="Irwin Reef")))%>% 
  mutate(Trip=0)%>%
  select(-c(ID,Notes,Code,Latitude,Longitude,Longitude.original,Latitude.original))%>%
  dplyr::rename(Longitude=Longitud.relocated,Latitude=Latitude.relocated,Day.Pull=Soking.time.days,Trap.ID=Trap.number,Pot.Number=Number,Pot.Type=Pot.type)%>%
  left_join(.,info.2017)%>%
  mutate(Exclude.pots=capitalise(Exclude.pots))%>%
  mutate(Site=str_replace_all(.$Names.to.display,c("7 Mile"="Seven Mile")))%>%
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

# Import Tag Return data sent to Fisheries----
fisheries.returns <- gs_title("Fisheries.Tag.Returns")%>%
  gs_read_csv(ws="Sheet1")%>%
  mutate(Recapture=TRUE)%>%
  mutate(Colour="Unknown")%>%
  mutate(Source= "fisheries-returns")%>%
  mutate(Sex=str_replace_all(.$Sex, c("M"="Male", "F"="Female")))%>%
  dplyr::rename(Tag.number=SpeciesTag, Carapace.length=SpeciesSize, Date=ReportCreatedDate, Fisher=ReportersUserName)%>%
  dplyr::mutate(Date=as_date(mdy(Date)))%>%
  mutate(Setose.state=str_replace_all(.$SpeciesName,c("Western rock lobster - "="","male"="","Western rock lobster"="","Western rock lobster- "="","- "="","setose no tarspot"="Mature","setose with tarspot"="Mature","non setose"="Immature")))%>%
  mutate(Tarspot=ifelse(SpeciesName%in%c("Western rock lobster - setose (with tarspot)"),TRUE,NA))%>%
  mutate(Retention.Status=ifelse(RetentionStatus%in%c("KEPT","kept"),"Retained",RetentionStatus))%>%
  replace_na(list(Sex="Unknown"))%>%
  mutate(Dead=ifelse(Comments%in%c("Eaten by octopus"),"Dead",NA))%>%
  rename(Individual.Remarks=Comments,Fisher.Email=ReportersUserEmail,Depth.fms=depth.fms,Exclude.Pots=Errors)%>%
  select(-c(ReportId,SpeciesName,SpeciesCategory,ReportersPhoneNumber,ReportersBoatRegistrationNumber,address,X19,X20,X21,X22,RetentionStatus))%>%
  glimpse()

length(fisheries.returns$Tag.number) # 183
length(unique(fisheries.returns$Tag.number)) # 161 unique individuals

names(fisheries.returns)

unique(fisheries.returns$Setose.state)
unique(fisheries.returns$Sex)
unique(fisheries.returns$Carapace.length)
unique(fisheries.returns$Individual.Remarks)
unique(fisheries.returns$Colour)
unique(fisheries.returns$Retention.Status)

# Change format of Longitude and Latitude
dm <- fisheries.returns%>%
  filter(PositionFormat%in%c("Decimal Minutes"))%>%
  mutate(Latitude=measurements::conv_unit(.$Latitude, from = 'deg_dec_min', to = 'dec_deg'))%>%
  mutate(Longitude=measurements::conv_unit(.$Longitude, from = 'deg_dec_min', to = 'dec_deg'))%>%
  glimpse()

dd <- fisheries.returns%>%
  filter(PositionFormat%in%c("Decimal Degrees"))%>%
  glimpse()

fisheries.returns <- bind_rows(dd, dm)%>%
  mutate(Latitude=as.numeric(Latitude))%>%
  mutate(Longitude=as.numeric(Longitude))%>%
  mutate(row=1:nrow(.))%>%
  mutate(Sample=paste(row,Tag.number,sep="."))%>%
  glimpse()

names(fisheries.returns)

metadata.fisheries<-fisheries.returns%>%
  select(Source,Sample,Latitude,Longitude,Fisher,Fisher.Email,Date,Depth.fms,Exclude.Pots)

length.fisheries<-fisheries.returns%>%
  mutate(Recapture=as.character(Recapture))%>%
  select(Source,Sample,Tag.number,Sex,Colour,Carapace.length,Recapture,Setose.state,Tarspot,Individual.Remarks,Retention.Status,Dead)

#Import Tag Return data sent to UWA----
fisher.returns <- gs_title("UWA.Tag.Returns")%>%
  gs_read_csv(ws="Sheet1")%>%
  mutate(Recapture=TRUE)%>%
  mutate(Source = "fisher-returns")%>%
  mutate(Sex=str_replace_all(.$Sex, c("M"="Male", "F"="Female","U"="Unknown")))%>%
  mutate(Colour=str_replace_all(.$Colour, c("W"="White", "R"="Red")))%>%
  dplyr::rename(Fisher= Reporter, Latitude=`Latitude(deg_dec_min)`, Longitude=`Longitude(deg_dec_min)`,Setose.state=Setose,Fisher.Email=Email,Pot.Remarks=Comments,Retention.Status=RetentionStatus)%>%
  dplyr::mutate(Date=as_date(dmy(Date)))%>%
  mutate(Latitude=measurements::conv_unit(.$Latitude, from = 'deg_dec_min', to = 'dec_deg'))%>%
  mutate(Longitude=measurements::conv_unit(.$Longitude, from = 'deg_dec_min', to = 'dec_deg'))%>%
  mutate(Latitude=as.numeric(Latitude))%>%
  mutate(Longitude=as.numeric(Longitude))%>%
  mutate(row=1:nrow(.))%>%
  mutate(Sample=paste(row,Tag.number,sep="."))%>%
  glimpse()

names(fisher.returns)

metadata.fisher<-fisher.returns%>%
  select(Source,Sample,Date,Latitude,Longitude,Fisher,Fisher.Email,Pot.Remarks)

length.fisher<-fisher.returns%>%
  mutate(Recapture=as.character(Recapture))%>%
  select(Source,Sample,Tag.number,Carapace.length,Sex,Colour,Setose.state,Retention.Status,Recapture)

# Clean up enviroment
rm(dd,dm,fisher.returns,fisheries.returns)

# Tidy names of data frames ----
names(length.2017)<-capitalise(names(length.2017))
names(length.2018)<-capitalise(names(length.2018))
names(length.fisheries)<-capitalise(names(length.fisheries))
names(length.fisher)<-capitalise(names(length.fisher))

names(metadata.2017)<-capitalise(names(metadata.2017))
names(metadata.2018)<-capitalise(names(metadata.2018))
names(metadata.fisheries)<-capitalise(names(metadata.fisheries))
names(metadata.fisher)<-capitalise(names(metadata.fisher))

# Combine data ----
metadata<-bind_rows(metadata.2017,metadata.2018,metadata.fisheries,metadata.fisher)%>%
  glimpse()
  
length<-bind_rows(length.2017,length.2018,length.fisheries,length.fisher)%>%
  replace_na(list(Damage.old.a = 0, Damage.old.l = 0,Damage.new.a = 0, Damage.new.l = 0,Sex="Unknown",Colour="Unknown"))%>%
  mutate(Total.damage=(Damage.old.a+Damage.old.l+Damage.new.a+Damage.new.l))%>%
  mutate(Count=1)%>% # Count for Abundance
  glimpse()

unique(metadata$Date)

# Write data
setwd(data.dir)
dir()

write.csv(metadata, "metadata.csv",row.names = FALSE)
write.csv(length, "length.csv",row.names = FALSE)