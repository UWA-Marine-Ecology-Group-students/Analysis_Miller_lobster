# Clean catch data----
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
  dplyr::select(Source,Trip,Sample,Tag.number,Recapture,Carapace.length,Sex,Colour,Setose.state,Egg.stage,Moult.stage,Damage.old.a,Damage.old.L,Damage.new.a,Damage.new.L,Dead,Outlier,Individual.Remarks)%>%
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
length(unique(length.2018$Sample)) # 1163 pots with crays

# Things that should actually be in the metadata not the lobster data
info.2018<-dat.2018%>%
  dplyr::select(Sample,Day.Pull,Pot.Remarks,PWO,WKW,PWF)%>% # Trap.Number, Date
  distinct()%>%
  glimpse()

length(unique(info.2018$Sample)) # 1440 total pot lifts

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
unique(metadata.2018$Site)

unique(metadata.2018$Date)

unique(metadata.2018$Location)%>%sort() # "Cliff Head"   "Golden Ridge" "Irwin Reef"   "Rivermouth"   "Seven Mile"  
unique(metadata.2018$Site)%>%sort() 
unique(metadata.2018$Site.Code)%>%sort()

length(unique(metadata.2018$Sample)) # 1461 (1163 non-empty, 298 empty)

duplicate.pots<-metadata.2018%>%
  group_by(Sample)%>%
  dplyr::summarise(n=n())%>%filter(n>1) # No longer any duplicates

# Clean up environment
rm(info.2018,duplicate.pots,duplicate.remarks,dat.2018)

# Import 2017 length data----
# Using original 2017 Data -----
dat.2017<-gs_title("1_Lobsters_data_171210.xlsx")%>% # To use GoogleSheets
  gs_read_csv(ws = "Sheet1",col_types = "nccccnccccccccnnnnccccc")%>% 
  mutate(Trap.ID=str_replace_all(.$Trap.number,c("CH6F6"="CH6C6")))%>%
  mutate(Pot.type=ifelse(Trap.ID%in%c("CH6C6"),"C",Pot.type))%>%
  mutate(Sample=paste(Day,Trap.ID,sep="."))%>%  #New column for Day and trap.number
  mutate(Source='oscar-doncel-canons-masters')%>%
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
  mutate(Exclude.pots=ga.capitalise(Exclude.pots))%>%
  mutate(Site=str_replace_all(.$Names.to.display,c("7 Mile"="Seven Mile Mid")))%>%
  mutate(Site=str_replace_all(.$Site, c("Cliff Head Sand Strips"="Cliff Head","Cliff Head"="Cliff Head North","South Whites Lump"= "White Point", "White Point Reef"="White Point", "South West Dummy"="South Dummy", "Big Horseshoe"="Little Horseshoe", "Sand Hole"="Little Horseshoe", "Darwin"="White Point", "Dry Shallows"="Little Horseshoe", "Power Pack"="Little Horseshoe", "Outside CH1"="Kevin Healy", "Outside CH2"="Cliff Head North")))%>%
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
unique(metadata.2018$Site)

# Import Tag Return data sent to Fisheries----
fisheries.returns <- gs_title("Fisheries.Tag.Returns")%>%
  gs_read_csv(ws="Sheet1")%>%
  mutate(Recapture=TRUE)%>%
  mutate(Colour="Unknown")%>%
  mutate(Source= "fisheries-returns")%>%
  mutate(Sex=str_replace_all(.$Sex, c("M"="Male", "F"="Female")))%>%
  dplyr::rename(Tag.number=SpeciesTag, Carapace.length=SpeciesSize, Date=ReportCreatedDate, Fisher=ReportersUserName)%>%
  mutate(Setose.state=str_replace_all(.$SpeciesName,c("Western rock lobster - "="","male"="","Western rock lobster"="","Western rock lobster- "="","- "="","setose no tarspot"="Mature","setose with tarspot"="Mature","non setose"="Immature")))%>%
  mutate(Tarspot=ifelse(SpeciesName%in%c("Western rock lobster - setose with tarspot"),TRUE,NA))%>%
  mutate(Retention.Status=ifelse(RetentionStatus%in%c("KEPT","kept"),"Retained",RetentionStatus))%>%
  replace_na(list(Sex="Unknown"))%>%
  mutate(Dead=ifelse(Comments%in%c("Eaten by octopus"),"Dead",NA))%>%
  dplyr::rename(Individual.Remarks=Comments,Fisher.Email=ReportersUserEmail,Depth.fms=depth.fms)%>%
  dplyr::select(-c(ReportId,SpeciesName,SpeciesCategory,ReportersPhoneNumber,ReportersBoatRegistrationNumber,address,X19,X20,X21,X22,RetentionStatus))%>%
  dplyr::mutate(Date=lubridate::as_date(mdy(Date)))%>%
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
  select(Source,Sample,Latitude,Longitude,Fisher,Fisher.Email,Date,Depth.fms,Exclude.Pots,Pot.Remarks)

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
  mutate(Retention.Status=str_replace_all(.$Retention.Status, c("Returned"="Released")))%>%
  glimpse()

names(fisher.returns)

metadata.fisher<-fisher.returns%>%
  select(Source,Sample,Date,Latitude,Longitude,Fisher,Fisher.Email,Pot.Remarks)

length.fisher<-fisher.returns%>%
  mutate(Recapture=as.character(Recapture))%>%
  select(Source,Sample,Tag.number,Carapace.length,Sex,Colour,Setose.state,Retention.Status,Recapture)

# Clean up enviroment
rm(dd,dm,fisher.returns,fisheries.returns)

# Seven Mile Data ----
# sevenmile <- gs_title("Lobster_Data_Fisheries_SMB")%>%
#   gs_read_csv("Dat.smb",col_types = "nnnnnccncnnccccnnc")%>%
#   mutate(Source="ben-seven-mile")%>%
#   mutate(Sample=as.character(Pot.ID))%>%
#   mutate(Tag.number=as.character(Tag.number))%>%
#   mutate(Location=str_replace_all(.$Site,c("Seven Mile Beach"="Seven Mile")))%>%
#   mutate(Outlier=ifelse(Tag.number%in%c("190428","190188","190124","190443"),"y",NA))%>% # four tags have more than -7 growth
#   mutate(Site="Seven Mile.in")%>%
#   mutate(month=format(as.Date(Date),'%m'))%>%
#   mutate(month=month((as_date(Date))))%>%
#   filter(month%in%c(5:12))%>% # Remove Jan-April
#   dplyr::mutate(Date=lubridate::as_date(ymd(Date)))%>%
#   glimpse()

#Ash's edit-to bring in new SM data- Need Brooke to double check
sevenmile<-gs_title("Lobster_Data_Fisheries_SMB_All")%>%
  gs_read_csv("SMB2",col_types = "cccnnnnnccnnnnnnncccnnnnnnnn")%>%
  mutate(Source="ben-seven-mile")%>%
  mutate(Location="Seven Mile")%>%
  mutate(Site="Seven Mile North")%>%
  mutate(Trip="10")%>%
  mutate(Trip=as.numeric(Trip))%>%
  mutate(Sample=as.character(POT_TYPE_ID))%>%
  dplyr::rename(Tag.number=VTAGNO)%>%
  mutate(Tag.number=as.character(Tag.number))%>%
  mutate(Tag.number=str_replace_all(.$Tag.number,c("V"="")))%>% #Removes V
  dplyr::filter(m%in%c(5:12))%>% #filter Jan-April
  mutate(Date=lubridate::ymd(sprintf('%04d%02d%02d',y,m,d)))%>% #Make date col
  mutate(Outlier=ifelse(Tag.number%in%c("190428","190188","190124","190443"),"y",NA))%>%
  dplyr::rename(Recapture= REC, Sex=SEX, Colour=COLOUR, Carapace.length=CLENGTH, Setose.state= SETOSE, Individual.Remarks=REMARKS, Damage.new.a=NEW_ANT, Damage.new.L=NEW_LEGS, Damage.old.a=OLD_ANT, Damage.old.L=OLD_LEGS,Pot.Number=POT_NO, Pot.Type=POT_TYPE_ID, Day.Pull=DAY_PULL)%>%
  mutate(Damage.old.a=if_else(Damage.new.a==9,0,Damage.old.a))%>% # Fix Damaged data
  mutate(Damage.new.L=if_else(Damage.new.a==9,0,Damage.new.L))%>%
  mutate(Damage.old.L=if_else(Damage.new.a==9,0,Damage.old.L))%>%
  mutate(Damage.new.a=if_else(Damage.new.a==9,0,Damage.new.a))%>%
  mutate(Colour=str_replace_all(.$Colour,c("W"="White", "R"="Red")))%>%
  mutate(Sex=str_replace_all(.$Sex, c("M"="Male", "F"="Female","U"="Unknown")))%>%
  mutate(Sex=if_else((!is.na(Colour)&!Sex%in%c("Female","Male")),"Unknown",Sex))%>%
  mutate(Recapture=str_replace_all(.$Recapture, c("1"= "TRUE")))%>%
  filter(!is.na(Sample))%>%
  glimpse()
 
#Need to do:
#Fix remarks

names(sevenmile)

metadata.sevenmile<-sevenmile%>%
  distinct(Source,Trip,Sample,Date,Longitude,Latitude,Location,Site,PWO,WKW,PWF,Pot.Remarks)%>%
  mutate(PWO=as.character(PWO))

length.sevenmile<-sevenmile%>%
  select(Source,Trip,Sample,Tag.number,Carapace.length,Sex,Colour,Recapture,Damage.new.a, Damage.new.L, Damage.old.a, Damage.old.L, Outlier)%>%
  filter(!is.na(Carapace.length)&!is.na(Sex))%>%
  glimpse()

 
# Tidy names of data frames ----
names(length.2017)<-ga.capitalise(names(length.2017))
names(length.2018)<-ga.capitalise(names(length.2018))
names(length.fisheries)<-ga.capitalise(names(length.fisheries))
names(length.fisher)<-ga.capitalise(names(length.fisher))
names(length.sevenmile)<-ga.capitalise(names(length.sevenmile))


names(metadata.2017)<-ga.capitalise(names(metadata.2017))
names(metadata.2018)<-ga.capitalise(names(metadata.2018))
names(metadata.fisheries)<-ga.capitalise(names(metadata.fisheries))
names(metadata.fisher)<-ga.capitalise(names(metadata.fisher))
names(metadata.sevenmile)<-ga.capitalise(names(metadata.sevenmile))

# Combine data ----

metadata<-bind_rows(metadata.2017,metadata.2018,metadata.fisheries,metadata.fisher,metadata.sevenmile)%>%
  replace_na(list(Exclude.pots="No"))%>%
  glimpse()

length<-bind_rows(length.2017,length.2018,length.fisheries,length.fisher,length.sevenmile)%>%
  replace_na(list(Damage.old.a = 0, Damage.old.l = 0,Damage.new.a = 0, Damage.new.l = 0))%>%
  mutate(Total.damage=(Damage.old.a+Damage.old.l+Damage.new.a+Damage.new.l))%>%
  replace_na(list(Sex="Unknown",Colour="Unknown",Cable.tie="FALSE",Dead="Alive",Retention.status="Released"))%>%
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
unique(length$Tarspot)
unique(length$Retention.status) # "Released" "Retained"
unique(length$Tarspot)

# Remove lobsters that have changed sex
changed.sex<-length%>%
  filter(!is.na(Tag.number))%>%
  group_by(Tag.number)%>%
  summarise(no.sex=length(unique(Sex)),no.times.caught=n())%>%
  filter(no.sex>1)

cant.keep<-changed.sex%>%
  filter(no.times.caught==2)%>%
  distinct(Tag.number)
#35

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

