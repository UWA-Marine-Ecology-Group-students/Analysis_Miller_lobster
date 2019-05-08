# Explore catch data----
rm(list=ls()) # Clears memory

# librarys----
library(tidyr)
library(dplyr)
library(googlesheets)
library(stringr)
library(lubridate)

# Study name----
study<-"Lobster.Data"

# Set work directory----

#For Desktop
# work.dir=("C:/Users/00097191/Google Drive/MEG/Projects/Projects_WRL/Project_WRL_low-catch zone/Fieldwork and Reporting/03_Trapping/Analysis_WRL_Reds_2018")

#For Laptop
# setwd("~/Google Drive/Analysis_WRL_Reds_2018/Data")
# setwd(work.dir)
# dir()

#For Tims Github 

work.dir=("~/GitHub/Analysis_Miller_WRL") #for Tim's github

work.dir=("~/workspace/Analysis_Miller_WRL") #for ecocloud server

## Sub directories ----
data.dir<-paste(work.dir,"Data",sep="/")
map.dir<-paste(work.dir,"Map Layers",sep="/")
plots.dir<-paste(work.dir,"Plots",sep="/")

# Functions----
se <- function(x) sd(x) / sqrt(length(x))
se.min <- function(x) (mean(x)) - se(x)
se.max <- function(x) (mean(x)) + se(x)
sd.min <- function(x) (mean(x)) - sd(x)
sd.max <- function(x) (mean(x)) + sd(x)
scaleFUN <- function(x) sprintf("%.0f", x)

# Import and make data----

# # For Rstudio Server
options(httr_oob_default=TRUE)
# options(httr_oob_default=FALSE) #for desktop
# gs_auth(new_user = TRUE) #only run once

# Import 2018 length data----

dat.length<-gs_title("Lobsters_data_2018_All")%>% # To use GoogleSheets
  gs_read_csv(ws = "Lobster.var" )%>%
  glimpse()

dat.length.1<-dat.length%>%
  mutate(Count=1)%>%        #Count for Abundance
  mutate(Carapace.length=as.numeric(as.character(Carapace.length)))%>%
  mutate(trip.day.trap=paste(Trip,Day,Trap.ID,sep="."))%>%  #Trap.ID
  mutate(Colour=str_replace_all(.$Colour,c("W"="White", "R"="Red")))%>%
  mutate(Sex=str_replace_all(.$Sex, c("M"="Male", "F"="Female")))%>%
  mutate(ID=1:nrow(.))%>%
  group_by(ID)%>%
  replace_na(list(Damage.old.a = 0, Damage.old.L = 0,Damage.new.a = 0, Damage.new.L = 0))%>%
  dplyr::mutate(Total.damage=sum(Damage.old.a+Damage.old.L+Damage.new.a+Damage.new.L,na.rm = TRUE))%>%
  glimpse()


unique(dat.length$Remarks)
length(unique(dat.length.1$Tag.number))
#7537
length(dat.length.1$Carapace.length)
#9614

# # Import 2018 pot data----

dat.pot.1<-gs_title("Lobsters_data_2018_All")%>% # To use GoogleSheets
  gs_read_csv(ws = "Pot.var")%>%
  mutate(trip.day.trap=paste(Trip,Day,Trap.ID,sep="."))%>% 
  mutate(Site.Name=str_replace_all(.$Site.Name,c( "SM"="Seven Mile", "DM"="Davids Marks",  "RM"="Rivermouth", "IR"="Irwin Reef", "LR"="Long Reef", "SD"="South Dummy", "LH"="Little Horseshoe", "CHin1_"="Cliff Head Mid","CHin2_"="Cliff Head South","CHout1_" = "Cliff Head OUT1","CHout2_" = "Cliff Head North", "JB"="Jim Bailey", "GR"="Golden Ridge", "SR"="South Rig", "WL"="Whites Lump")))%>% 
  filter(Johns=="No")%>% # turn off when I add in john's data (if ever)
  dplyr::rename(Latitude=Latitude.y, Longitude=Longitude.x)%>%
  mutate(Latitude=as.numeric(Latitude))%>%
  mutate(Longitude=as.numeric(Longitude))%>%
  #mutate(Remarks=NA)%>%
  select(Trip, Site.Name, Pot.Number, Trap.ID, Day, Date.Recovered, Longitude, Latitude, trip.day.trap)%>%
  glimpse()


#Create "sites" for 2018 Data----
sites<-dat.pot.1%>%
  distinct(Trap.ID,Site.Name)%>% #Keeps only distinct rows (Trap.ID & Site.Name)
  mutate(Trap.ID=as.character(Trap.ID))%>%
  dplyr::rename(Site=Site.Name)%>%
  glimpse()

#Add a "Site" column by Trap.ID to dat.length----
dat.length.1<-left_join(dat.length.1,sites, by="Trap.ID") 

#Check for missing sites: ones in dat.length but not in dat.pot
#missing.site.1<-anti_join(dat.length.1,sites)  # 0

#Join Lobster and pot 2018 data----

data.oct <- dat.length.1%>%
  dplyr::filter(Remarks%in%c("PWO (1)", "1 X CARDinAL PWO (1)","Dead (octopus)","PWO(X)" ,"PWO(X), WKW (4)", "PWO(1), Crab (1)",  "PWO(1), Daed crab (1)","Dead recapture (by occy) PWO (X)","Dead-Occy","Occy","PWO(!)","Dead-by Occy, Had cliiped pleopod but no tag, Occy (x)","(x) occy, (1) wkw", "Still alive, Occy (1)","Occy (X)", "Dead by occy, PWO(X)" ,"PWO(x), Dead by occy" ,"PWO(x)", "PWO(1), Dead-by occy", "PWO(x), 2 tails" , "PWO (1), Dead-by Occy"  ,"Dead-by occy, PWO(X), (2) WKW","Dead-occy" ,"PWO(X), Dead-by occy" ,"DEAD-by occy, PWO(2)" , "DEAD -by occy ,PWO(1), WKW (1)", "PWO(X), DEAD-by occy", "PWO(1), wkw(1)" , "Dead-by occy, infected tag", "PWO (X), Dead-by occy", "DEAD-BY OCCY, PWO(X), WKW(3)", "DEAD- OCCY",  "Dead-occy, wkw (2)", "Dead-occy, wkw (2)" ,  "(2) PWO", "Dead- by Occy" ,   "PWO(1), Dead- by occy", "PWO(X), Dead- by occy", "DEAD- BY OCCY, PWO (X)", "Dead by occy, PWO (X)" , "Dead- by occy","By Occy" , "Dead-by occy, (1) PWO, (1) WKW" ,"Dead-by occy, (1) PWO", "Dead- Occy", "Half dead (octopus)", "PWO(1)",  "Dead, PWO(X)", "Dead (octopus), PWO (1)" ,  "PWO (X)","MAYBE OCCY?","WOBBY 0.8 M","1.5 m wobby"))%>%
  ungroup()%>%
  select(trip.day.trap)%>%
  glimpse()

length(unique(data.oct$trip.day.trap))


#Filter out Octopus----
dat.1 <- dplyr::left_join(dat.length.1, dat.pot.1,by = c("trip.day.trap"))%>%
  ungroup()%>%
  select(trip.day.trap,  Date, Tag.number, Recapture,  Carapace.length, Sex, Colour, Site, Longitude, Latitude, Remarks)%>%
  mutate(Location=str_replace_all(Site,c("Seven Mile Beach"= "Seven Mile","Little Horseshoe"="Cliff Head", "Cliff Head North"="Cliff Head","Cliff Head Mid"= "Cliff Head","Cliff Head South"="Cliff Head","Cliff Head OUT1"= "Cliff Head","CHM"="Cliff Head", "Davids Marks"="Cliff Head","CHM"= "Cliff Head", "CHS"="Cliff Head", "CHN"="Cliff Head", "Jim Bailey"="Irwin Reef", "Long Reef"="Irwin Reef", "South Dummy"="Irwin Reef","South Rig"= "Irwin Reef","Whites Lump"= "Irwin Reef","WP"= "Irwin Reef","Whitepoint"="Irwin Reef")))%>%
  mutate(Site=str_replace_all(.$Site, c("Jim Bailey"="White Point" ,"WP"="White Point" , "Whitepoint"="White Point" , "CHS"="Cliff Head South","CHM"="Cliff Head Mid","CHN"="Cliff Head North", "Seven Mile"="Seven Mile.out")))%>%
  anti_join(data.oct)%>%
  filter(!Location=="Rivermouth")%>%
  mutate(Count=ifelse(is.na(Carapace.length),0,1))%>%
  glimpse()


#Import 2017 length data----

dat.length.2<-gs_title("Lobsters_data_20180214")%>% # To use GoogleSheets
  gs_read_csv(ws = "Lobster.var" )%>%
  mutate(Count=1)%>%        #Count for Abundance
  mutate(Carapace.length=as.numeric(as.character(Carapace.length)))%>%
  mutate(Recapture=NA)%>%
  mutate(Trip=0)%>%
  mutate(trip.day.trap=paste(Trip,Day,Trap.number,sep="."))%>%  #New column for Day and trap.number
  select(Trip, Day, Trap.number, Carapace.length, Sex, Colour, Tag.number, Recapture, trip.day.trap)%>%
  glimpse()


#Import 2017 pot data----

dat.pot.2<-gs_title("Lobsters_data_20180214")%>% # To use GoogleSheets
  gs_read_csv(ws = "Pot.var")%>%
  mutate(Trip=0)%>%
  mutate(trip.day.trap=paste(Trip, Day,Trap.number,sep="."))%>% 
  select(Trip, Day, Date, Trap.number, Location, Number,Longitude.original, Latitude.original, trip.day.trap)%>%
  glimpse()


#Create "sites" for 2017 Data----
sites.2<-dat.pot.2%>%
  distinct(trip.day.trap,Location)%>% #Keeps only distinct rows (Trap.ID & Site.Name)
  dplyr::rename(Site=Location)%>%
  glimpse()


#Add a "Site" column by Trap.ID to dat.length----
glimpse(sites.2)

dat.all.2<-left_join(dat.length.2,sites.2, by="trip.day.trap")%>%
  glimpse()

#Check for missing sites: ones in dat.length but not in dat.pot
missing.site.2<-anti_join(dat.length.2,sites.2)  # 0

#Join 2017 Lobster and pot data----
glimpse(dat.length.2)

dat.2 <- dplyr::left_join(dat.all.2, dat.pot.2)%>%
  dplyr::rename(Longitude=Longitude.original, Latitude=Latitude.original, Trap.ID=Trap.number, Trap.Number=Number)%>% 
  mutate(Latitude=as.numeric(Latitude))%>%
  mutate(Longitude=as.numeric(Longitude))%>%
  select(trip.day.trap,Date,Tag.number, Recapture,Carapace.length,Sex, Colour,Site, Longitude, Latitude)%>%
  glimpse()

dat.2%<>%
  mutate(Total.damage="0")%>%
  mutate(Remarks=NA)%>%
  mutate(Location="Seven Mile")%>%
  mutate(Count=1)%>%
  select(trip.day.trap,Date,Tag.number, Recapture,Carapace.length,Sex, Colour,Site, Longitude, Latitude, Remarks,Location,Count)%>%
  glimpse()

#Check data that's in dat.length.all but not in dat.pot
#missing.dat.2 <-anti_join(dat.length.2, dat.pot.2) #0!

#Combine 2017 and 2018 data ----
glimpse(dat.1)
glimpse(dat.2)


dat.all <- bind_rows(dat.1, dat.2)%>%
  dplyr::mutate(Date=as_date(dmy(Date)))%>%
  glimpse()


#FIND SUM PER POT----
glimpse(dat.all)

sum.dat <-dat.all%>%
  group_by(trip.day.trap)%>%
  dplyr::summarise(Count=sum(Count))%>%
  ungroup()%>%
  distinct()%>%
  glimpse()

#Hmmm..
glimpse(dat.all)

dat.location <- dat.all%>%
  select(Date, trip.day.trap, Location, Site, Longitude, Latitude)%>%
  distinct()%>%
  glimpse()

#JOIN BACK WITH DATA: Trap.Id, Sum per pot, Location, Site----

sum.per.pot <- left_join(sum.dat, dat.location, by="trip.day.trap")%>%
  glimpse()

#Bring in Seven Mile Data----

dat.sm <- gs_title("SMB")%>% 
  gs_read_csv(ws = "Sheet1")%>%
  mutate(Date= as.Date(Date, format= "%d/%m/%Y"))%>%
  select(Date, Location, Site, num, Latitude, Longitude, trap.id)%>%
  dplyr::rename("Count"="num")%>%
  dplyr::rename("trip.day.trap"="trap.id")%>%
  glimpse()

glimpse(sum.per.pot)
glimpse(dat.sm)


#COMBINE SEVEN MILE WITH OTHER DATA----

dat.sum.pot <- rbind(sum.per.pot, dat.sm)%>%
  glimpse()

setwd(data.dir)
write.csv(dat.sum.pot, "Catch.Per.Pot.csv", row.names = F)


#IMPORT DAT AND COMBINE WITH SWELL AND SST DATA!----
catch.per.pot <- read_csv("Catch.Per.Pot.csv")%>%
  glimpse()



#Bring in swell data for 2018----

dat.swell.18 <-gs_title("JDW2018")%>%
  gs_read_csv(ws="Sheet1", header=TRUE)%>%
  mutate(Date=as.Date(Date,format= "%d/%m/%Y"))%>%
  fill(2:12, .direction = c("down"))%>% #Some data is missing from certain days. k. cool. whatever.
  group_by(Date) %>%
  summarise_all(funs(mean))%>% #Find average per day
  distinct()%>%
  dplyr::rename("Hs.m.sw"="Hs(m).sw",
                "Hs.m.sea"="Hs(m).sea",
                "T1.s.sw"="T1(s).sw",
                "T1.s.sea"="T1(s).sea")%>%
  select(Date, Hs.m.sw, Hs.m.sea, T1.s.sw, T1.s.sea)%>%
  ungroup()%>%
  # distinct()%>%
  # ungroup()%>%
  # semi_join(dat.date)%>%
  glimpse()

#2017 data----
#Damn. Forgot about that. Added as sheet 2 to 2018 data. 

dat.swell.17 <-gs_title("JDW2018")%>%
  gs_read_csv(ws="Sheet2", header=TRUE)%>%
  mutate(Date=as.Date(Date,format= "%d/%m/%Y"))%>%
  #mutate(Date= lubridate::ymd_hm(Date))%>%
  fill(2:12, .direction = c("down"))%>%
  group_by(Date) %>%
  summarise_all(funs(mean))%>%
  distinct()%>%
  dplyr::rename("Hs.m.sw"="Hs(m).sw",
                "Hs.m.sea"="Hs(m).sea",
                "T1.s.sw"="T1(s).sw",
                "T1.s.sea"="T1(s).sea")%>%
  select(Date, Hs.m.sw, Hs.m.sea, T1.s.sw, T1.s.sea)%>%
  ungroup()%>%
  glimpse()

#combine 2017 & 2018 swell data----  
dat.swell<- rbind(dat.swell.17, dat.swell.18)%>%
  glimpse()


setwd(data.dir)
write.csv(dat.swell, "dat.swell.csv", row.names = F)

#Join Count data (catch.per.pot) with Swell data (dat.swell)----
glimpse(catch.per.pot)
catch.sw <- left_join(catch.per.pot, dat.swell, by="Date")%>%
  glimpse()

#SAVE CATCH PER POT AND SWELL DATA----
setwd(data.dir)
write.csv(catch.sw, "Catch.Swell.csv", row.names = F)

#ALRIGHT! Let's bring in SST data for out Dates!!!!!!! - Use Mathew's script function thang----
#Wait maybe in another script? 






















# # Make factors----
# dat.factors<-dat.all%>%
#   select(trip.day.trap,Date,Site,Longitude,Latitude,Location)%>% #Tim thinks there is 1 sample mismatch here? WARNING - there is a trip trap somethine that has non unique
#   distinct()%>%
#   glimpse()

# dat.date<-read_csv("dat.count.csv")%>% #BE WARNED!!! THERE are non unique trip.day.trap
#   select(Date)%>%
#   distinct()%>%
#   glimpse()

# dat.id<-read_csv("dat.count.csv")%>% #BE WARNED!!! THERE are non unique trip.day.trap
#   select(trip.day.trap)%>%
#   distinct()%>%
#   glimpse()

# #Bring in SST data (Thanks Matt)----
# 
# dat.sst<-gs_title("dat.all.sst")%>% # To use GoogleSheets
#   gs_read_csv(ws = "Sheet1", row.names=FALSE)%>%
#   select(trip.day.trap,sst)%>%
#   distinct()%>%
#   glimpse()
# 
# 
# #Combine with dat.all.sst----
# glimpse(dat)
# glimpse(dat.swell)
# glimpse(dat.sst)
# 
# dat.sw <- inner_join(dat, dat.swell,by = "Date")%>%
#   semi_join(dat.id)%>%
#   glimpse()
# 
# dat.sw.sst <- inner_join(dat.sw, dat.sst,by = "trip.day.trap")%>% #used more conservative join to fix probs
#   semi_join(dat.id)%>%
#   semi_join(dat.date)%>%
#   # Make long format
#   mutate(Sum.all=Sum.m+Sum.f)%>%
#   # mutate(males=Sum.m,females=Sum.f)%>%
#   
#   gather(key=sex,value=response,Sum.m,Sum.f,Sum.all)%>%
#   glimpse()
# 
# # Write data----
# setwd(data.dir)
# write_csv(dat.sw.sst,"dat.sw.sst.csv")
# 
# # Validate----
# 
# ggplot(dat,aes(x=Date,y=Sum.m,colour=Location))+
#   geom_point()
# 
# length(unique(dat.1$trip.day.trap))
# 1438-82
# 
# unique(dat.1$Location)
# unique(dat.1$Site)
# 
# table(dat.1$Location,dat.1$Site)

