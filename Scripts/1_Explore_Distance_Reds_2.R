# Explore catch data----
rm(list=ls()) #clear memory

# librarys----
library(tidyr)
library(dplyr)
options(dplyr.width = Inf) #enables head() to display all coloums
library(forcats)
library(readxl)
library(googlesheets)
library(stringr)
library(ggmap)
library(ggplot2)
library(cowplot)
library(argosfilter) 


# Study name----
study<-"Reds.Distance"

# Set work directory----
work.dir=("~/Google Drive/Projects/Project_WRL low-catch zone/Fieldwork and Reporting/03_Trapping/Analysis_WRL_Reds_2018")

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
setwd("~/Google Drive/Analysis_WRL_Reds_2018/Data")
setwd(work.dir)
dir()

# Import length data----

dat.length<-gs_title("Lobsters_data_2018_Reds_20180511")%>% # To use GoogleSheets
  gs_read_csv(ws = "Lobster.var" )%>%
  glimpse()

dat.length<-dat.length%>%
  mutate(Count=1)%>%
  filter(!is.na(Carapace.length))%>%
  mutate(Carapace.length=as.numeric(as.character(Carapace.length)))%>%
  mutate(trip.day.trap=paste(Trip, Day, Trap.Number, sep = "."))%>% # Creates new column with Trip, day & trap number combined
  glimpse()

write.csv(dat.length,"lob.dat.csv")
dat.length<-read.csv("lob.dat.csv")


unique(dat.length$Trap.ID) # 365 levels
dat.length$Trap.ID



# # Import pot data----
dat.pot<-gs_title("Lobsters_data_2018_Reds_20180511")%>% # To use GoogleSheets
  gs_read_csv(ws = "Pot.var")%>%
  mutate(trip.day.trap=paste(Trip, Day, Pot.Number, sep = "."))%>% # Creates new column with Trip, day & trap number combined
  mutate(Site.Name=str_replace_all(.$Site.Name,c( "SM"="Seven Mile", "DM"="Davids Marks",  "RM"="Rivermouth", "IR"="Irwin Reef", "LR"="Long Reef", "SD"="South Dummy", "LH"="Little Horseshoe", "CHin1_"="Cliff Head Mid","CHin2_"="Cliff Head South","CHout1_" = "Cliff Head OUT1","CHout2_" = "Cliff Head North", "JB"="Jim Bailey", "GR"="Golden Ridge", "SR"="South Rig", "WL"="Whites Lump")))%>% 
  filter(Johns=="No")%>% # turn off if you add in john's data
  glimpse()

dat.pot$Site.Name
write.csv(dat.pot,"dat.pot.csv")
dat.pot<-read.csv("dat.pot.csv")
unique(dat.pot$Trap.ID)  # 400 levels
dat.pot$trip.day.trap


#Create "sites" for Reds----

sites<-dat.pot%>%
  distinct(Trap.ID,Site.Name)%>% #Keeps only distinct rows (Trap.ID & Site.Name)
  mutate(Trap.ID=as.character(Trap.ID))%>%
  dplyr::rename(Site=Site.Name)%>%
  glimpse()

#Add a column of "sites" by Trap.ID to dat.length----

dat.length<-left_join(dat.length,sites, by="Trap.ID")

View(dat.length)

#Total tagged----

total.tagged<-dat.length%>%         #Total tagged at each site
  filter(!is.na(Tag.number))%>%
  group_by(Site)%>%
  dplyr::summarise(Total=n())

View(total.tagged)


#Add lat and long 

#dat.pot.dis <- subset(dat.pot, select=c("")

dat.length.dis <- left_join(dat.length, dat.pot, by="Trap.ID")

dat.length.dis1 <- subset(dat.length.dis, select = c("Trip.x","Tag.number", "Date", "Recapture","Site", "Site.Name", "trip.day.trap" ))



View(dat.length.dis)
# Recaptures----

total.recap.dis<-dat.length%>%
  left_join(.,sites)%>%
  distinct(Site, Tag.number, Trip)%>%
  group_by(Site, Tag.number)%>%
  dplyr::summarise(min=min(Trip), max=max(Trip), no.trips=n())%>%
  filter(max>min)%>%
  filter(!is.na(Tag.number))
  
View(total.recap.dis)

#OR
# Don't need this one 
total.recap<-dat.length%>%
  left_join(.,sites)%>%
  distinct(Site,Tag.number,Trip)%>%
  group_by(Site,Tag.number)%>%
  dplyr::summarise(min=min(Trip),max=max(Trip),no.trips=n())%>%
  filter(max>min)%>%                      #Filter out recaptures from within the same trip
  filter(!is.na(Tag.number))%>%
  ungroup()%>%
  group_by(Site)%>%
  dplyr::summarise(Total.recaptured=n())

### To make 'wide' dataset (columns = month, each row lobster measurements)----

almost.true.recaps<-dat.length%>% # caught between different months
  left_join(.,sites)%>%
  distinct(Site,Tag.number,Trip,Date)%>%
  group_by(Site,Tag.number)%>%
  dplyr::summarise(min=min(Trip),max=max(Trip),no.trips=n())%>%
  filter(max>min)%>%
  select(Tag.number)

#604 individuals
# tag no. 191410 caught twice in same trip

#doubles<-wide%>%
#  group_by(Trip,Tag.number)%>%
#  summarise(n=n())%>%
#  filter(n>1)

#names(wide)

# Good example tag = 191033


true.recaps<-semi_join(dat.length,almost.true.recaps)%>%
  group_by(Tag.number,Trip)%>%
  slice(which.min(Day))%>% # chooses first day, can change to which.max(carapace.length) or whatever
  left_join(dat.length)

#1315 individuals

dates<-gs_title("Dates")%>% # To use GoogleSheets
  gs_read_csv(ws = "Sheet1")%>%
  separate(Date,into=c("m","d","y"))%>%
  mutate(Date=paste(d,m,y,sep="/"))%>%
  select(-c(m,d,y))%>%
  #mutate(Date.t=as.Date(Date, format="%m/%d/%y"))%>%
  glimpse()


###Need to add lat and long to data frame? 
###Maybe initial tag location and the last tag location for ones caught multi times?










# Calculate the distance from boat ramp for each sample----
# Ramps
ids <- factor(c("Exmouth","Bundegi","Onslow","Pilbara","Dampier","WithnellBay","BreadonCreek","Tantabiddi","Coral Bay","Fortescue River", "Warroora", "Gnaraloo")) 
ramps <- data.frame(ids = rep(ids, each = 1),
  
  y = c(-21.97970240551842,
        -21.84371102054585,
        -21.69503107602818,
        -21.0840122708165,
        -20.66562571673585,
        -20.539744728225,
        -21.648725,
        -21.912580,
        -23.155828,
        -21.028040)
  #         -23.485963,
  #         -23.873282)
  ,x = c(114.1461956058358,
         114.1882002543887,
         114.9237761395207,
         115.9306982155436,
         116.6746382564552,
         116.7980700539128,
         115.131243,
         113.978251,
         113.767124,
         116.029232))
#                          113.772874,
#                          113.497430))
head(ramps,15)

head(bruv.combined)
distance.to.ramp<-bruv.combined%>%
  select(OpCode,Latitude,Longitude)%>%
  mutate(To.Exmouth=distance(lat1=ramps[1,2],lat2=.$Latitude,lon1=ramps[1,3],lon2=.$Longitude))%>%
  mutate(To.Bundegi=distance(lat1=ramps[2,2],lat2=.$Latitude,lon1=ramps[2,3],lon2=.$Longitude))%>%
  mutate(To.Onslow=distance(lat1=ramps[3,2],lat2=.$Latitude,lon1=ramps[3,3],lon2=.$Longitude))%>%
  mutate(To.Pilbara=distance(lat1=ramps[4,2],lat2=.$Latitude,lon1=ramps[4,3],lon2=.$Longitude))%>%
  mutate(To.Dampier=distance(lat1=ramps[5,2],lat2=.$Latitude,lon1=ramps[5,3],lon2=.$Longitude))%>%
  mutate(To.WithnellBay=distance(lat1=ramps[6,2],lat2=.$Latitude,lon1=ramps[6,3],lon2=.$Longitude))%>%
  mutate(To.BreadonCreek=distance(lat1=ramps[7,2],lat2=.$Latitude,lon1=ramps[7,3],lon2=.$Longitude))%>%
  mutate(To.Tantabiddi=distance(lat1=ramps[8,2],lat2=.$Latitude,lon1=ramps[8,3],lon2=.$Longitude))%>%
  mutate(To.CoralBay=distance(lat1=ramps[9,2],lat2=.$Latitude,lon1=ramps[9,3],lon2=.$Longitude))%>%
  mutate(To.Fortescue=distance(lat1=ramps[10,2],lat2=.$Latitude,lon1=ramps[10,3],lon2=.$Longitude))%>%
  #   mutate(To.Warroora=distance(lat1=ramps[11,2],lat2=.$Latitude,lon1=ramps[11,3],lon2=.$Longitude))%>%
  #   mutate(To.Gnaraloo=distance(lat1=ramps[12,2],lat2=.$Latitude,lon1=ramps[12,3],lon2=.$Longitude))%>%
  mutate(Distance.to.ramp=do.call(pmin, .[,4:13]))%>%
  select(OpCode,Distance.to.ramp)%>%
  distinct() #need to be distinct otherwise joins dont work
head(distance.to.ramp)
names((distance.to.ramp))
