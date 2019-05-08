# Explore catch data----
rm(list=ls()) #clear memory

# librarys----
library(tidyr)
library(dplyr)
library(googlesheets)
library(stringr)
library(scales)
library(ggplot2)
library(tidyverse)
library(magrittr)
library(lubridate)

# Study name----
study<-"Growth.Rate.All"

# Set work directory----

#work.dir=("C:/Users/00097191/Google Drive/MEG/Projects/Projects_WRL/Project_WRL_low-catch zone/Fieldwork and Reporting/03_Trapping/Analysis_WRL_Reds_2018") 

# setwd("~/Documents/University/Masters Project/Plots/Plot per recapture")

work.dir=("~/GitHub/Analysis_Miller_WRL") #for Tim's github

work.dir=("~/workspace/Analysis_Miller_WRL") #for ecocloud server


# Set sub-directories----
data.dir=paste(work.dir,"Data",sep="/")
plot.dir=paste(work.dir,"Plots",sep="/")


#### Trips All (Reds & White) ----

# Import length data from googlesheet----

# # For Rstudio Server
options(httr_oob_default=TRUE)
# options(httr_oob_default=FALSE) #for desktop
# gs_auth(new_user = TRUE) #only run once

dat.length.all<-gs_title("Lobsters_data_2018_All")%>% 
  gs_read_csv(ws = "Lobster.var" )%>%
  glimpse()

dat.length.all<-dat.length.all%>%
  mutate(Count=1)%>%
  #filter(!is.na(Carapace.length))%>% Turn on if you want to filter out NA cl and tag.no
  #filter(!is.na(Tag.number))%>%
  mutate(Colour=str_replace_all(.$Colour,c("W"="White", "R"="Red")))%>%
  mutate(Sex=str_replace_all(.$Sex, c("M"="Male", "F"="Female")))%>%
  mutate(Carapace.length=as.numeric(as.character(Carapace.length)))%>%
  mutate(trip.day.trap=paste(Trip,Day,Trap.Number,sep="."))%>%
  #mutate(Date=as.Date(Date,"%d/%m/%y"))%>% #Turn on if you want date in 'date' format
  glimpse()



#Replace NA in outlier with 'n'
dat.length.all$Outlier[is.na(dat.length.all$Outlier)] <- 'n'

# Write data
#setwd(data.dir)
#write.csv(dat.length.all,"lob.dat.all.csv")
#dat.length.all<-read.csv("lob.dat.all.csv")



# Checks ---
summary(dat.length.all$Carapace.length) #195143 CL entered as 9.8mm, need to find in raw data and fix
length(dat.length.all$Carapace.length) #9601 individual length measurements
length(dat.length.all$Tag.number)
length(unique(dat.length.all$trip.day.trap)) #1382
unique(dat.length.all$Trap.ID) 
dat.length.all$Trap.ID

# # Import pot data----

dat.pot.all<-gs_title("Lobsters_data_2018_All")%>% 
  gs_read_csv(ws = "Pot.var")%>%
  mutate(trip.day.trap=paste(Trip,Day,Pot.Number,sep="."))%>%
  mutate(Site.Name=str_replace_all(.$Site.Name,c( "SM"="Seven Mile", "DM"="Davids Marks",  "RM"="Rivermouth", "IR"="Irwin Reef", "LR"="Long Reef", "SD"="South Dummy", "LH"="Little Horseshoe", "CHin1_"="Cliff Head Mid","CHin2_"="Cliff Head South","CHout1_" = "Cliff Head OUT1","CHout2_" = "Cliff Head North", "CHN"="Cliff Head North", "CHM"="Cliff Head Mid", "CHS"="Cliff Head South", "JB"="Jim Bailey", "GR"="Golden Ridge", "SR"="South Rig", "WL"="Whites Lump")))%>% 
  filter(Johns=="No")%>% # turn off  if you add in john's data
  glimpse()


dat.pot.all$Site.Name


#write.csv(dat.pot.all,"dat.pot.all.csv")
#dat.pot<-read.csv("dat.pot.all.csv")
unique(dat.pot.all$Trap.ID)  

# Checks----
unique(dat.pot.all$Trap.ID)
unique(dat.pot.all$trip.day.trap)
length(unique(dat.pot.all$trip.day.trap)) #1392

#Create "sites" for Reds----

sites<-dat.pot.all%>%
  distinct(Trap.ID,Site.Name)%>% #Keeps only distinct rows (Trap.ID & Site.Name)
  mutate(Trap.ID=as.character(Trap.ID))%>%
  dplyr::rename(Site=Site.Name)%>%
  glimpse()

#Add a column of "Site" by Trap.ID to dat.length----
#Reds

dat.length.all<-left_join(dat.length.all,sites, by="Trap.ID") 

#Check for missing sites: ones in dat.length but not in dat.pot
missing.site<-anti_join(dat.length.all,sites) 

# 0 missing in dat.pot (yay)

# Checks---
unique(dat.pot.all$trip.day.trap)
length(unique(dat.pot.all$trip.day.trap)) # 1392

# Recaptures----

#Total recaptured per site----
total.recap<-dat.length.all%>%
  left_join(.,sites)%>%
  distinct(Site,Tag.number,Trip)%>%
  group_by(Site,Tag.number)%>%
  dplyr::summarise(min=min(Trip),max=max(Trip),no.trips=n())%>%
  filter(max>min)%>%     #Filter out recaptures from within the same trip
  filter(!is.na(Tag.number))%>% #Filter out individuals with no tag.no
  ungroup()%>%
  group_by(Site)%>%
  dplyr::summarise(Total.recaptured=n())%>%
  glimpse()

total.tagged<-dat.length.all%>%         #Total tagged at each site
  filter(!is.na(Tag.number))%>%
  group_by(Site)%>%
  dplyr::summarise(Total=n())

### new recaptured ----

names(dat.length.all) #Recapture


recaps.site.tag<-dat.length.all%>% 
  left_join(.,sites)%>%        #Add 'Site' column  
  distinct(Site,Tag.number,Trip,Date)%>% #Keep only four columns
  group_by(Tag.number)%>% #Site removed
  dplyr::summarise(min=min(Trip),max=max(Trip),no.trips=n())%>%
  filter(max>min)%>% #Filters out non-recaptures (Keeps only those caught in different trips)
  select(Tag.number)


All.recaps<-semi_join(dat.length.all,recaps.site.tag)%>% 
  group_by(Tag.number,Trip)%>%
  filter(!is.na(Tag.number))%>%  #Filters out NA tags
  left_join(dat.length.all)

#Individuals caught twice in same sampling trip
doubles<-growth%>%
  group_by(Trip,Tag.number)%>%
  summarise(n=n())%>%
  filter(n>1)


# Create a dataframe for all recaptures---- 

growth<-semi_join(dat.length.all,All.recaps)%>%
  select(Date,Tag.number,Trip,Carapace.length)%>% #Maybe need site
  mutate(Trip=paste("T",Trip,sep=""))%>%
  group_by(Tag.number)%>%
  filter(!is.na(Tag.number))%>%
  glimpse()

#Create dataframe to use for growth curve----

#Create dataframe for initial captures only

release.dat <- growth%>%
  dplyr::distinct(Tag.number,.keep_all = TRUE)%>% #keeps only the first tag cl, filters out the duplicates (recaptues)
  select(Date, Tag.number, Carapace.length)%>%
  rename(Initial.cl= Carapace.length)

#Create dataframe for recaptures only

recap.dat <- growth[duplicated(growth$Tag.number), ]%>%
  select(Date, Tag.number, Carapace.length)%>%
  rename(Recap.Date = Date)%>%
  rename(Recap.cl=Carapace.length)

#Join two data frames together 

release.recap <- left_join(release.dat, recap.dat)

#Simons plot
rr<- release.recap
rr%<>%mutate(inc=Recap.cl-Initial.cl)
with(rr, plot(Initial.cl,inc, pch=16, cex=.6))

#Tims plot
rr2<-rr%>%
  dplyr::mutate(Date=as_date(dmy(Date)))%>%
  dplyr::mutate(Recap.Date=as_date(dmy(Recap.Date)))%>%
  dplyr::mutate(diff=Recap.Date-Date)%>%
  glimpse()

ggplot(data=rr2, aes(x=diff, y=inc))+
  geom_smooth(method='lm', se=F)+
  geom_point()

#Add column for propotion of year

dates<-gs_title("Dates")%>% # To use GoogleSheets, 
  gs_read_csv(ws = "Sheet1")%>%
  separate(Date,into=c("m","d","y"))%>%
  mutate(Date=paste(d,m,y,sep="/"))%>%
  select(-c(m,d,y))%>%
  #mutate(Date.t=as.Date(Date, format="%m/%d/%y"))%>%
  glimpse()

#Combine with data

#final.dat <- left_join(release.recap, dates) #nope
