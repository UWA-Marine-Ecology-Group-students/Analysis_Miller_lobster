# Explore catch data----

rm(list=ls()) #clear memory

# librarys----
library(tidyr)
library(dplyr)
library(googlesheets)
library(stringr)
library(ggplot2)
library(magrittr)
library(readr)

# Study name----
study<-"Growth.Rate"

# Set work directory----

#work.dir=("C:/Users/00097191/Google Drive/MEG/Projects/Projects_WRL/Project_WRL_low-catch zone/Fieldwork and Reporting/03_Trapping/Analysis_WRL_Reds_2018") 
# setwd("~/Documents/University/Masters Project/Plots/Plot per recapture")

work.dir=("~/GitHub/Analysis_Miller_WRL") #for Tim's github
work.dir=("~/workspace/Analysis_Miller_WRL") #for ecocloud server

# Set sub-directories----
data.dir=paste(work.dir,"Data",sep="/")
plot.dir=paste(work.dir,"Plots",sep="/")


#### Import data from googlesheets ----

# # For Rstudio Server
options(httr_oob_default=TRUE)
# options(httr_oob_default=FALSE) #for desktop
# gs_auth(new_user = TRUE) #only run once

Dat.Combined<-gs_title("Lobster_Data_All_Combined")%>% 
  gs_read_csv(ws = "Sheet1" )%>%
  filter(!is.na(Tag.number))%>% #Filter out individuals with no tag.no
  filter(Tag.number!="CT")%>%   #Filter out weird 'CT' tags from Oscars data  
  filter(!is.na(Carapace.length))%>% #Filter out individuals with no length measurements
  mutate(mini.site=Site)%>% #Create mini.site= Later this will be 'site'
  mutate(Site=str_replace_all(.$Site,c("Seven Mile Beach"= "Seven Mile","Little Horseshoe"="Cliff Head", "Cliff Head North"="Cliff Head","Cliff Head Mid"= "Cliff Head","Cliff Head South"="Cliff Head","Cliff Head OUT1"= "Cliff Head","CHM"="Cliff Head", "Davids Marks"="Cliff Head","CHM"= "Cliff Head", "CHS"="Cliff Head", "CHN"="Cliff Head", "Jim Bailey"="Irwin Reef", "Long Reef"="Irwin Reef", "South Dummy"="Irwin Reef","South Rig"= "Irwin Reef","Whites Lump"= "Irwin Reef","WP"= "Irwin Reef","Whitepoint"="Irwin Reef")))%>% 
  mutate(mini.site=str_replace_all(.$mini.site, c("Jim Bailey"="White Point" ,"WP"="White Point" , "Whitepoint"="White Point" , "CHS"="Cliff Head South","CHM"="Cliff Head Mid","CHN"="Cliff Head North", "Seven Mile Beach"= "Seven Mile.out")))%>%
  glimpse()


 
# #Test----
# test <- Dat.Combined%>%
#   filter(Source!="Oscar" & Source!="Fisheries" & Source!="UWA")%>%
#   glimpse()
# 
# length(unique(test$Tag.number))
# length(test$Tag.number)
# 
# test<- test%>%
#   arrange(Date)%>%
#   glimpse()
# 
# #Releases
# test.initial <- test%>%
#   dplyr::distinct(Tag.number,.keep_all = TRUE)%>% #keeps only the first tag cl, filters out the duplicates (recaptues)
#   glimpse()
# 
# test.initial%>%         #Total tagged at each site
#   group_by(Trip)%>%
#   dplyr::summarise(Total=n())%>%
#   glimpse()
# 
# 
# trip<-test.initial%>%
#   filter(Trip=="8")%>%
#   glimpse()
# 
# #Recaptures
# 
# test.recap <- test[duplicated(test$Tag.number), ]%>%
#   glimpse()
# 
# test.rr<- right_join(test.initial, test.recap, by= "Tag.number")%>%
#   glimpse()
# 
# test.rr%>%
#   filter(Trip.x=="8")%>%
#   group_by(Trip.y)%>%
#   dplyr::summarise(Total=n())%>%
#   glimpse()

#filter out Bruce Cockmans data, Rive mouth Data & obvious outliers----

Dat.Combined <- Dat.Combined%>%  
  dplyr::filter(is.na(Fisher) | Fisher!="Bruce Cockman")%>%
  dplyr::filter(Tag.number!="K2400"&Tag.number!="K1617"&Tag.number!="K1221"&Tag.number!="K0653" & Tag.number!="K0457" & Tag.number!="K1045"& Tag.number!="K0755")%>% 
  #Obvious fisher return measures (more than -10 growth) & Oscars recaptures
  dplyr::filter(Tag.number!="198428" & Tag.number!="196072" & Tag.number!="K4501")%>% #Outliers from Cliff Head 
  dplyr::filter(Tag.number!="198821" & Tag.number!="K2519" & Tag.number!="K2402")%>% #Outliers from Irwin Reef
  dplyr::filter(Tag.number!="K3805")%>% 
  dplyr:: filter(is.na(Site)| Site!="Rivermouth")%>% 
  glimpse()

  
#Bring in Ben's Seven Mile data----

dat.smb <- gs_title("Lobster_Data_Fisheries_SMB")%>%
  gs_read_csv("Dat.smb")%>%
  mutate(Tag.number=as.character(Tag.number))%>%
  mutate(Site=str_replace_all(.$Site,c("Seven Mile Beach"="Seven Mile")))%>%
  filter(Tag.number!="190428" & Tag.number!="190188" & Tag.number!="190124" &Tag.number!="190443")%>% #four tags have more than -7 growth
  mutate(Trip=paste("T",Trip,sep=""))%>%
  mutate(mini.site="Seven Mile.in")%>%
  glimpse()


#Create another Dataframe for SM with Jan-April removed----
dat.smb.edit<-dat.smb%>%
     mutate(month=format(as.Date(Date),'%m'))%>%
     mutate(month=month((as_date(Date))))%>%
     filter(month%in%c(5:12))%>% #Remove Jan-April
     select(Date, Tag.number, Carapace.length, Sex, Total.damage, Trip, Source, Fisher, Colour, Recapture, Longitude, Latitude, Site, mini.site)%>%
     glimpse()


#Combine Dat.combined and dat.smb----
# 
# dat.mine <- Dat.Combined%>%
#   mutate(Trip=paste("T",Trip,sep=""))%>%
#   select(Trip, Date,Tag.number, Carapace.length, Site, Sex, Colour, Total.damage, mini.site, Longitude, Latitude)%>% 
#   glimpse()
# 
# dat.ben <-dat.smb%>%
#   select(Trip, Date,Tag.number, Carapace.length, Site, Sex, Colour, Total.damage, mini.site, Longitude, Latitude)%>% 
#   glimpse()
# 
# lobster.data <-rbind(dat.mine, dat.ben)%>%
#   glimpse()
# 
# setwd(data.dir)
# write.csv(lobster.data,"lobster.data.csv",row.names = F)

# Recaptures----
#Total Tagged per site----

# total.tagged<-Dat.Combined%>%         #Total tagged at each site
#   group_by(Site)%>%
#   dplyr::summarise(Total=n())%>%
#   glimpse()
# 
# #Number of unique tags
# length(unique(Dat.Combined$Tag.number))
# #9148
# 
# #Total at Seven Mile from Ben
# tt.smb<- dat.smb%>%
#   dplyr::summarise(Total=n())%>%
#   glimpse()
# 
# length(unique(dat.smb$Tag.number))
# #379
# 
# #Total from SM month 5->12
# tt.smb.edit<- dat.smb.edit%>%
#   dplyr::summarise(Total=n())%>%
#   glimpse()
# 
# length((unique(dat.smb.edit$Tag.number)))
# #253

#Below gives recaps that were tagged and caught at the SAME site
# total.recap <-Dat.Combined%>%
#   distinct(Site,Tag.number,Trip)%>%
#   group_by(Tag.number,Site)%>% #
#   dplyr::summarise(min=min(Trip),max=max(Trip),no.trips=n())%>%
#   filter(max>min)%>%     #Filter out recaptures from within the same trip
#   ungroup()%>%
#   group_by(Site)%>%
#   dplyr::summarise(Total.recaptured=n())%>%
#   glimpse()

### new recaptured ----

# recap.tag<-Dat.Combined%>% 
#   distinct(Site,Tag.number,Trip,Date)%>% #Keep only four columns
#   group_by(Tag.number)%>% #Site removed
#   dplyr::summarise(min=min(Trip),max=max(Trip),no.trips=n())%>%
#   filter(max>min)%>% #Filters out non-recaptures (Keeps only those caught in different trips)
#   select(Tag.number)%>%
#   glimpse()
# 
# All.recaps<-semi_join(Dat.Combined,recap.tag)%>% 
#   group_by(Tag.number,Trip)%>%
#   glimpse()

#OR include all recaptures: including from same trip)
recaps<- Dat.Combined%>%
  select(Tag.number)%>%
  glimpse()

recaps<-recaps%>%
  mutate(duplicates = duplicated(recaps) | duplicated(recaps, fromLast = TRUE))%>%
  filter(duplicates=="TRUE")%>%
  glimpse()

All.recaps.1<-semi_join(Dat.Combined,recaps)%>% 
  group_by(Tag.number,Trip)%>%
  glimpse()


#Number of recaptures per site----
# All.recaps%>%
#   group_by(Site)%>%
#   dplyr::summarise(Total=n())%>%
#   glimpse()
# 
# All.recaps.1%>%
#   group_by(Site)%>%
#   dplyr::summarise(Total=n())%>%
#   glimpse()

# Create a dataframe for all recaptures---- 

# growth.1<-All.recaps%>%
#   select(Trip, Date,Tag.number, Carapace.length, Site, Sex, Colour, Total.damage, mini.site, Longitude, Latitude)%>% 
#   group_by(Tag.number)%>%
#   mutate(Trip=paste("T",Trip,sep=""))%>%
#   glimpse()

#For seven mile data----

# growth.2<-dat.smb%>%
#   select(Trip,Date,Tag.number, Carapace.length, Site, Sex, Colour, Total.damage, Longitude, Latitude)%>% 
#   group_by(Tag.number)%>%
#   mutate(Trip=paste("T",Trip,sep=""))%>%
#   mutate(mini.site="Seven Mile")%>%
#   glimpse()

#For Seven Mile without months 1->4----

growth.3<-dat.smb.edit%>%
  select(Trip,Date,Tag.number, Carapace.length, Site, Sex, Colour, Total.damage, Longitude, Latitude, mini.site)%>% 
  group_by(Tag.number)%>%
  mutate(Trip=paste("T",Trip,sep=""))%>%
  glimpse()

#For data including recaps from same trip----
growth.4<-All.recaps.1%>%
  select(Trip, Date,Tag.number, Carapace.length, Site, Sex, Colour, Total.damage, mini.site, Longitude, Latitude)%>% 
  group_by(Tag.number)%>%
  mutate(Trip=paste("T",Trip,sep=""))%>%
  glimpse()

# # #Combine growth.1 and growth.2 dataframes
# growth.comb <- rbind(growth.2, growth.4)%>%
#   glimpse()
# 
# setwd(data.dir)
# write.csv(growth.comb, "growth.comb.csv", row.names = F)
# #And for growth data without month 1-4
recapture.data<- rbind(growth.3, growth.4)%>%
  glimpse()

# growth.comb.edit <- rbind(growth.1, growth.3)

#All recaptures----

#Save data for growth Models
# growth.all <- rbind(growth.2, growth.4)
# setwd(data.dir)
# write.csv(growth.all, "growth.all.csv")
# 
# #Save for sm no jan- april
# growth.edited <- rbind(growth.3, growth.4)
# setwd(data.dir)
# write.csv(growth.edited, "growth.edited.csv")

#Growth data #1-----
#Create dataframe for initial captures only
# initial.dat <- growth.1%>%
#   select(Date, Tag.number, Carapace.length, Site, Sex, Colour,Trip, Total.damage, mini.site, Longitude, Latitude)%>%
#   arrange(Date)%>% #Order by date
#   dplyr::distinct(Tag.number,.keep_all = TRUE)%>% #keeps only the first tag cl, filters out the duplicates (recaptues)
#   rename(initial.cl= Carapace.length)%>%
#   glimpse()
# 
# 
# length(unique(initial.dat$Tag.number))
# #946
# 
# # #Create dataframe for recaptures only
# recap.dat <- growth.1[duplicated(growth.1$Tag.number), ]%>%
#   arrange(Date)%>%
#   select(Date, Tag.number, Carapace.length, Site, Sex, Colour,Trip, Total.damage, mini.site, Longitude, Latitude)%>%
#   rename(recap.Date = Date)%>%
#   rename(recap.cl=Carapace.length)%>%
#   glimpse()
# 
# 
# length(recap.dat$Tag.number)
# #1,265 recaptured
# 
# r<-recap.dat%>%
#   group_by(Site)%>%
#   dplyr::summarise(Total=n())%>%
#   glimpse()
# 
# 
# #Combine Initial captures and recaptures
# 
# release.recap.1 <- left_join(initial.dat, recap.dat, by="Tag.number")%>%
#   glimpse()

#  #SEVEN MILE DATA-----
# #Create dataframe for initial captures only
# growth.2<- growth.2%>%
#   arrange(Date)%>%
#   glimpse()
# 
# smb.initial <- growth.2%>%
#   select(Date, Tag.number, Carapace.length, Site, Sex, Colour,Trip,Total.damage, mini.site, Longitude, Latitude)%>%
#   #arrange(Date)%>% #Order by date
#   dplyr::distinct(Tag.number,.keep_all = TRUE)%>% #keeps only the first tag cl, filters out the duplicates (recaptues)
#   dplyr::rename(initial.cl=Carapace.length)%>%
#   glimpse()
# 
# # #Create dataframe for recaptures only
# 
# smb.recap <- growth.2[duplicated(growth.2$Tag.number), ]%>%
#   select(Date, Tag.number, Carapace.length, Site, Sex, Colour,Trip, Total.damage, mini.site, Longitude, Latitude)%>%
#   dplyr::rename(recap.Date = Date)%>%
#   dplyr::rename(recap.cl=Carapace.length)%>%
#   glimpse()
# 
# length(smb.recap$Tag.number)
# #682
# #Combine Initial captures and recaptures
# release.recap.2 <-left_join(smb.initial, smb.recap, by="Tag.number")%>%
#   glimpse()

#For Edited SM data (months 1-4 removed)----
#Order by earliest date to latest
growth.3<- growth.3%>%
  arrange(Date)%>%
  glimpse()

smb.initial.edit <- growth.3%>%
  select(Date, Tag.number, Carapace.length, Site, Sex, Colour,Trip,Total.damage, mini.site, Longitude, Latitude)%>%
  #arrange(Date)%>% #Order by date
  dplyr::distinct(Tag.number,.keep_all = TRUE)%>% #keeps only the first tag cl, filters out the duplicates (recaptues)
  dplyr::rename(initial.cl= Carapace.length)%>%
  glimpse()

# #Create dataframe for recaptures only
glimpse(growth.3)

smb.recap.edit <- growth.3[duplicated(growth.3$Tag.number), ]%>%
  select(Date, Tag.number, Carapace.length, Site, Sex, Colour,Trip, Total.damage, mini.site, Longitude, Latitude)%>%
  dplyr::rename(recap.Date = Date)%>%
  dplyr::rename(recap.cl=Carapace.length)%>%
  glimpse()


length(smb.recap.edit$Tag.number)
#206
#Combine Initial captures and recaptures
#Not left join as now not all are recaptures cause we removed Jan-April
release.recap.3 <-inner_join(smb.initial.edit, smb.recap.edit, by="Tag.number")%>% 
  glimpse()


#Growth data #4 (All recaptures)-----
#Create dataframe for initial captures only
glimpse(growth.4)
2901-2838 # >2 removes 63 
2901-2860 # >3 removes 41
  
#Filter out damaged individuals
growth.4<- growth.4%>%
  filter(!Total.damage>2)%>%
  glimpse()


initial.4 <- growth.4%>%
  select(Date, Tag.number, Carapace.length, Site, Sex, Colour,Trip, Total.damage, mini.site, Longitude, Latitude)%>%
  arrange(Date)%>% #Order by date
  dplyr::distinct(Tag.number,.keep_all = TRUE)%>% #keeps only the first tag cl, filters out the duplicates (recaptues)
  dplyr::rename(initial.cl= Carapace.length)%>%
  glimpse()


length(unique(initial.4$Tag.number))
#1284

# #Create dataframe for recaptures only
recap.4 <- growth.4[duplicated(growth.4$Tag.number), ]%>%
  arrange(Date)%>%
  select(Date, Tag.number, Carapace.length, Site, Sex, Colour,Trip, Total.damage, mini.site, Longitude, Latitude)%>%
  dplyr::rename(recap.Date = Date)%>%
  dplyr::rename(recap.cl=Carapace.length)%>%
  glimpse()

length(recap.4$Tag.number)
#1554 recaptured

r<-recap.4%>%
  group_by(Site)%>%
  dplyr::summarise(Total=n())%>%
  glimpse()


#Combine Initial captures and recaptures
release.recap.4 <- left_join(initial.4, recap.4, by="Tag.number")%>%
  glimpse()

#1585

#filter out individuals that have moved sites
# 33 individuals in total that have moved Location (not neccarily site)

release.recap.4 <- release.recap.4%>%
  dplyr::filter(!Tag.number %in% c("K1054", "K1565", "K1604", "191162", "K2485", "K2446", "195768", "195655", "K2934", "195997", "195997", "195256", "195958", "195985", "196281", "K4249","K4248", "K4955", "197205", "198251"))%>%
  glimpse()

#Combine my data with SM data----
# 
# dat.rr <- bind_rows(release.recap.1, release.recap.2)%>% #Combine Ash's data with Ben's data
#   dplyr::rename(Location.int = Site.x, Sex.int = Sex.x, Colour.int=Colour.x, Trip.int= Trip.x, Total.damage.int=Total.damage.x)%>%
#   dplyr::rename(Location.rec = Site.y, Sex.rec = Sex.y, Colour.rec=Colour.y, Trip.rec= Trip.y, Total.damage.rec=Total.damage.y)%>%
#   glimpse()
# 
# 
# # #Save data with SMB to use for Fabians Models
# setwd(data.dir)
# write.csv(dat.rr, "dat.rr.csv")

#Combine my data with Edited SM data----

# dat.rr.edit <- bind_rows(release.recap.1, release.recap.3)%>% #Combine Ash's data with Ben's data
#   dplyr::rename(Location.int = Site.x, Sex.int = Sex.x, Colour.int=Colour.x, Trip.int= Trip.x, Total.damage.int=Total.damage.x)%>%
#   dplyr::rename(Location.rec = Site.y, Sex.rec = Sex.y, Colour.rec=Colour.y, Trip.rec= Trip.y, Total.damage.rec=Total.damage.y)%>%
#   glimpse()

# #Save data with SMB to use for Fabians Models
# setwd(data.dir)
# write.csv(dat.rr.edit, "dat.rr.new.csv")

#Combine my data with SM data----

# dat.rr.all <- bind_rows(release.recap.4, release.recap.2)%>% #Combine Ash's data with Ben's data
#   dplyr::rename(Location.int = Site.x, Sex.int = Sex.x, Colour.int=Colour.x, Trip.int= Trip.x, Total.damage.int=Total.damage.x)%>%
#   dplyr::rename(Location.rec = Site.y, Sex.rec = Sex.y, Colour.rec=Colour.y, Trip.rec= Trip.y, Total.damage.rec=Total.damage.y)%>%
#   dplyr::rename(Lat1=Latitude.x, Lon1=Longitude.x, lat2=Latitude.y, Lon2=Longitude.y)%>%
#   glimpse()
# 
# setwd(data.dir)
# write.csv(dat.rr.all, "dat.rr.all.csv")

#Combine my data with Edited SM data----

dat.rr.clean <- bind_rows(release.recap.4, release.recap.3)%>% #Combine Ash's edited data with Ben's data no jan-apr
  dplyr::rename(Location.int = Site.x, Sex.int = Sex.x, Colour.int=Colour.x, Trip.int= Trip.x, Total.damage.int=Total.damage.x)%>%
  dplyr::rename(Location.rec = Site.y, Sex.rec = Sex.y, Colour.rec=Colour.y, Trip.rec= Trip.y, Total.damage.rec=Total.damage.y)%>%
  glimpse()

setwd(data.dir)
write.csv(dat.rr.clean, "dat.rr.clean.csv", row.names = F)

#Rename release recap
# rr<- dat.rr
# glimpse(rr)

# #Determine potential individuals that have moulted----
# # rr1<-rr%>%
# #   mutate(inc=recap.cl-initial.cl)%>%
# #   dplyr::mutate(diff=recap.Date-Date)%>%
# #   mutate(Moulted=ifelse(Colour.rec==Colour.int,"FALSE", "TRUE"))%>%
# #   mutate(Moulted=ifelse((((Colour.rec%in%c(NA))&(Colour.int%in%c("Red"))&(recap.Date>2018-12-01))|(!Colour.rec==Colour.int)),"TRUE","FALSE"))%>%
# #   mutate(Moulted=ifelse(((Colour.int=="White")&(is.na(Colour.rec))),"FALSE",Moulted))%>%
# #   glimpse()
# # 
# # length(rr1$Tag.number) #1268 or 1956 with Seven Mile
# # length(rr1$Moulted[rr1$Moulted=="TRUE"]) #138 or 826 with Seven Mile
# # 
# # View(rr1)
# 
# # Plotting Themes ----
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    text=element_text(size=10),
    strip.text.y = element_text(size = 10,angle = 270),
    axis.title.x=element_text(vjust=0.3, size=10),
    axis.title.y=element_text(vjust=0.6, angle=90, size=10),
    axis.text.x=element_text(size=10,colour="black"),
    axis.text.y=element_text(size=10,colour="black"),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.ticks.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank(),
    strip.text.x = element_text(size = 10),
    plot.background = element_blank()) # Brooke added


#Recapture plots----
glimpse(recapture.data)
recapture.data<-recapture.data%>%
  filter(!Sex=="U")%>%
  glimpse()

#Need a way to get recapture data by location they were originally tagged at. 
catch.plot <- ggplot(data=recapture.data, aes(Carapace.length))+
  #geom_freqpoly(binwidth=2)+
  # geom_line(aes(y = Count, colour = Sex))+
  #geom_histogram(binwidth = 2, color="black", fill="gray")+
  geom_histogram(binwidth=2, aes(colour=Sex, fill=Sex), alpha=0.4)+
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+
  Theme1+
  coord_cartesian(xlim = c(40, 105))+
  scale_x_continuous(breaks= c(40, 50, 60, 70, 80, 90, 100))+
  ylab("Frequency")+
  xlab("Carapace length")+
  theme(strip.text = element_text(size=12, face="plain"))+
  facet_wrap(~Site)
catch.plot



# 
# #Simons plot
# 
# with(rr, plot(Initial.cl,inc, pch=16, cex=.6))
# 
# filter(rr1,Trip.x=="T7")%>%
#   glimpse()
# 
# 
# glimpse(rr1)
# 
# # Simons's filters----
# rr1 %<>% 
#   # mutate(month=as.numeric(format(as.Date(Date),'%m')), year=as.numeric(format(as.Date(Date),'%Y')))%>%
#   filter(!Trip.x=="T0")%>%
#   filter(!Trip.y=="T9")%>%
#   mutate(Dec.tagg=ifelse(`Trip.x`%in%c("T7"),"late.nov",ifelse(`Trip.x`%in%c("T8"),"dec","reds")))%>%
#   mutate(Damage=ifelse(`Total.damage.x`==0,"none","some"))%>%
#   mutate(count=1)%>%
#   
#   glimpse()
# 
# glimpse(rr1)         
# 
#          # , col=Moulted
# increase.plot <- ggplot(data=rr1%>%filter(Dec.tagg=="reds"&!Site.x=="Seven Mile"), aes(x=inc/Initial.cl), pch=16, cex=.6)+
# 
# # increase.plot <- ggplot(data=rr1, aes(x=inc/Initial.cl), pch=16, cex=.6)+
#   # geom_density(stat="identity",alpha=0.5)+
#   # geom_density(alpha=0.5)+
#   geom_histogram()+
#   scale_color_manual(values=c("TRUE" = "red", "FALSE"= "black"))+ #, "FALSE" = "black", "NA" = "black"
#   Theme1+
#   # ylab("Growth (mm) before recapture")+
#   # xlab("Initial carapace length (mm)")+
#   xlim(0,0.2)+
#   facet_grid(Damage~Site.x,scales="free") # , scales = "free"
# increase.plot
# 
# 
# #Icorporate Time- Tims plot----
# 
# Time.plot <- ggplot(data=rr1, aes(x=diff, y=inc, col=Moulted), notch= FALSE, position=dodge1, outlier.shape=NA)+
#   #geom_smooth(method='lm', se=F, size=0.7)+
#   geom_point()+
#   scale_color_manual(values = c("TRUE"="red", "FALSE"= "black"))+
#   Theme1+
#   ylab("Growth (mm) before Recapture")+
#   xlab("Time between Recaptures (Days)")+
#   facet_wrap(. ~ Site.x, scales = "free") #
# Time.plot
#   
# 
# 
# 
