# Explore catch data----
rm(list=ls()) # Clears memory

# librarys----
library(tidyr)
library(dplyr)
library(googlesheets)
library(stringr)
library(measurements)
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



# Plotting Themes ----
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=12),
    text=element_text(size=12),
    strip.text.y = element_text(size = 12,angle = 270),
    axis.title.x=element_text(vjust=0.3, size=12),
    axis.title.y=element_text(vjust=0.6, angle=90, size=12),
    axis.text.x=element_text(size=10,colour="black"),
    axis.text.y=element_text(size=10,colour="black"),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.ticks.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank(),
    plot.background = element_blank()) # Brooke added

#Plot----
glimpse(dat)

#For Males
count.plot <- ggplot(data=dat, aes(x=Location, y=Sum.m))+
  geom_boxplot()+
  geom_jitter(width = 0.1, height=NULL, alpha=0.2)+
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ #this is adding the dot for the mean
  Theme1+
  ylab("Catch rate per pot")+
  xlab("Location")
count.plot

#For Females
count.plot <- ggplot(data=dat.all, aes(x=Location, y=Sum.f))+
  geom_boxplot()+
  geom_jitter(width = 0.1, height=NULL, alpha=0.2)+
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ #this is adding the dot for the mean
  Theme1+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"))+
  ylab("Catch rate per pot")+
  xlab("Location")
count.plot

#Be cool to have average catch per pot for each month/location
glimpse(dat)

#Want to average catch per day, or maybe per month. i.e. sampling trip?


av.dat <- dat%>%
  mutate(Date=factor(Date))%>%
  select(Date, Location, Sum.m, Sum.f)%>%
  group_by(Location, Date)%>%
  summarise_all(funs(mean))%>%
  ungroup()%>%
  glimpse()

#Average Males
ggplot(av.dat,aes(x=Date,y=Sum.m, colour=Location))+
  geom_bar(stat="identity", aes(colour=Location, fill=Location))+
  theme_bw()+Theme1+
  theme(axis.text.x = element_text(angle=90, size=10))+
  ylab("Average Males caught per pot")

#Average Females
ggplot(av.dat,aes(x=Date,y=Sum.f, colour=Location))+
  geom_bar(stat="identity", aes(colour=Location, fill=Location))+
  theme_bw()+Theme1+
  theme(axis.text.x = element_text(angle=90, size=10))+
  ylab("Average females caught per pot")

#Or size class..hmm
glimpse(dat)

legal.dat <- dat%>%
  mutate(sum = rowSums(.[7:8]))%>%
  glimpse()


catch.plot <- ggplot(data=catch.data, aes(Carapace.length))+
  #geom_freqpoly(binwidth=2)+
  # geom_line(aes(y = Count, colour = Sex))+
  #geom_histogram(binwidth = 2, color="black", fill="gray")+
  geom_histogram(binwidth=2, aes(colour=Sex, fill=Sex), alpha=0.4)+
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+
  Theme1+
  coord_cartesian(xlim = c(30, 100))+
  scale_x_continuous(breaks= c(30, 40, 50, 60, 70, 80, 90, 100))+
  ylab("Frequency")+
  xlab("Carapace length")+
  theme(strip.text = element_text(size=12, face="plain"))+
  facet_wrap(~location)
catch.plot