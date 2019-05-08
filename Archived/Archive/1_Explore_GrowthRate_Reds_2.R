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

# Study name----
study<-"Growth.Rate"

# Set work directory----
work.dir=("C:/Users/00097191/Google Drive/MEG/Projects/Projects_WRL/Project_WRL_low-catch zone/Fieldwork and Reporting/03_Trapping/Analysis_WRL_Reds_2018") # Brooke

work.dir.whites=("C:/Users/00097191/Google Drive/MEG/Projects/Projects_WRL/Project_WRL_low-catch zone/Fieldwork and Reporting/03_Trapping/Analysis_WRL_Whites_2018") # Brooke

work.dir=("~/Google Drive/Projects/Project_WRL low-catch zone/Fieldwork and Reporting/03_Trapping/Analysis_WRL_Reds_2018")



## Sub directories ----
data.dir<-paste(work.dir,"Data",sep="/")
data.dir.whites<-paste(work.dir.whites,"Data",sep="/")
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

#### Trips 1-6 ----

# Import length data----

dat.length<-gs_title("Lobsters_data_2018_Reds_20180511")%>% # To use GoogleSheets
  gs_read_csv(ws = "Lobster.var" )%>%
  glimpse()

dat.length<-dat.length%>%
  mutate(Count=1)%>%
  filter(!is.na(Carapace.length))%>%
  mutate(Carapace.length=as.numeric(as.character(Carapace.length)))%>%
  mutate(day.trap=paste(Day,Trap.Number,sep="."))%>%
  glimpse()

write.csv(dat.length,"lob.dat.csv")
dat.length<-read.csv("lob.dat.csv")


unique(dat.length$Trap.ID) # 381 levels
dat.length$Trap.ID

## Bring in whites dat ----
dat.length.wh<-gs_title("Lobsters_data_Whites_2018")%>% # To use GoogleSheets
  gs_read_csv(ws = "Lobster.var" )%>%
  mutate(Count=1)%>%
  filter(!is.na(Carapace.length))%>%
  mutate(Carapace.length=as.numeric(as.character(Carapace.length)))%>%
  mutate(day.trap=paste(Day,Trap.Number,sep="."))%>%
  glimpse()

write.csv(dat.length.wh,"lob.dat.csv")
dat.length.wh<-read.csv("lob.dat.csv")


unique(dat.length.wh$Trap.ID) #197

# # Import pot data----
dat.pot<-gs_title("Lobsters_data_2018_Reds_20180511")%>% # To use GoogleSheets
  gs_read_csv(ws = "Pot.var")%>%
  mutate(day.trap=paste(Day,Pot.Number,sep="."))%>%
  mutate(Site.Name=str_replace_all(.$Site.Name,c( "SM"="Seven Mile", "DM"="Davids Marks",  "RM"="Rivermouth", "IR"="Irwin Reef", "LR"="Long Reef", "SD"="South Dummy", "LH"="Little Horseshoe", "CHin1_"="Cliff Head Mid","CHin2_"="Cliff Head South","CHout1_" = "Cliff Head OUT1","CHout2_" = "Cliff Head North", "JB"="Jim Bailey", "GR"="Golden Ridge", "SR"="South Rig", "WL"="Whites Lump")))%>% 
  filter(Johns=="No")%>% # turn off if you add in john's data
  glimpse()

dat.pot$Site.Name
write.csv(dat.pot,"dat.pot.csv")
dat.pot<-read.csv("dat.pot.csv")
unique(dat.pot$Trap.ID) #399 

## Whites pot dat -----
dat.pot.wh <- gs_title("Lobsters_data_Whites_2018")%>%
  gs_read_csv(ws= "Pot.var")%>%
  mutate(day.trap=paste(Day,Pot.Number,sep="."))%>%
  mutate(Site=str_replace_all(.$Location,c("CHN"="Cliff Head-north", "CHM"="Cliff Head-mid", "CHS"="Cliff Head-south", "GR"="Golden Ridge", "LH"="Little Horseshoe", "IR"="Irwin Reef", "WP"="Jim Bailey", "WL"="Whites Lump", "SR"="South Rig")))%>% 
  filter(Johns=="No")%>% # turn off if you add in john's data
  glimpse()

dat.pot.wh$Site
write.csv(dat.pot.wh,"dat.pot.wh.csv")
dat.pot.wh<-read.csv("dat.pot.wh.csv")
unique(dat.pot.wh$Trap.ID) #243 


sites<-dat.pot%>%
  distinct(Trap.ID,Site.Name)%>%
  mutate(Trap.ID=as.character(Trap.ID))%>%
  dplyr::rename(Site=Site.Name)%>%
  glimpse()

sites.wh<-dat.pot.wh%>%
  distinct(Trap.ID,Site)

#Add a column of "sites" by Trap.ID to dat.length
dat.length<-left_join(dat.length,sites, by="Trap.ID") 
dat.length.wh<-left_join(dat.length.wh,sites.wh, by="Trap.ID") 

missing.site<-anti_join(dat.length,sites) # these ones are not in dat.pot but are in dat.length 01/09

glimpse(dat.length)

# Checks---
unique(dat.pot$day.trap)
length(unique(dat.pot$day.trap)) # 224

# Recaptures----








### new recaptured ----

names(dat.length) #Recapture

total.recap<-dat.length%>%
  left_join(.,sites)%>%
  distinct(Site,Tag.number,Trip)%>%
  group_by(Site,Tag.number)%>%
  dplyr::summarise(min=min(Trip),max=max(Trip),no.trips=n())%>%
  filter(max>min)%>%
  
  filter(!is.na(Tag.number))%>%
  ungroup()%>%
  group_by(Site)%>%
  dplyr::summarise(Total.recaptured=n())




total.tagged<-dat.length%>%
  filter(!is.na(Tag.number))%>%
  group_by(Site)%>%
  dplyr::summarise(Total=n())

# Check NAs in site

#Not sure if accurate ^

### Trying to make wide dataset (columns = month, each row lobster measurements)

almost.true.recaps<-dat.length%>% # caught between different months
  left_join(.,sites)%>%
  distinct(Site,Tag.number,Trip,Date)%>%
  group_by(Site,Tag.number)%>%
  dplyr::summarise(min=min(Trip),max=max(Trip),no.trips=n())%>%
  filter(max>min)%>%
  select(Tag.number)

#191410 caught twice in same trip

doubles<-wide%>%
  group_by(Trip,Tag.number)%>%
  summarise(n=n())%>%
  filter(n>1)

names(wide)

# Good example tag = 191033
dates<-

true.recaps<-semi_join(dat.length,almost.true.recaps)%>%
  group_by(Tag.number,Trip)%>%
  slice(which.min(Day))%>% # chooses first day, can change to which.max(carapace.length) or whatever
  left_join(dat.length)

dates<-gs_title("Dates")%>% # To use GoogleSheets
  gs_read_csv(ws = "Sheet1")%>%
  separate(Date,into=c("m","d","y"))%>%
  mutate(Date=paste(d,m,y,sep="/"))%>%
  select(-c(m,d,y))%>%
  #mutate(Date.t=as.Date(Date, format="%m/%d/%y"))%>%
  glimpse()

growth<-semi_join(dat.length,true.recaps)%>%
  select(Date,Tag.number,Site,Trip,Carapace.length,Sex,Colour)%>% #
  mutate(Trip=paste("T",Trip,sep=""))%>%
  group_by(Site,Colour,Sex,Tag.number) %>%
  mutate(Diff.cl = Carapace.length - lag(Carapace.length))%>%
  left_join(dates)%>%
  mutate(Diff.day = Date.number - lag(Date.number))%>%
  mutate(Diff.week=Diff.day/7)%>%
  mutate(Growth.day=Diff.cl/Diff.day)%>%
  mutate(Growth.week=Diff.cl/Diff.week)%>%
  filter(!is.na(Diff.cl))%>% # to get rid of first tagged 
  filter(!is.na(Tag.number))%>% # ones where tag number was not included
  filter(Growth.week>0) # turn off if you want negative growth

# growth.dat<-wide%>%
#   mutate("T6-5"=(T6-T5)/2)%>%
#   mutate("T6-4"=(T6-T4)/3)%>%
#   mutate("T6-3"=(T6-T3)/4)%>%
#   mutate("T6-2"=(T6-T2)/5)%>%
#   mutate("T6-1"=(T6-T1)/6)%>%
#   mutate("T5-4"=(T5-T4)/1)%>%
#   mutate("T5-3"=(T5-T3)/2)%>%
#   mutate("T5-2"=(T5-T2)/3)%>%
#   mutate("T5-1"=(T5-T1)/4)%>%
#   mutate("T4-3"=(T4-T3)/1)%>%
#   mutate("T4-2"=(T4-T2)/2)%>%
#   mutate("T4-1"=(T4-T1)/3)%>%
#   mutate("T3-2"=(T3-T2)/1)%>%
#   mutate("T3-1"=(T3-T1)/2)%>%
#   mutate("T2-1"=(T2-T1)/1)%>%
#   #mutate(mean.growth=rowMeans(c[,11:(ncol(.))],na.rm = T))
#   mutate(growth=(rowSums(.[,11:(ncol(.))],na.rm = TRUE )))%>%
#   mutate(num.measurements.pos=rowSums(.[,11:25] > 0,na.rm = TRUE))%>%
#   mutate(num.measurements.neg=rowSums(.[,11:25] < 0,na.rm = TRUE))%>%
#   mutate(num.measurements=num.measurements.pos+num.measurements.neg)%>%
#   mutate(mean.growth=growth/num.measurements)%>%
#   mutate(mean.growth=ifelse(mean.growth=="NaN",0,mean.growth))
  

avg.position<-dat.pot%>%
  group_by(Site.Name)%>%
  summarise(lat=mean(Latitude.y),lon=mean(Longitude.x))%>%
  rename(Site=Site.Name)

### Box plots ----
# Plotting themes pallettes and function
Theme1 <-
  theme( # use theme_get() to see available options
    strip.text.x = element_text(size = 11,angle = 0,face="italic",family="TN"),
    strip.text.y = element_text(size = 11,family="TN"),
    strip.background=element_blank(), 
    axis.title.x=element_text(vjust=-0.0, size=12,family="TN"),
    axis.title.y=element_text(vjust=0.0, angle=90, size=12,family="TN"),
    axis.text.x=element_text(size=10,face="italic",family="TN"), # Bg fixed 
    axis.line = element_line(colour = 'black'),  
    panel.border = element_rect(colour = 'black'), 
    axis.text.y=element_text(size=10,family="TN"),
    plot.title=element_text(size=10,face="bold",hjust=0.5,family="TN"),
    legend.text = element_text(size=12,family="TN"),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    panel.background=element_blank())
theme_get()


names(growth.dat)

site.loc<-left_join(dat.pot,sites)%>%
  group_by(Site)%>%
  summarise(av.lat=mean(Latitude.y))

growth.dat<-left_join(growth.dat,site.loc)%>%
  arrange(-av.lat)

growth.dat$Site<-factor(growth.dat$Site,levels=c("Irwin Reef","Whites Lump","Jim Bailey","South Rig","Little Horseshoe","Cliff Head North","Cliff Head Mid","Cliff Head South","Golden Ridge"))

# box.growth<-ggplot(data=growth.dat,aes(x=factor(Site), y=mean.growth),notch=FALSE,position = dodge1, outlier.shape = NA)+
#   #geom_point()+
#   guides(fill=FALSE)+
#   theme( panel.background = element_blank(),axis.line = element_line(colour = "black"))+
#   stat_boxplot(geom='errorbar')+
#   geom_boxplot(outlier.color = NA, notch=FALSE)+
#   stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ #this is adding the dot for the mean
#   theme_bw()+
#   Theme1+
#   coord_cartesian(ylim=c(-1,2))+
#   ylab("Mean growth per month (mm)")+
#   xlab("Site")
# box.growth

growth$Site<-factor(growth$Site,levels=c("Irwin Reef","Whites Lump","Jim Bailey","South Rig","Little Horseshoe","Cliff Head North","Cliff Head Mid","Cliff Head South","Golden Ridge"))

new.box.growth<-ggplot(data=growth,aes(x=factor(Site), y=Growth.week),notch=FALSE,position = dodge1, outlier.shape = NA)+
  #geom_point()+
  guides(fill=FALSE)+
  theme( panel.background = element_blank(),axis.line = element_line(colour = "black"))+
  stat_boxplot(geom='errorbar')+
  geom_boxplot(outlier.color = NA, notch=FALSE)+
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ #this is adding the dot for the mean
  theme_bw()+
  Theme1+
  coord_cartesian(ylim=c(0,0.5))+
  ylab("Mean growth per week (mm)")+
  xlab("Site")+ facet_grid(. ~ Sex)
new.box.growth


summary(growth)

# 
# 
# initial.size<-dat.length%>%
#   filter(is.na(Recapture))%>%
#   left_join(growth.dat)%>%
#   filter(!is.na(mean.growth))%>%
#   filter(mean.growth>0)

plot<-ggplot(data=growth,aes(x=Carapace.length, y=Growth.week),notch=FALSE,position = dodge1, outlier.shape = NA)+
  geom_point()+ geom_smooth(method = "gam")+ facet_grid(. ~ Sex)
plot

#### Now start using the whites data ----
mean.cl.wh<-dat.length.wh%>%
  mutate(Trip=7)%>%
  filter(Colour=="W")%>%
  filter(Recapture=="TRUE")%>%
  group_by(Site,Tag.number,Trip,Sex,Colour)%>%
  summarise(Carapace.length=mean(Carapace.length))%>%
  semi_join(dat.length,by="Tag.number")

wide.wh<-dat.length%>%
  group_by(Trip,Site,Sex,Tag.number)%>%
  summarise(Carapace.length=mean(Carapace.length))%>%
  bind_rows(.,mean.cl.wh)%>%
  ungroup()%>%
  select(Tag.number,Site,Trip,Carapace.length,Sex)%>% #
  mutate(Trip=paste("T",Trip,sep=""))%>%
  filter(!is.na(Tag.number))%>% # ones where tag number was not included
  filter(!Tag.number=="NA")%>%
  spread(Trip,Carapace.length)%>%
  filter(!is.na(T7))

setwd(work.dir.whites)
write.csv(wide.wh,"recaptures.from.trip.7.csv")

changed.sex.or.col<-wide%>%
  group_by(Tag.number)%>%
  summarise(number=n())%>%
  filter(number>1)




