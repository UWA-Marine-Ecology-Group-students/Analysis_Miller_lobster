# Explore catch data----
rm(list=ls()) # Clears memory

# librarys----
library(tidyr)
library(dplyr)
library(googlesheets)
library(stringr)
library(lubridate)
library(ggplot2)

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

# work.dir=("~/GitHub/Analysis_Miller_WRL") #for Tim's github
# work.dir=("~/workspace/Analysis_Miller_WRL") #for ecocloud server
# work.dir=("C:/GitHub/Analysis_Miller_lobster")
work.dir=("Z:/Analysis_Miller_lobster") # FOr Ash's laptop using Git

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
# Import length data

dat.length<-read.csv("length.csv")%>%
  filter(!Source%in%c("fisheries-returns", "fisher-returns"))%>% #I want these removed
  select(Sample, Tag.number, Carapace.length, Sex, Colour, Count)%>% #To simplify for Ash, Brooke can remove
  replace_na(list(Count=1))%>% #Seven Mile North count as NA
  glimpse()

#Check
length(unique(dat.length$Tag.number)) #9687
length(dat.length$Carapace.length) #14254

#Import Pot data----
dat.pot<-read.csv("metadata.csv")%>%
  filter(!Location=="Rivermouth")%>% #Removes NAs from Fishers as well. good.
  dplyr::filter(!Pwo%in%c("1", "2", "X"))%>% #Removed 148 pots with Occy's.
  select(Date, Location, Site,Day.pull, Sample, Longitude, Latitude )%>%
  glimpse()

#Check
unique(dat.pot$Pwo)
unique(dat.pot$Location) 
unique(dat.pot$Source)

#Join Length and Pot data----
glimpse(dat.length) #14,254
glimpse(dat.pot) #1,857

length(unique(dat.pot$Sample)) #1857
length(unique(dat.length$Sample)) #1557

#This doesn't keep empty pots!
# dat.all<- dplyr::semi_join(dat.length, dat.pot)%>%
#   left_join(.,dat.pot)%>%
#   glimpse()


#This does keep empty pots
dat.all <- dplyr::full_join(dat.length, dat.pot)%>%
   filter(!is.na(Location))%>% #Removes the pwo/rivermouth data from the length data.
   glimpse()

length(unique(dat.all$Sample)) #1857, same. good.

#Seperate into Legal and Sublegal----

dat.legal <-dat.all%>%
  mutate(sizeclass= ifelse(Carapace.length>=76.0,"Legal", "Sublegal"))%>% 
  filter(!is.na(Carapace.length))%>%
  glimpse()

sum.legal <-dat.legal%>%
  group_by(Sample, sizeclass)%>%
  dplyr::summarise(Count=sum(Count))%>%
  ungroup()%>%
  distinct()%>%
  glimpse()

#FIND SUM PER POT----
sum.dat <-dat.all%>%
  group_by(Sample)%>%
  dplyr::summarise(Count=sum(Count))%>%
  ungroup()%>%
  mutate(sizeclass="All")%>%
  distinct()%>%
  bind_rows(.,sum.legal)%>%
  arrange(Sample)%>%
  glimpse()

dat.location <- dat.all%>%
  select(Date, Sample, Location, Site, Longitude, Latitude)%>%
  distinct()%>%
  group_by(Sample)%>%
  slice(1)%>% # Fix the 5 errors and turn off the group by and the slice
  glimpse()

test<-dat.location%>%
  group_by(Sample)%>%
  dplyr::summarise(n=n())%>%
  glimpse()

length(unique(dat.location$Sample)) #1857

#JOIN BACK WITH DATA: Trap.Id, Sum per pot, Location, Site----
test.dat<-sum.dat%>%distinct(Sample)

sum.per.pot<-full_join(sum.dat,dat.location)%>%
  tidyr::complete(sizeclass,nesting(Sample))%>%
  select(sizeclass,Sample,Count)%>%
  left_join(.,dat.location)%>%
  replace_na(list(Count=0))%>%
  arrange(Sample)%>%
  glimpse()

length(unique(sum.per.pot$Sample))
1857*3
5571
#Save sum data----

setwd(data.dir)
write.csv(sum.per.pot,"dat.catch.csv", row.names=F)



#Find mean catch per pot per date per location----
glimpse(dat.pot)
glimpse(dat.length)

dat <- dplyr::full_join(dat.length, dat.pot)%>%
  filter(!is.na(Location))%>% #Removes the pwo/rivermouth data from the length data. 587 removed
  glimpse()

legal.sum<- dat%>%
  mutate(sizeclass= ifelse(Carapace.length>=76.0,"Legal", "Sublegal"))%>% 
  filter(!is.na(Carapace.length))%>%
  group_by(Sample, sizeclass)%>%
  dplyr::summarise(Count=sum(Count))%>%
  ungroup()%>%
  distinct()%>%
  glimpse()

all.sum <-dat%>%
  group_by(Sample)%>%
  dplyr::summarise(Count=sum(Count))%>%
  ungroup()%>%
  mutate(sizeclass="All")%>%
  distinct()%>%
  bind_rows(.,legal.sum)%>%
  arrange(Sample)%>%
  glimpse()

location <- dat%>%
  select(Date, Sample, Location, Site, Longitude, Latitude)%>%
  distinct()%>%
  group_by(Sample)%>%
  slice(1)%>% # Fix the 5 errors and turn off the group by and the slice
  glimpse()

sum.pot<-full_join(legal.sum,location)%>%
  complete(sizeclass,nesting(Sample))%>%
  select(sizeclass,Sample,Count)%>%
  left_join(.,dat.location)%>%
  replace_na(list(Count=0))%>%
  arrange(Sample)%>%
  glimpse()

length(unique(sum.per.pot$Sample))
1433*3


#Okay I give up on the legal/sublegal/all above----
#New bit, sum per pot all

all <- dplyr::full_join(dat.length, dat.pot)%>%
  filter(!is.na(Location))%>% #Removes the pwo/rivermouth data from the length data.
  dplyr::mutate(Date=lubridate::as_date(ymd(Date)))%>%
  mutate(month=format(as.Date(Date, 'd%%m%Y'), '%m'))%>%
  glimpse()

#check
unique(all$month)
length(unique(all$Sample)) #1857

#sum per pot (Sample)
sum.all<- all%>%
  group_by(Sample)%>%
  dplyr::summarise(Count=sum(Count))%>%
  ungroup()%>%
  glimpse()

#sum per pot
sum.pot<-all%>%
  select(Date, month,Sample, Location, Site, Longitude, Latitude)%>%
  distinct()%>%
  left_join(sum.all)%>%
  replace_na(list(Count=0))%>%
  filter(!Count==0)%>%
  glimpse()

#Find the mean per trip/location
# sum.loc<-sum.pot%>%
#   group_by(month, Location)%>%
#   dplyr::summarise(Count=mean(Count))%>%
#   ungroup()%>%
#   filter(!Count==0)%>%
#   #dplyr::mutate(month=str_replace_all(.$month,c("05"="May", "06"="June", "07"="July", "08"="August", "09"="September","11"= "November", "12"= "December")))%>%
#   glimpse()

#Plot mean catch per pot----
# Theme-----
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #legend.position = "none",
    text=element_text(size=12),
    strip.text.y = element_text(size = 12,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=12, face = "bold"),
    axis.title.y=element_text(vjust=0.6, angle=90, size=12, face = "bold"),
    axis.text.x=element_text(size=12,angle = 0, colour = "black"), #, hjust=1,vjust=0.5
    axis.text.y=element_text(size=12,colour = "black"),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.text = element_text(size=14, face="bold"),
    strip.background = element_blank())

#Plot All----
plot.dat<-sum.pot%>%
  filter(!Location%in%c("Golden Ridge", "Seven Mile"))%>%
  glimpse()

mean.plot<- ggplot(plot.dat, aes(x=month, y=Count, group=Location, color=Location)) + 
  stat_summary(fun.y=mean, geom="line", size=1) +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1)+
  theme_bw()+Theme1+ 
  xlab("Month")+
  ylab("Average Lobster per pot (+/- SE)")+
  scale_x_discrete(breaks=c("05","06","07", "08", "09", "11", "12"),
                   labels=c("May", "June", "July", "Aug", "Sept", "Nov", "Dec"))
mean.plot

setwd(plots.di)

ggsave(mean.plot,file="mean.plot.png", width = 20, height = 12,units = "cm")

#Divide into Legal and Sublegal----

#Create sizeclass
legal<-all%>%
  mutate(sizeclass= ifelse(Carapace.length>=76.0,"Legal", "Sublegal"))%>% 
  filter(!is.na(Carapace.length))%>%
  glimpse()

#find sum per sizeclass----
sum.legal <-legal%>%
  group_by(Sample, sizeclass)%>%
  dplyr::summarise(Count=sum(Count))%>%
  ungroup()%>%
  distinct()%>%
  glimpse()

#find sum per pot----
sum.all <-all%>%
  group_by(Sample)%>%
  dplyr::summarise(Count=sum(Count))%>%
  ungroup()%>%
  mutate(sizeclass="All")%>%
  distinct()%>%
  bind_rows(.,sum.legal)%>%
  arrange(Sample)%>%
  glimpse()

dat.location <- all%>%
  select(Date, Sample, Location, Site, Longitude, Latitude)%>%
  distinct()%>%
  group_by(Sample)%>%
  slice(1)%>% # Fix the 5 errors and turn off the group by and the slice
  glimpse()

#JOIN BACK WITH DATA: Trap.Id, Sum per pot, Location, Site----
sum.per.pot<-full_join(sum.all,dat.location)%>%
  complete(sizeclass,nesting(Sample))%>%
  select(sizeclass,Sample,Count)%>%
  left_join(.,dat.location)%>%
  replace_na(list(Count=0))%>%
  arrange(Sample)%>%
  mutate(month=format(as.Date(Date, 'd%%m%Y'), '%m'))%>%
  glimpse()

glimpse(sum.per.pot)

sum.plot<-sum.per.pot%>%
  filter(!sizeclass=="All")%>%
  filter(!Count==0)%>%
  glimpse()

#Plot legal and sublegal----
legal.plot<- ggplot(sum.plot, aes(x=month, y=Count, group=Location, color=Location)) + 
  stat_summary(fun.y=mean, geom="line") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1)+
  theme_bw()+Theme1+ 
  xlab("Month")+
  ylab("Average Lobster per pot (+/- SE)")+
  scale_x_discrete(breaks=c("05","06","07", "08", "09", "11", "12"),
                   labels=c("May", "June", "July", "Aug", "Sept", "Nov", "Dec"))+
  facet_wrap(.~sizeclass)
legal.plot



