# Explore catch data----

rm(list=ls()) #clear memory

# librarys----
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(readr)


# Study name----
study<-"frequency.plot"

# Set work directory----
work.dir=("Z:/Analysis_Miller_lobster")
work.dir=("C:/GitHub/Analysis_Miller_lobster_current") # Brooke's desktop

setwd(work.dir)

# Set sub-directories----
data.dir=paste(work.dir,"Data",sep="/")
plots.dir=paste(work.dir,"Plots",sep="/")

#Import data----
dat.length<-read_csv("length.csv")%>%
  filter(!is.na(Carapace.length))%>%
  filter(!Sex=="Unknown")%>%
  mutate(Count=1)%>%
  #select(Sample, Tag.number, Carapace.length, Sex, Colour, Count)%>%
  glimpse()

unique(dat.length$Source)

dat.ben<-dat.length%>%
  filter(Source=="ben-seven-mile")%>%
  glimpse()

#Import Pot data----
dat.pot<-read.csv("metadata.csv")%>%
  dplyr::mutate(Date=as_date(ymd(Date)))%>%
  filter(!Location=="Rivermouth")%>% #Removes NAs from Fishers as well. good.
  select(Date, Location, Site,Day.pull, Sample, Pot.number, Longitude, Latitude )%>%
  glimpse()


#Join Length and Pot data----

catch.all<- dplyr::semi_join(dat.length, dat.pot)%>%
  dplyr::left_join(.,dat.pot)%>%
  glimpse()

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
    axis.text.x=element_text(size=12,colour="black"),
    axis.text.y=element_text(size=12,colour="black"),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.ticks.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank(),
    plot.background = element_blank()) # Brooke added


#Frequency plots----
glimpse(catch.all)

catch.plot <- ggplot(data=catch.all, aes(Carapace.length))+
  geom_histogram(binwidth=2, aes(colour=Sex, fill=Sex), alpha=0.4)+
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+
  coord_cartesian(xlim = c(30, 100))+
  scale_x_continuous(breaks= c(30, 40, 50, 60, 70, 80, 90, 100))+
  ylab("Catch frequency")+
  xlab("Carapace length (mm)")+
  theme_bw()+
  Theme1+
  theme(strip.text = element_text(size=12, face="plain"))+
  facet_wrap(~Location)
catch.plot


# Save plot----
setwd(plots.dir)

ggsave(catch.plot,file="catch.frequency.png", width = 17, height = 12,units = "cm")



#Catch frequency per Trip, location, sex----
#These aren't great- Won't use
av.catch.data<-catch.all%>%
  #filter(!Trip=="T0" & !Trip=="T10")%>%
  select(Trip, Location, Sex, Count)%>%
  glimpse()


av.test <- av.catch.data%>%
  group_by(Trip, Location, Sex)%>%
  glimpse()
  

av <- dplyr::summarise(av.test, catch=n())%>%
  glimpse()


av.plot<-ggplot(av, aes(x=Trip, y=catch, fill=Location))+
  geom_bar(stat="identity")
av.plot



