# Explore catch data----
rm(list=ls()) # Clears memory

# librarys----
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(googlesheets)
library(readr)

# Study name----
study<-"Lobster.Data"

# Set work directory----
work.dir=("C:/GitHub/Analysis_Miller_lobster") # Brooke
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

# Bring in and format the data----
setwd(data.dir)
dir()

dat<- read.csv("length.sw.sst.csv")%>% 
  #change locations to four
  mutate(Location=str_replace_all(.$Location, c("Little Horseshoe"="Boundary", "Golden Ridge"="Boundary", "Irwin Reef"="Mid", "White Point"="Mid", "Cliff Head"="Low-Catch", "Seven Mile"= "Control")))%>%
  drop_na(Colour)%>%
  drop_na(Carapace.length)%>%
  filter(Carapace.length>0)%>%
  filter(Colour%in%c("Red","White"))%>%
  # filter(!Location=="Golden Ridge")%>% #think about putting that back in
  # #   Transform variables
  mutate(Date=as.factor(yday(Date)))%>%
  mutate(Site=as.factor(Site))%>%
  mutate(Location=as.factor(Location))%>%
  mutate(Pot.number=as.factor(Pot.number))%>%
  select(Location, Site, Sample, Tag.number, Carapace.length, Sex, Colour)%>%
  glimpse()

unique(dat$Location)


#Removes doubles from Ben's SM data-----
dat<- dat[!duplicated(dat[,c("Tag.number","Sample", "Carapace.length")]),]%>%
  glimpse

dat<-as.data.frame(dat)

unique(dat$Location)

# Plotting Themes ----
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=12),
    text=element_text(size=12),
    strip.text.x = element_text(size = 12),
    strip.text.y = element_text(size = 12,angle = 270, vjust=3),
    axis.title.x=element_text(vjust=0.3, size=12),
    axis.title.y=element_text(vjust=0.6, angle=90, size=12),
    axis.text.x=element_text(size=12,colour="black"),
    axis.text.y=element_text(size=12,colour="black"),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.ticks.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())

#### Mean

mean<-tapply(dat$Carapace.length, list(dat$Colour, dat$Location), mean)
mean<-as.data.frame(mean)
glimpse(mean)

glimpse(dat)

unique(dat$Location)
dat_mean <- dat %>% 
  group_by(Location, Colour)%>% 
  dplyr::summarise(mean = mean(Carapace.length),min=(mean(Carapace.length) - se(Carapace.length)),max=se.max(Carapace.length),median = median(Carapace.length),number=n())%>%
  glimpse()

labels<-dat_mean%>%
  mutate(label=paste("N (",Colour,"): ",number,sep=""))

#Hitsograms----
glimpse(dat)
dat$Location<-factor(dat$Location, levels = c("Low-Catch","Boundary", "Mid", "Control"))
glimpse(dat)
unique(dat$Location)

hist.stage<-ggplot(data = dat, aes(x = Carapace.length, fill = Colour)) + 
  geom_histogram(data = dplyr::filter(dat, Colour == "White"), aes(y = ..density..),col="grey30",binwidth=2) + 
  geom_histogram(data = dplyr::filter(dat, Colour == "Red"), aes(y = -..density..),col="grey30",binwidth=2) +
  #geom_point(data = dat_mean, aes(x = mean, y=0, shape=Colour), size=3)+
  geom_point(data = dplyr::filter(dat_mean, Colour == "Red"), aes(x = median, y=0, shape=Colour), size=3)+
  geom_point(data = dplyr::filter(dat_mean, Colour == "White"), aes(x = median, y=0, shape=Colour), size=3)+
  #geom_segment(data = dat_mean, aes(x=min,xend=max,y=0.02,yend=0.02))+
  #geom_errorbarh(data=dplyr::filter(dat, Colour == "White"),aes(xmin=se.min(Carapace.length), xmax=se.max(Carapace.length),y=0),height = .92)+
  #geom_errorbarh(data=dplyr::filter(dat, Colour == "Red"),aes(xmin=se.min(Carapace.length), xmax=se.max(Carapace.length),y=0),height = .92)+
  #geom_linerange(data=dat_mean, aes(xmin=se.min, xmax=se.max))+
  scale_y_continuous(name='Relative density',breaks = seq(-0.08,0.08,0.04),labels=abs(seq(-8,8,4))) +
  # coord_flip()+
  theme_bw()+
  Theme1+
  geom_vline(xintercept = 76, linetype = "dashed")+
  xlab("Carapace length (mm)")+
  scale_fill_manual(values = c("red","white"))+ #,name="Lobster colouration",labels=c("Red","White"
  geom_hline(aes(yintercept=0))+
  facet_grid(Location~.)+
  xlim(40,100)+
  #ylim(-0.08,0.15)+
  theme(legend.position="top")+
  #annotate("text", x = 60, y = 0, label = paste("n red:",number))+
  geom_text(data = dplyr::filter(labels, Colour == "White"), aes(x = 97, y = 0.07, label = label))+
  geom_text(data = dplyr::filter(labels, Colour == "Red"), aes(x = 97, y = 0.05, label = label))

hist.stage

se.min(dat$Carapace.length)
mean(dat$Carapace.length)

#Save plots--
setwd(plots.dir)


ggsave(hist.stage,file="hist.location.stage.tall.median.png", width = 10, height = 16, units = "cm")


