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

sites<-dat.pot%>%
  distinct(Trap.ID,Site.Name)%>%
  mutate(Trap.ID=as.character(Trap.ID))%>%
  dplyr::rename(Site=Site.Name)%>%
  glimpse()

#Add a column of "sites" by Trap.ID to dat.length
dat.length<-left_join(dat.length,sites, by="Trap.ID") 

glimpse(dat.length)

# Checks---
unique(dat.pot$day.trap)
length(unique(dat.pot$day.trap)) # 224

