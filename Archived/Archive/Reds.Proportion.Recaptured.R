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
study<-"Proportion.Recaptures"

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
dir()

#### Trips 1-6 (Reds) ----

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


unique(dat.length$Trap.ID) # 365 levels
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
unique(dat.pot$Trap.ID)  # 400 levels

#Create "sites" for Reds----

sites<-dat.pot%>%
  distinct(Trap.ID,Site.Name)%>% #Keeps only distinct rows (Trap.ID & Site.Name)
  mutate(Trap.ID=as.character(Trap.ID))%>%
  dplyr::rename(Site=Site.Name)%>%
  glimpse()


#Add a column of "sites" by Trap.ID to dat.length----
#Reds
dat.length<-left_join(dat.length,sites, by="Trap.ID") 


#Check for missing sites: ones in dat.length but not in dat.pot
missing.site<-anti_join(dat.length,sites) # 0

glimpse(dat.length)

# Checks---
unique(dat.pot$day.trap)
length(unique(dat.pot$day.trap)) # 224

# Recaptures----

### new recaptured ----

names(dat.length) #Recapture

dat.recap<-dat.length%>%
  left_join(.,sites)%>%
  distinct(Site,Tag.number,Trip)%>%
  group_by(Site,Tag.number)%>%
  dplyr::summarise(min=min(Trip),max=max(Trip),no.trips=n())%>%
  filter(max>min)%>%                      #Filter out recaptures from within the same trip
  filter(!is.na(Tag.number))%>%
  ungroup()%>%
  group_by(Site)%>%
  dplyr::summarise(Total.recaptured=n())

glimpse(dat.recap)

dat.tagged<-dat.length%>%         #Total tagged at each site
  filter(!is.na(Tag.number))%>%
  group_by(Site)%>%
  dplyr::summarise(Total=n())


### Join total.tagged with total.recap-----

dat.recap.prop<-left_join(dat.tagged,dat.recap, by="Site")%>%
  filter(!is.na(Total.recaptured))%>%               #Filter out sites that don't occur in recaptured
  mutate(proportion = Total.recaptured/Total)       #Create new column of proportion of recaptures

# Plotting Themes ----
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=11),
    #legend.title = element_blank(),
    #legend.position = c(0.4, 0.8),
    text=element_text(size=12),
    strip.text.y = element_text(size = 11,angle = 270),
    axis.title.x=element_text(vjust=0.2, size=12),
    axis.title.y=element_text(vjust=0.2, angle=90, size=12),
    axis.text.x=element_text(size=11,colour="black", face="italic"),
    axis.text.y=element_text(size=11,colour="black", ),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.ticks.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank(),
    plot.background = element_blank())   

#Order sites North->South
dat.recap.prop$Site<-factor(dat.recap.prop$Site,levels=c("Irwin Reef","Whites Lump","Jim Bailey","South Rig","Little Horseshoe","Cliff Head North","Cliff Head Mid","Cliff Head South","Golden Ridge"))


cols <- c("Little Hosreshoe" = "red", "Cliff Head North" = "blue", "Cliff Head Mid" = "darkgreen", "Cliff Head South" = "orange")
scale_colour_manual(values = cols)

#Bar graph----

bar.prop<-ggplot(data = dat.recap.prop, aes(x = Site,y=proportion)) + 
  stat_summary(fun.y=mean, geom="bar",fill="white",colour="black") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  geom_hline(aes(yintercept=0))+
  geom_text(aes(x = Site,y=proportion, label=format(dat.recap.prop$proportion, digits = 2)), hjust=0.5, vjust=-0.6, size=3)+
  ggtitle ("Trips 1-6: Reds")+
  xlab("Site")+
  ylab("Proportion of recaptures")+
  theme_bw()+Theme1+ 
  coord_cartesian(ylim = c(0, 0.18))+
  theme(plot.title = element_text(hjust = 0, size=12, face = "plain"))+
  theme(axis.text.x = element_text(angle=90, vjust=0.7))+
  theme(axis.line.x=element_line(colour="white", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="white", size=0.5,linetype='solid'))
bar.prop 



help(scale_fill_manual)

  
  
  





