# Explore catch data----
rm(list=ls()) #clear memory

# librarys----
library(tidyr)
library(dplyr)
library(googlesheets)
library(stringr)
library(scales)
library(ggplot2)

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

dat.length.all<-gs_title("Lobsters_data_2018_All_1")%>% 
  gs_read_csv(ws = "Lobster.var" )%>%
  glimpse()

dat.length.all<-dat.length.all%>%
  mutate(Count=1)%>%
  filter(!is.na(Carapace.length))%>%
  filter(!is.na(Tag.number))%>%
  mutate(Colour=str_replace_all(.$Colour,c("W"="White", "R"="Red")))%>%
  mutate(Sex=str_replace_all(.$Sex, c("M"="Male", "F"="Female")))%>%
  mutate(Carapace.length=as.numeric(as.character(Carapace.length)))%>%
  mutate(trip.day.trap=paste(Trip,Day,Trap.Number,sep="."))%>%
  glimpse()


dat.length.all$Date <- as.character(dat.length.all$Date)

glimpse(dat.length.all)

# Write data
setwd(data.dir)


write.csv(dat.length.all,"lob.dat.all.csv")
dat.length.all<-read.csv("lob.dat.all.csv")



# Checks ---
summary(dat.length.all$Carapace.length) 
length(dat.length.all$Carapace.length) #8825 individual length measurements
length(unique(dat.length.all$trip.day.trap)) #1048

unique(dat.length.all$Trap.ID) # 522 levels
dat.length.all$Trap.ID

# # Import pot data----

dat.pot.all<-gs_title("Lobsters_data_2018_All_1")%>% 
  gs_read_csv(ws = "Pot.var")%>%
  mutate(trip.day.trap=paste(Trip,Day,Pot.Number,sep="."))%>%
  mutate(Site.Name=str_replace_all(.$Site.Name,c( "SM"="Seven Mile", "DM"="Davids Marks",  "RM"="Rivermouth", "IR"="Irwin Reef", "LR"="Long Reef", "SD"="South Dummy", "LH"="Little Horseshoe", "CHin1_"="Cliff Head Mid","CHin2_"="Cliff Head South","CHout1_" = "Cliff Head OUT1","CHout2_" = "Cliff Head North", "CHN"="Cliff Head North", "CHM"="Cliff Head Mid", "CHS"="Cliff Head South", "JB"="Jim Bailey", "GR"="Golden Ridge", "SR"="South Rig", "WL"="Whites Lump")))%>% 
  filter(Johns=="No")%>% # turn off if you add in john's data
  glimpse()


dat.pot.all$Site.Name

# Write data
setwd(data.dir)

write.csv(dat.pot.all,"dat.pot.all.csv")
dat.pot<-read.csv("dat.pot.all.csv")
unique(dat.pot.all$Trap.ID)  # 601 levels

# Checks----
unique(dat.pot.all$trip.day.trap)
length(unique(dat.pot.all$trip.day.trap)) #1379

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

# 116 missing in dat.po <_Neeeed to check this!!!

# Checks---
unique(dat.pot.all$trip.day.trap)
length(unique(dat.pot.all$trip.day.trap)) # 1379

# Recaptures----

### new recaptured ----

names(dat.length.all) #Recapture

almost.true.all<-dat.length.all%>% # caught between different months
  left_join(.,sites)%>%
  distinct(Site,Tag.number,Trip,Date)%>%
  group_by(Site,Tag.number)%>%
  dplyr::summarise(min=min(Trip),max=max(Trip),no.trips=n())%>%
  filter(max>min)%>%
  select(Tag.number)
  
All.recaps<-semi_join(dat.length.all,almost.true.all)%>% 
  group_by(Tag.number,Trip)%>%
  left_join(dat.length.all)
  
#1683



### Plotting----
setwd(plot.dir)

# Plotting themes pallettes and function
Theme1 <-
  theme( # use theme_get() to see available options
    strip.text.x = element_text(size = 11,angle = 0,face="italic"),
    strip.text.y = element_text(size = 11),
    strip.background=element_blank(), 
    axis.title.x=element_text(vjust=-0.0, size=12),
    axis.title.y=element_text(vjust=0.0, angle=90, size=12),
    axis.text.x=element_text(size=11,face="italic"), # Bg fixed 
    axis.line = element_line(colour = 'black'),  
    panel.border = element_rect(colour = 'black'), 
    axis.text.y=element_text(size=10),
    plot.title=element_text(size=10,face="bold",hjust=0.5),
    legend.text = element_text(size=12),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    panel.background=element_blank())
#theme_get()


# Loop plot to explore/find outliers/errors----

glimpse(All.recaps)

# list of values to loop over
tag.numbers = unique(All.recaps$Tag.number)


# Loop
for (i in tag.numbers) {
  
  temp_plot = ggplot(data= subset(All.recaps, Tag.number == i), 
    aes(x = as.Date(Date,"%d/%m/%y"), y=Carapace.length )) + 
    geom_point(size=3) +
    geom_smooth(method="lm",se=FALSE, colour='black', size=0.5)+
    ylim(min(All.recaps$Carapace.length),max(All.recaps$Carapace.length))+
    ylab("Carapace Length (mm)")+
    xlab("Date")+
    ggtitle(i)
  
  ggsave(temp_plot, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}

#Example Plot
  
glimpse(All.recaps)



test<-All.recaps%>%
  mutate(Date=as.Date(Date,"%d/%m/%y"))%>%
  glimpse()


plot.tag<- ggplot(data= filter(test, Tag.number == 'K3296'), 
  aes(x = Date, y=Carapace.length )) + 
    geom_point(size=3) +
    ylim(min(test$Carapace.length),max(test$Carapace.length))+
  scale_x_date(limits = c(min(test$Date),max(test$Date)), date_breaks = "1 month",labels=date_format("%b-%Y"))+
    geom_smooth( method=lm ,se=FALSE, colour='black', size=0.5)+
    ylab("Carapace Length (mm)")+
    xlab("Date")+
    ggtitle("Individual K3296")
plot.tag





