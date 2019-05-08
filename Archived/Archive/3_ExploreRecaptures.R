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
  dplyr::summarise(Total.recaptured=n())


glimpse(total.recap)

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


dates<-gs_title("Dates")%>% # To use GoogleSheets, 
  gs_read_csv(ws = "Sheet1")%>%
  separate(Date,into=c("m","d","y"))%>%
  mutate(Date=paste(d,m,y,sep="/"))%>%
  select(-c(m,d,y))%>%
  #mutate(Date.t=as.Date(Date, format="%m/%d/%y"))%>%
  glimpse()

#Combine with date

final.dat <- left_join(release.recap, dates)
  

# release.dat <- growth%>%
#   group_by(Tag.number)%>%
#   summarise(Date=min(Date))%>%
#   ungroup()#%>% #Hmm looses carapace length somehow

#Filters out doubles (same month)-completely
# test1<- growth%>%
#   group_by(Trip, Tag.number)%>%
#   summarise(n=n())%>%
#   filter(n==1)%>% #This removes it all together, not what I want?? slice()?
#   ungroup()

# test2<- semi_join(growth, test1)%>%
#   spread(Trip, Carapace.length)%>%
#   group_by(Tag.number)



#Individuals caught twice in same sampling trip
doubles<-growth%>%
  group_by(Trip,Tag.number)%>%
  summarise(n=n())%>%
  filter(n>1)



#true.recaps<-growth%>%
#    group_by(Tag.number,Trip)%>%
#    slice(which.min(Day))#%>% # chooses first day, can change to which.max(carapace.length) or whatever
#  left_join(dat.length)


#changed.sex.or.col<-wide%>%
#  group_by(Tag.number)%>%
#  summarise(number=n())%>%
#  filter(number>1)
 

#1702 individuals caught in different months (true recaptures)

### For growth model ----
#1315 individuals

dates<-gs_title("Dates")%>% # To use GoogleSheets, 
  gs_read_csv(ws = "Sheet1")%>%
  separate(Date,into=c("m","d","y"))%>%
  mutate(Date=paste(d,m,y,sep="/"))%>%
  select(-c(m,d,y))%>%
  #mutate(Date.t=as.Date(Date, format="%m/%d/%y"))%>%
  glimpse()





### Plotting----
setwd(plot.dir)

# Loop plot to explore/find outliers/errors----

glimpse(All.recaps)
#Change working dir~to save plots
setwd("~/workspace/Analysis_Miller_WRL/Plots")

#Change Date format
All.recaps<-All.recaps%>%
  mutate(Date=as.Date(Date,"%d/%m/%y"))%>%
  glimpse()

# list of values to loop over
tag.numbers = unique(All.recaps$Tag.number)

# Loop
for (i in tag.numbers) {
  
  temp_plot = ggplot(data= subset(All.recaps, Tag.number == i), 
    aes(x = Date, y=Carapace.length )) + 
    geom_point(size=3) +
    geom_text(aes(label=Carapace.length), hjust=0.5, vjust=-1, size=3.5)+
    geom_smooth(method="lm",se=FALSE, colour='black', size=0.5)+
    ylim(min(All.recaps$Carapace.length),max(All.recaps$Carapace.length))+
    scale_x_date(limits = c(min(All.recaps$Date),max(All.recaps$Date)), date_breaks = "1 month",labels=date_format("%b-%Y"))+
    ylab("Carapace Length (mm)")+
    xlab("Date")+
    theme(axis.line.x=element_line(colour="black", size=0.5,linetype='solid'))
    theme(axis.line.y=element_line(colour="black", size=0.5,linetype='solid'))
    ggtitle(i)
  
  ggsave(temp_plot, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}

#Example Plot

test<-All.recaps%>%
  mutate(Date=as.Date(Date,"%d/%m/%y"))%>%
  glimpse()

names(test)

plot.tag<- ggplot(data= filter(test, Tag.number == '191502'), 
  aes(x = Date, y=Carapace.length, col=Outlier)) + #
    geom_point(size=3) +
    scale_color_manual(values=c("y" = "red", "n" = "black"))+
    geom_text(aes(label=Carapace.length), hjust=0.5, vjust=-1, size=3.5, nudge_y = 0.5)+
    ylim(min(test$Carapace.length),max(test$Carapace.length))+
    scale_x_date(limits = c(min(test$Date),max(test$Date)), date_breaks = "1 month",labels=date_format("%b-%Y"))+
    geom_smooth( method=lm ,se=FALSE, colour='black', size=0.5)+
    ylab("Carapace Length (mm)")+
    xlab("Date")+
    ggtitle("Individual K3296")+
    theme(axis.line.x=element_line(colour="black", size=0.5,linetype='solid'))+
    theme(axis.line.y=element_line(colour="black", size=0.5,linetype='solid'))+
    theme(panel.background=element_blank())
plot.tag



help(ggplot)  
  
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
