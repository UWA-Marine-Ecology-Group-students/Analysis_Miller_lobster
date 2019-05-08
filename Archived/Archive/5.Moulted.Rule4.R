# Explore catch data----
rm(list=ls()) #clears memory

# librarys----
library(tidyr)
library(dplyr)
library(googlesheets)
library(stringr)
library(ggplot2)
library(magrittr)
library(readr)
library(lubridate)

# Study name----
study<-"moulted.rule4"

# Set work directory----

#work.dir=("C:/Users/00097191/Google Drive/MEG/Projects/Projects_WRL/Project_WRL_low-catch zone/Fieldwork and Reporting/03_Trapping/Analysis_WRL_Reds_2018") 
# setwd("~/Documents/University/Masters Project/Plots/Plot per recapture")

work.dir=("~/GitHub/Analysis_Miller_WRL") #for Tim's github
work.dir=("~/workspace/Analysis_Miller_WRL") #for ecocloud server

# Set sub-directories----

data.dir=paste(work.dir,"Data",sep="/")
plot.dir=paste(work.dir,"Plots",sep="/")


# check1 <-read_csv("growth.comb.csv")%>%
  # glimpse()

#Filter out errors from Seven Mile data
#Four tags that have negative growth (more than -7)
#Filter out fisher returns errors
#   r4<-check1%>%
#   filter(Tag.number!="190428" & Tag.number!="190188" & Tag.number!="190124" &Tag.number!="190443")%>%
#   filter(Tag.number!="K2400"&Tag.number!="K1617"&Tag.number!="K1221")%>%
#   #filter(Site!="Rivermouth")%>% #filter out Rivermouth recaptures <Don't do this! as it removes all NA's aswell
#   glimpse()


#Add a column for TRUE if duplicated more than 2 times
# r4<- r4%>%
#   dplyr::group_by(Tag.number)%>%
#   mutate(dupe=n()>2)%>%
#   glimpse()

#Order data by tag number ~ easier to read
# rule.4 <-r4[order(r4$Tag.number),]%>%
#   glimpse()

#Filter out recaptures only caught twice:----

# rule.4.dupe <-rule.4%>%
#   filter(dupe=="TRUE")%>%
#   glimpse()

#Need to filter out Hebbos caught not within months 5->12----

# no.sm<-rule.4.dupe%>%
#   filter(is.na(Site)|Site != "Seven Mile")%>%
#   glimpse()

# sm<-rule.4.dupe%>%
#   filter(Site=="Seven Mile")%>%
#   mutate(month=month((as_date(Date))))%>%
#   filter(month%in%c(5:12))%>%#Filter out months 5-12
#   glimpse()

# r4.edit<-rbind(no.sm, sm)%>%
#   select(Trip, Date, Tag.number, Carapace.length, Site, Sex, Colour, Total.damage, mini.site)%>%
#   glimpse()


#Bring in data from googlesheets
#Bring in Data: Rule 4
# dat.r4<- gs_title("Moult.Rule.4")%>%
#   gs_read_csv(ws="Sheet1",col_types = cols(Tag.number = col_character()))%>%
#   glimpse()
# 
# 
# dat.r4.edit<-inner_join(dat.r4, r4.edit)%>%
#   glimpse()
# 
# #save data to edit for rule 4:
# setwd(data.dir)
# write.csv(dat.r4.edit, "r.4.new.csv")


#Bring in Data: Rule 4
dat.r4<- gs_title("r.4.new")%>%
  gs_read_csv(ws="Sheet1",col_types = cols(Tag.number = col_character()))%>%
  glimpse()

#check for zeros
sum(is.na(dat.r4$Tag.number)) #0
    
# cols(
#   X1 = col_integer(),
#   X1_1 = col_integer(),
#   Trip = col_character(),
#   Date = col_date(format = ""),
#   Tag.number = col_integer(),
#   Carapace.length = col_double(),
#   Site = col_character(),
#   Sex = col_character(),
#   Colour = col_character(),
#   Total.damage = col_integer()
# )


dat.r4<-dat.r4%>%
  mutate(inc=recaptured.tag-initial.tag)%>% #Add column for growth increase
  filter(!is.na(Site))%>% #Remove for now but edit later
  dplyr::rename(Location=Site)%>%
  dplyr::rename(site=mini.site)%>%
  mutate(Moulted=replace_na(Moulted, "N"))%>%
  glimpse()

glimpse(dat.r4)

# dat.r4 <- dat.r4%>%
#   mutate(inc=recaptured.tag-initial.tag)%>%
#   #filter(!is.na(Tag.number))%>%
#   dplyr::rename(Moulted=R4)%>%
#   glimpse()
# 
# dat.r4<-dat.r4%>%
#   mutate(Moulted=replace_na(Moulted, "N"))%>%
#   mutate(Moulted=str_replace_all(.$Moulted,c("N"="False")))%>%
#   mutate(Moulted=str_replace_all(.$Moulted,c("y"="True")))%>%
#   glimpse()

#Subset to only moulted individuals
rule.4.m <- dat.r4%>%
  subset(Moulted=="y")%>%
  glimpse()

#Save data for Rule 4:
setwd(data.dir)
write.csv(rule.4.m, "Rule.4.csv")

#Count how many moulted per site
#Need to filter out the measurements that are being averaged from
count.R4<-rule.4.m%>%
  filter(!is.na(initial.tag))%>%
  glimpse()

count.R4%>%
  group_by(Location)%>%
  dplyr::summarise(Total=n())%>%
  glimpse()

#Plotting----
# Plotting Themes ----
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

r4.m.plot <- ggplot(data=rule.4.m, aes(x=initial.tag, y=inc), notch=FALSE, position=dodge1, outlier.shape=NA)+
  geom_point(colour="red")+
  Theme1+
  ylab("Relative Growth (mm)")+
  xlab("Initial Carapace Length")+
  ylim(0, 25)+
  ggtitle("Rule # 4: 3+ recaptures & Visual moult")+
  theme(legend.position = "none") +
  facet_wrap(. ~ Location)
r4.m.plot

#Already done below.. 
# r4.edit<-dat.r4%>%
#   # mutate(year=format(as.Date(Date),'%Y')) %>%
#   # mutate(month=format(as.Date(Date),'%m'))%>%
#   mutate(month=month((as_date(Date))))%>% #Add a column for month
#   glimpse()

# r4.edit<-r4.edit%>%
#   filter(month%in%c(5:12))%>% #Filter out months 5-12
#   glimpse()

#Rrder sites: North-> South
rule.4.m$Location<-factor(rule.4.m$Location, levels = c("Seven Mile", "Irwin Reef", "Cliff Head", "Golden Ridge"))

glimpse(rule.4.m)
#boxplot
r4.box<-ggplot(data= rule.4.m ,aes(x=Location, y=inc),notch=FALSE,position = dodge1, outlier.shape = NA)+
  geom_boxplot(outlier.color = NA, notch=FALSE)+
  geom_jitter(width = 0.1, height = NULL)+
  guides(fill=FALSE)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"))+
  # stat_boxplot(geom='errorbar')+          # Adds error bars
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ #this is adding the dot for the mean
  theme_bw()+Theme1+
  #coord_cartesian(ylim=c(0,3.5))+
  ggtitle ("Rule 4: Moult")+
  theme(plot.title = element_text(hjust = 0, size=12, face = "plain"))+
  theme(axis.text.x = element_text(angle=90))+     #Changes angle of site names
  ylab("Growth (mm)")+
  xlab("Location") #+
  #facet_grid(. ~ Sex) #Turn off if you want growth combined
r4.box





