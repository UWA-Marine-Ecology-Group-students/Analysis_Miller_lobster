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
library(emmeans)
library(lmerTest)
# Study name----
study<-"moulted"

# Set work directory----

#work.dir=("C:/Users/00097191/Google Drive/MEG/Projects/Projects_WRL/Project_WRL_low-catch zone/Fieldwork and Reporting/03_Trapping/Analysis_WRL_Reds_2018") 
# setwd("~/Documents/University/Masters Project/Plots/Plot per recapture")

work.dir=("~/GitHub/Analysis_Miller_WRL") #for Tim's github
work.dir=("~/workspace/Analysis_Miller_WRL") #for ecocloud server

# Set sub-directories----

data.dir=paste(work.dir,"Data",sep="/")
plot.dir=paste(work.dir,"Plots",sep="/")

#Bring in recapture data from ecocloud----

# setwd("~/workspace/Analysis_Miller_WRL/Data")
# # dat.rr<-read_csv("dat.rr.csv")%>% OR bring in one with all recaps from same trip
# #dat.rr<- read_csv("dat.rr.all.csv")%>%
dat.rr<- read_csv("dat.rr.clean.csv")%>% #No jan- April, no daamage > 2, no movement between location
  glimpse()

dat.rr<- dat.rr%>%
  mutate(inc=recap.cl-initial.cl)%>% #Makes column for growth increment
  dplyr::mutate(diff=recap.Date-Date)%>%
  filter(inc>-4)%>%
  glimpse()


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

#Determine potential individuals that have moulted----

#Rule number 1: ----
#Colour change: Red to White
 
rule.1<- dat.rr%>%
  mutate(Moulted=ifelse(Colour.rec==Colour.int, "False", "True"))%>% #True for individuals that have changed red to white
  mutate(Moulted=ifelse((Colour.int=="White"), "False", Moulted))%>% #False for indivdiuals that have changed white to red or white to white, can change this!
  mutate(Moulted= replace_na(Moulted, "False"))%>%
  glimpse()
 

plyr::count(rule.1$Moulted)
#62 recapture lines that have moulted white to red
#1660 recapture lines that have not (false)

#Create data set for Rule 1----
#I feel like I have to filter out recaps at liberty for > 3 months
rule.1.m <- rule.1%>%
  subset(Moulted=="True")%>%
  #filter(!diff>100)%>%
  glimpse()

#Save Rule 1
setwd(data.dir)
write.csv(rule.1.m, "Rule.1.csv")
# 
# 
# #Number of moulted recaptures per site
rule.1.m%>%
  group_by(Location.int)%>%
  dplyr::summarise(Total=n())%>%
  glimpse()

#Seven Mile: 3 or 3
#Cliff Head: 18 or 5
#Golden Ridge: 6 or 1
#Irwin Reef: 35 or 7

#Only leves 16 recaptures-Ahwell

# #Plot all
# r1.plot <- ggplot(data=rule.1, aes(x=initial.cl, y=inc, col=Moulted), notch= FALSE, position=dodge1, outlier.shape=NA)+
#   #geom_smooth(method='lm', se=F, size=0.7)+
#   geom_point()+ #alpha=0.7
#   scale_color_manual(values = c("True"="red", "False"= "black"))+
#   Theme1+
#   theme(legend.key = element_rect(fill="white"))+
#   ylab("Relative Growth (mm)")+
#   xlab("Initial Carapace Length (mm)")+
#   ggtitle("Rule # 1: Red to White") #+
#   #facet_wrap(. ~ Site.int) #, scales = "free"
# r1.plot

# #Plot of moulted
# r1.m.plot <- ggplot(data=rule.1.m, aes(x=initial.cl, y=inc), notch=FALSE, position=dodge1, outlier.shape=NA)+
#   geom_point(colour="red")+
#   Theme1+
#   ylab("Relative Growth (mm)")+
#   xlab("Initial Carapace Length (mm)")+
#   ggtitle("Rule # 1: Red to White")+
#   theme(legend.position = "none") #+
#   #facet_wrap(. ~ Site.int)
# r1.m.plot
  

#Rule number 2: -----

#Caught after 01/12/2018 by fishers
#Initial Colour was Red
#Recap colour is NA?? Maybe?

# glimpse(dat.rr)
# 
rule.2 <- dat.rr%>%
  mutate(Moulted=ifelse((((Colour.rec%in%c(NA))&(Colour.int%in%c("Red"))&(recap.Date>'2018-12-01'))),"True","False"))%>%
  glimpse()
# 
# length(rule.2$Moulted[rule.2$Moulted=="True"]) #44
plyr::count(rule.2$Moulted) 
# #31 True
# #1688 False
# 
# #subset only moulted indidviduals with Rule 2
rule.2.m <- rule.2%>%
  subset(Moulted=="True")%>%
  #filter(!diff>100)%>%
   glimpse
# 
# #Save Rule 2
setwd(data.dir)
write.csv(rule.2.m, "Rule.2.csv")

# #Number of moulted recaptures per site
rule.2.m%>%
  group_by(Location.int)%>%
  dplyr::summarise(Total=n())%>%
  glimpse()

#Cliff Head: 17 or 0
#Golden Ridge: 7 or 0
#Irwin Reef: 7 or 0
#Seven Mile: 4 or 0

# #Plot all
# r2.plot <- ggplot(data=rule.2, aes(x=initial.cl, y=inc, col=Moulted), notch= FALSE, position=dodge1, outlier.shape=NA)+
#   #geom_smooth(method='lm', se=F, size=0.7)+
#   geom_point()+ #alpha=0.7
#   scale_color_manual(values = c("True"="red", "False"= "black"))+
#   Theme1+
#   theme(legend.key = element_rect(fill="white"))+
#   ylab("Relative Growth (mm)")+
#   xlab("Initial Carapace Length (mm)")+
#   ggtitle("Rule # 2: Fisher caught after 01/12/2018")+
#   facet_wrap(. ~ Location.int) #, scales = "free"
# r2.plot

# r2.m.plot <- ggplot(data=rule.2.m, aes(x=initial.cl, y=inc), notch=FALSE, position=dodge1, outlier.shape=NA)+
#   geom_point(colour="red")+
#   Theme1+
#   ylab("Relative Growth (mm)")+
#   xlab("Initial Carapace Length")+
#   ggtitle("Rule # 2: Fisher caught after 01/12/2018")+
#   theme(legend.position = "none")+
#   facet_wrap(. ~ Site.int)
# r2.m.plot

#Old Rule 3- not used
#Create loop plots for individual recaptures
### Plotting----
# setwd(plot.dir)

#Not used: Old Rule number 3----
# Loop plot to explore/find outliers/errors----
# 
# glimpse(dat.rr)
# #Change dir~to save plots
# setwd("~/workspace/Analysis_Miller_WRL/Plots")
# glimpse(dat.rr)
# 
# # #split data a part to create just a long list etc.  
# # initial.dat <- dat.rr%>%
#   select(Date, Tag.number, initial.cl, Site.int,Sex.int, Colour.int, Trip.int)%>%
#   dplyr::rename(Carapace.length=initial.cl, Site=Site.int,Sex=Sex.int, Colour = Colour.int, Trip=Trip.int)%>%
#   glimpse()
# 
# recap.dat <-dat.rr%>%
#   select(recap.Date, Tag.number, recap.cl, Site.rec, Sex.rec, Colour.rec, Trip.rec)%>%
#   dplyr::rename(Date=recap.Date, Carapace.length=recap.cl, Site=Site.rec, Sex=Sex.rec, Colour=Colour.rec, Trip=Trip.rec)%>%
#   glimpse()
# 
# 
# #combine data
# loop.rr <- rbind(initial.dat, recap.dat)
# glimpse(loop.rr)

#Or instead of doing above^
#Just read in 'growth.com' data from the '3.Recaptures.Data' script
# setwd("~/workspace/Analysis_Miller_WRL/Data")
# loop.rr<- read_csv("growth.comb.csv")%>%
#   glimpse()
# 
# #Filter out errors from Seven Mile data
# #Four tags that have negative growth (more than -7)
# #Filter out fisher returns errors
# loop.rr<-loop.rr%>%
#   filter(Tag.number!="190428" & Tag.number!="190188" & Tag.number!="190124" &Tag.number!="190443")%>%
#   filter(Tag.number!="K2400"&Tag.number!="K1617"&Tag.number!="K1221")%>%
#   #filter(Site!="Rivermouth")%>% #filter out Rivermouth recaptures <Don't do this! as it removes all NA's aswell
#   glimpse()
# 
# #Order data by tag number ~ easier to read
# 
# rule.4 <- loop.rr[order(loop.rr$Tag.number),]%>%
#   glimpse()
# 
# #save dataframe for Rule 4:
# 
# setwd(data.dir)
# write.csv(rule.4, "rule.4.csv")
# 
# # list of values to loop over
# tag.numbers = unique(dat.rr$Tag.number)

# Loop-turn on if you want to make multiple plots
# for (i in tag.numbers) {
#   
#   temp_plot = ggplot(data= subset(loop.rr, Tag.number == i), 
#                      aes(x = Date, y=Carapace.length )) + 
#     geom_point(size=3) +
#     geom_text(aes(label=Carapace.length), hjust=0.5, vjust=-1, size=3.5)+
#     geom_smooth(method="lm",se=FALSE, colour='black', size=0.5)+
#     ylim(min(loop.rr$Carapace.length),max(loop.rr$Carapace.length))+
#     scale_x_date(limits = c(min(loop.rr$Date),max(loop.rr$Date)), date_breaks = "1 month",labels=date_format("%b-%Y"))+
#     ylab("Carapace Length (mm)")+
#     xlab("Date")+
#     theme(axis.text.x = element_text(angle=90))+
#     theme(axis.line.x=element_line(colour="black", size=0.5,linetype='solid'))+
#     theme(axis.line.y=element_line(colour="black", size=0.5,linetype='solid'))+
#     ggtitle(i)
#   
#   ggsave(temp_plot, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
# }

#Example Plot

# test <-loop.rr %>%
#      glimpse()
# 
# tag.example <- loop.rr%>%
#   subset(Tag.number=='190444')%>%
#   glimpse()
# 
# 
# library(ggrepel)
# 
# plot.tag<- ggplot(data= tag.example, aes(x = Date, y=Carapace.length))+ 
#   geom_point(size=3) +
#   geom_text_repel(label=format(tag.example$Carapace.length, digits = 3), hjust=0.5, vjust=-1, size=3, nudge_y = 0.5)+
#   scale_color_manual(values=c("y" = "red", "n" = "black"))+
#   ylim(min(test$Carapace.length),max(test$Carapace.length))+
#   scale_x_date(limits = c(min(test$Date),max(test$Date)), date_breaks = "1 month",labels=date_format("%b-%Y"))+
#   geom_smooth( method=lm ,se=FALSE, colour='black', size=0.5)+
#   ylab("Carapace Length (mm)")+
#   xlab("Date")+
#   ggtitle("Individual 190444")+
#   theme(axis.text.x = element_text(angle=90))+
#   theme(axis.line.x=element_line(colour="black", size=0.5,linetype='solid'))+
#   theme(axis.line.y=element_line(colour="black", size=0.5,linetype='solid'))+
#   theme(panel.background=element_blank())
# plot.tag


#Read in the data----
# dat.r3<-gs_title("dat.rr")%>% # To use GoogleSheets
#   gs_read_csv(ws = "dat.rr" )%>%
#   glimpse()
# 
# dat.r3<-dat.r3%>%
#   mutate(inc=recap.cl-initial.cl)%>% #Makes column for growth increment
#   dplyr::mutate(diff=recap.Date-Date)%>%
#   mutate(R3.Moult=replace_na(R3.Moult, "N"))%>%
#   mutate(R3.Moult= str_replace_all(.$R3.Moult, c("Y"= "True", "N"= "False")))%>%
# #   glimpse()
# # 
# # #Maybe cut off is more likely to be 5mm plus?
# # #Filter out errors from Seven Mile data
# # #Four tags that have negative growth (more than -7)
# # #Filter out fisher returns errors
# # dat.r3<-dat.r3%>%
# #   filter(Tag.number!="190428" & Tag.number!="190188" & Tag.number!="190124" &Tag.number!="190443")%>%
# #   filter(Tag.number!="K2400"&Tag.number!="K1617"&Tag.number!="K1221")%>%
# #   filter(Site.int!="Rivermouth")%>% #filter out Rivermouth recaptures
# #   dplyr::rename(Moulted=R3.Moult)%>%
# #   glimpse()
# # 
# # 
# # length(dat.r3$Moulted[dat.r3$Moulted=="True"]) 
# # plyr::count(dat.r3$Moulted) #710 recapture lines
# #   
# # r3.plot <- ggplot(data=dat.r3, aes(x=initial.cl, y=inc, col=Moulted), notch= FALSE, position=dodge1, outlier.shape=NA)+
# #   #geom_smooth(method='lm', se=F, size=0.7)+
# #   geom_point(alpha=0.60)+ #alpha=0.7
# #   scale_color_manual(values = c("True"="red", "False"= "black"))+
# #   Theme1+
# #   theme(legend.key = element_rect(fill="white"))+
# #   ylab("Relative Growth (mm)")+
# #   xlab("Initial Carapace Length (mm)")+
# #   ggtitle("Rule # 3: Visual Growth") +
# #   facet_wrap(. ~ Site.int) #, scales = "free"
# # r3.plot
# # 
# #  #Plot only moulted indidviduals with Rule 3
# # rule.3.m <- dat.r3%>%
# #   subset(Moulted=="True")%>%
# #   glimpse()
# # 
# # #Number of moulted recaptures per site
# # rule.3.m%>%
# #   group_by(Site.int)%>%
# #   dplyr::summarise(Total=n())%>%
# #   glimpse()
# # 
# # #Cliff Head: 76
# # #Golden Ridge: 41
# # #Irwin Reef: 190
# # #Seven Mile: 403
# # 
# # r3.m.plot <- ggplot(data=rule.3.m, aes(x=initial.cl, y=inc), notch=FALSE, position=dodge1, outlier.shape=NA)+
# #   geom_point(colour="red")+
# #   Theme1+
# #   ylab("Relative Growth (mm)")+
# #   xlab("Initial Carapace Length")+
# #   ylim(0, 25)+
# #   ggtitle("Rule # 3: Visual growth")+
# #   theme(legend.position = "none") +
# #   facet_wrap(. ~ Site.int)
# # r3.m.plot
# # 
# # Old#Rule 4-----
# # 
# # # #Boxplot for Rule 4----
# # # glimpse(rule.4.m)
# # # 
# # # r4.box<-ggplot(data=rule.4.m,aes(x=Site, y=inc),notch=FALSE,position = dodge1, outlier.shape = NA)+
# # #   geom_point()+
# # #   geom_boxplot(outlier.color = NA, notch=FALSE)+
# # #   guides(fill=FALSE)+
# # #   theme(panel.background = element_blank(),axis.line = element_line(colour = "black"))+
# # #   stat_boxplot(geom='errorbar')+          # Adds error bars
# # #   stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ #this is adding the dot for the mean
# # #   theme_bw()+Theme1+
# # #   #coord_cartesian(ylim=c(0,3.5))+
# # #   ggtitle ("Rule 4: Moult")+
# # #   theme(plot.title = element_text(hjust = 0, size=12, face = "plain"))+
# # #   theme(axis.text.x = element_text(angle=90))+     #Changes angle of site names
# # #   ylab("Growth (mm)")+
# # #   xlab("Location") #+
# # #   #facet_grid(. ~ Sex) #Turn off if you want growth combined
# # # r4.box
# 
# #Read in the data----
# # dat.r4<-gs_title("Rule_4")%>% # To use GoogleSheets
# #   gs_read_csv(ws = "Sheet.1" )%>%
# #   glimpse()
# # 
# # 
# # dat.r4<-dat.r4%>%
# #   mutate(inc=recaptured.tag-initial.tag)%>%
# #   dplyr::rename(Moulted=R4)%>%
# #   mutate(Moulted=replace_na(Moulted, "N"))%>%
# #   glimpse()
#   
# 
# # dat.r4 <- dat.r4%>%
# #   mutate(inc=recaptured.tag-initial.tag)%>%
# #   filter(!is.na(Site))%>% #Remove for now but edit later
# #   #filter(!is.na(Tag.number))%>%
# #   dplyr::rename(Moulted=R4)%>%
# #   glimpse()
# # 
# # dat.r4<-dat.r4%>%
# #   mutate(Moulted=replace_na(Moulted, "N"))%>%
# #   mutate(Moulted=str_replace_all(.$Moulted,c("N"="False")))%>%
# #   mutate(Moulted=str_replace_all(.$Moulted,c("y"="True")))%>%
# #   glimpse()
# #   
# # #Subset to only moulted individuals
# # rule.4.m <- dat.r4%>%
# #   subset(Moulted=="y")%>%
# #   glimpse()
# # 
# # #Count how many moulted per site
# # rule.4.m%>%
# #   group_by(Site)%>%
# #   dplyr::summarise(Total=n())%>%
# #   glimpse()
# # 
# # 
# # r4.m.plot <- ggplot(data=rule.4.m, aes(x=initial.tag, y=inc), notch=FALSE, position=dodge1, outlier.shape=NA)+
# #   geom_point(colour="red")+
# #   Theme1+
# #   ylab("Relative Growth (mm)")+
# #   xlab("Initial Carapace Length")+
# #   ylim(0, 25)+
# #   ggtitle("Rule # 4: 3+ recaptures & Visual moult")+
# #   theme(legend.position = "none") #+
# #   #facet_wrap(. ~ Site)
# # r4.m.plot
# # 
# # #Need to filter out summer months i.e. Seven Mile Jan-April
# # 
# # r4.edit<-dat.r4%>%
# #    # mutate(year=format(as.Date(Date),'%Y')) %>%
# #    # mutate(month=format(as.Date(Date),'%m'))%>%
# #   mutate(month=month((as_date(Date))))%>%
# #    glimpse()
# # 
# # r4.edit<-r4.edit%>%
# #   filter(month%in%c(5:12))%>%
# #   glimpse()
# # 
# # unique(r4.edit$month)
# # 
# # #boxplot
# # r4.box.edit<-ggplot(data=r4.edit,aes(x=Site, y=inc),notch=FALSE,position = dodge1, outlier.shape = NA)+
# #   geom_jitter(width = 0.1, height = NULL)+
# #   # geom_boxplot(outlier.color = NA, notch=FALSE)+
# #   guides(fill=FALSE)+
# #   theme(panel.background = element_blank(),axis.line = element_line(colour = "black"))+
# #   # stat_boxplot(geom='errorbar')+          # Adds error bars
# #   stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ #this is adding the dot for the mean
# #   theme_bw()+Theme1+
# #   #coord_cartesian(ylim=c(0,3.5))+
# #   ggtitle ("Rule 4: Moult")+
# #   theme(plot.title = element_text(hjust = 0, size=12, face = "plain"))+
# #   theme(axis.text.x = element_text(angle=90))+     #Changes angle of site names
# #   ylab("Growth (mm)")+
# #   xlab("Location") #+
# # #facet_grid(. ~ Sex) #Turn off if you want growth combined
# # r4.box.edit
# 
# #simons
# #rec[rec$Lyrs>0.2 & rec$Lyrs<2.3 & rec$rlloc=='smb' & rec$rlmonth%in%5:11,]
# 
# 
# 
# #Rule "5" New----
# #More than 3 recaptures
# #And max times between recaps 3 months
# 
# #Read in the data----
dat.r5<-gs_title("r.5.moult")%>% # To use GoogleSheets
  gs_read_csv(ws = "r.5.moult" )%>%
  glimpse()

dat.r5<-dat.r5%>%
  mutate(inc=recap.cl-initial.cl)%>% #Makes column for growth increment
  dplyr::rename(Location.int=Site, Site= mini.site, n1=X8, n2=X10)%>%
  select(Trip, Date, Tag.number, initial.cl,n1, recap.cl, n2, Location.int, Sex, Colour, Total.damage, Site, inc)%>%
  glimpse()
 
# #Filter down to only moulted individuals
rule.5.m <- dat.r5%>%
  filter(!is.na(initial.cl))%>%
  mutate(Site=str_replace_all(Site, c("Jim Bailey" = "White Point", "Whitepoint"="White Point")))%>%
  glimpse()

unique(rule.5.m$Site)
 
# #Save Rule 5
setwd(data.dir)
write.csv(rule.5.m, "Rule.5.csv")
# 
# #Number of moulted recaptures per site
# rule.5.m%>%
#   group_by(Location.int)%>%
#   dplyr::summarise(Total=n())%>%
#   glimpse()
# 
# 
# #Combine Rules 1, 2 & 5----
# glimpse(rule.1.m)
#
R1<-read_csv("Rule.1.csv")%>%
  mutate(Moulted=str_replace_all(.$Moulted,c("True"="1")))%>%
  dplyr::rename(Location=Location.int, Rule=Moulted, Site=mini.site.x)%>%
  select(Date, Tag.number, initial.cl,Location, Sex.int, Colour.int, recap.cl, inc, Rule, diff, Site)%>% #, diff
  filter(Tag.number!="198821" & Tag.number!="K3814")%>%
  glimpse()

# #To remove > 3 months liberty
R1 <- R1%>%
  dplyr::filter(!diff>90)%>%
  select(Date, Tag.number, initial.cl,Location, Sex.int, Colour.int, recap.cl, inc, Rule, Site)%>%
  glimpse()

glimpse(rule.2.m)

R2<-read_csv("Rule.2.csv")%>%
  mutate(Moulted="2")%>%
  dplyr::rename(Location=Location.int, Rule=Moulted, Site=mini.site.x)%>%
  select(Date, Tag.number, initial.cl,Location, Sex.int, Colour.int, recap.cl, inc,diff,  Rule, Site)%>% #, diff
  glimpse()

R2<- R2%>%
 dplyr::filter(!diff>90)%>%
 select(Date, Tag.number, initial.cl,Location, Sex.int, Colour.int, recap.cl, inc, Rule, Site)%>%
 glimpse()

R5<-read_csv("Rule.5.csv")%>%
  mutate(Rule="3")%>%
  dplyr::rename(Location=Location.int, Sex.int=Sex, Colour.int=Colour)%>%
  select(Date, Tag.number, initial.cl,Location, Sex.int, Colour.int, recap.cl, inc, Rule, Site)%>%
  glimpse()

R125 <- rbind(R1, R2, R5)%>%
  filter(!Sex.int=="UNKNOWN")%>%
  glimpse()

R15<-rbind(R1, R5)%>%
  filter(!Sex.int=="UNKNOWN")%>%
  glimpse()

setwd(data.dir)
write.csv(R125, "R125.csv")
write.csv(R15, "R15.filtered.csv")


#Bring in Data----

setwd("~/workspace/Analysis_Miller_WRL/Data")
#Data notfiltered to 3 months 
R125<-read_csv("R125.csv")%>%
glimpse()

#Data filtered to 3 months
R15<-read_csv("R15.filtered.csv")%>%
glimpse()
  
R125%>%
  group_by(Location)%>%
  dplyr::summarise(Total=n())%>%
  glimpse()


#Plot Rules----

r125.plot <- ggplot(data=R125, aes(x=initial.cl, y=inc, col=Rule), notch=FALSE, position=dodge1, outlier.shape=NA)+
  geom_point()+ #colour="red"
  scale_color_manual(values = c("3"="plum","1"="red", "2"= "blue"))+
  Theme1+
  theme(legend.key = element_rect(fill="white"))+
  ylab("Relative Growth (mm)")+
  xlab("Initial Carapace Length")+
  ylim(0, 18)+
  #ggtitle("'Business rules' 1 & 3") + 
  #theme(legend.position = "none") +
  facet_wrap(. ~ Location)
r125.plot

#Boxplot for Rules 1,2 &5
R125$Location<-factor(R125$Location, levels = c("Seven Mile", "Irwin Reef", "Cliff Head", "Golden Ridge"))
glimpse(R125)
r125.box<-ggplot(data= R125 ,aes(x=Location, y=inc),notch=FALSE,position = dodge1, outlier.shape = NA)+
  geom_boxplot(outlier.color = NA, notch=FALSE)+
  geom_jitter(width = 0.1, height = NULL, alpha=0.5)+
  guides(fill=FALSE)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"))+
  # stat_boxplot(geom='errorbar')+          # Adds error bars
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ #this is adding the dot for the mean
  theme_bw()+Theme1+
  #coord_cartesian(ylim=c(0,3.5))+
  ylim(0, 15)+
  #ggtitle ("Busniness 'rules' 1 & 3")+
  theme(plot.title = element_text(hjust = 0, size=12, face = "plain"))+
  theme(axis.text.x = element_text(angle=90))+     #Changes angle of site names
  ylab("Growth (mm)")+
  xlab("Location") +
  facet_grid(. ~ Sex.int) #Turn off if you want growth combined
r125.box


#Model----

mod.1 <- lmer(inc~Location+Sex.int+initial.cl+ (1|Site/Location), data=R15)
summary(mod.1)
anova(mod.1)
pair.mod1<-lsmeans(mod.1,pairwise~Location)
summary(pair.mod1)

mod.2 <- lmer(inc~Location+Sex.int+initial.cl+ Rule+ (1|Site/Location), data=R15)
summary(mod.2)
anova(mod.2)
pair.mod2<-lsmeans(mod.2,pairwise~Location)
summary(pair.mod2)

#Error with scaling for unfiltered data
mod.3 <- lmer(inc~Location+Sex.int+I(initial.cl/3)+ (1|Site/Location), data=R125)
summary(mod.3)
anova(mod.3)
pair.mod3<-lsmeans(mod.3,pairwise~Location)
summary(pair.mod3)


#no error with 'scaling'
# #Explore pair-wise comparisions
# mod.1.em <- emmeans(mod.1, "Location")
# summary(mod.1.em)
# plot(mod.1.em, comparisons=TRUE)
# pairs(mod.1.em)
# emmeans::pwpp(mod.1.em)
# pwpp(mod.1.em) #doesn't work for this version of R
# ??pwpp






# mod.2 <- glmer(inc~Location+Sex.int+I(initial.cl/10)+ (1|Site/Location), data=R125)
# summary(mod.2)








# # Simons's filters----
# rr1 %<>% 
#   # mutate(month=as.numeric(format(as.Date(Date),'%m')), year=as.numeric(format(as.Date(Date),'%Y')))%>%
#   filter(!Trip.x=="T0")%>%
#   filter(!Trip.y=="T9")%>%
#   mutate(Dec.tagg=ifelse(`Trip.x`%in%c("T7"),"late.nov",ifelse(`Trip.x`%in%c("T8"),"dec","reds")))%>%
#   mutate(Damage=ifelse(`Total.damage.x`==0,"none","some"))%>%
#   mutate(count=1)%>%
#   
#   glimpse()
# 
# glimpse(rr1)         
# 
# # , col=Moulted
# increase.plot <- ggplot(data=rr1%>%filter(Dec.tagg=="reds"&!Site.x=="Seven Mile"), aes(x=inc/Initial.cl), pch=16, cex=.6)+
#   
#   # increase.plot <- ggplot(data=rr1, aes(x=inc/Initial.cl), pch=16, cex=.6)+
#   # geom_density(stat="identity",alpha=0.5)+
#   # geom_density(alpha=0.5)+
#   geom_histogram()+
#   scale_color_manual(values=c("TRUE" = "red", "FALSE"= "black"))+ #, "FALSE" = "black", "NA" = "black"
#   Theme1+
#   # ylab("Growth (mm) before recapture")+
#   # xlab("Initial carapace length (mm)")+
#   xlim(0,0.2)+
#   facet_grid(Damage~Site.x,scales="free") # , scales = "free"
# increase.plot
# 
# 
# #Icorporate Time- Tims plot----
# 
# Time.plot <- ggplot(data=rr1, aes(x=diff, y=inc, col=Moulted), notch= FALSE, position=dodge1, outlier.shape=NA)+
#   #geom_smooth(method='lm', se=F, size=0.7)+
#   geom_point()+
#   scale_color_manual(values = c("TRUE"="red", "FALSE"= "black"))+
#   Theme1+
#   ylab("Growth (mm) before Recapture")+
#   xlab("Time between Recaptures (Days)")+
#   facet_wrap(. ~ Site.x, scales = "free") #
# Time.plot