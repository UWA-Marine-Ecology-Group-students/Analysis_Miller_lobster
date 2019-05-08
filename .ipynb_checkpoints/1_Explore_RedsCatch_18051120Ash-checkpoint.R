
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
library(stringr)
library(rgdal)

# Study name----
study<-"Reds.Catch"

# Set work directory----


# work.dir=("~/Google Drive/Projects/Project_WRL low-catch zone/Fieldwork and Reporting/03_Trapping/Analysis_WRL_Reds_2018") #For Tim
#work.dir=("C:/Users/00097191/Google Drive/MEG/Project_WRL low-catch zone/Fieldwork and Reporting/03_Trapping/Analysis_WRL_Reds_2018")# For Brooke
work.dir=("~/Google Drive/Projects/Project_WRL low-catch zone/Fieldwork and Reporting/03_Trapping/Analysis_WRL_Reds_2018") #For Ash

# Set sub directories----
data.dir=paste(work.dir,"Data",sep="/")
plots.dir=paste(work.dir,"Plots",sep="/")

# Functions----
se <- function(x) sd(x) / sqrt(length(x))
se.min <- function(x) (mean(x)) - se(x)
se.max <- function(x) (mean(x)) + se(x)
sd.min <- function(x) (mean(x)) - sd(x)
sd.max <- function(x) (mean(x)) + sd(x)
scaleFUN <- function(x) sprintf("%.0f", x)



# Import and make data----
#setwd(data.dir)
setwd("~/Google Drive/Analysis_WRL_Reds_2018/Data")
dir()

# Import length data----
dat.length.gsheet <- gs_title("Lobsters_data_2018_Reds_20180511")%>%
  gs_read_csv(ws = "Lobster.var")%>%
  glimpse()

dat.length<-dat.length.gsheet%>%
  mutate(Count=1)%>%
  # filter(!Carapace.length%in%c("UNKNOWN"))%>%
  filter(!is.na(Carapace.length))%>%
  # filter(!Colour%in%c("UNKNOWN"))%>%
  mutate(Carapace.length=as.numeric(Carapace.length))%>%
  mutate(Trap.Number.2=as.numeric(str_extract(Trap.ID, "[0-9]+")))%>%
  mutate(Site=(str_extract(Trap.ID, "[aA-zZ]+")))%>%
  mutate(day.site.trap=paste(Day,Site,Trap.Number,sep="."))%>%
  mutate(Site = fct_recode(Site,
                               "Little Horseshoe" = "LH",
                               "Cliff Head OUT" = "CHOUT",
                           "Cliff Head IN" = "CHIN",
                           "White Point" = "WP",
                           "Seven Mile" = "SM",
                           "River Mouth" = "RM"))%>%
  mutate(Site=fct_relevel(Site,"Seven Mile","River Mouth","White Point","Little Horseshoe","Cliff Head OUT","Cliff Head IN"))%>%
  glimpse()

write.csv(dat.length,"lob.dat.csv")
dat.length<-read.csv("lob.dat.csv")

unique(dat.length$Trap.ID) #178 

# Checks---
summary(dat.length$Carapace.length)
length(dat.length$Carapace.length)
# 2220
length(unique(dat.length$day.site.trap)) 
# 260


# # Import pot data----
dat.pot <-gs_title("Lobsters_data_2018_Reds_20180511")%>%
   gs_read_csv(ws = "Pot.var")%>%
   mutate(day.trap=paste(Day,Pot.Number,sep="."))%>%
   mutate(Site.Name=str_replace_all(.$Site.Name,c("SM"="Seven Mile","CHin"="Cliff Head IN","CHout" = "Cliff Head OUT", "WP"="Whitepoint", "RM"="River Mouth")))%>% #AM added CHout and RM
   mutate(Trap.ID=str_replace_all(.$Trap.ID, c("out"="OUT", "in"="IN")))%>%
  mutate(Site.Name=fct_relevel(Site.Name,"Seven Mile", "Whitepoint"))%>%
   glimpse()

write.csv(dat.pot,"dat.pot.csv")
dat.pot<-read.csv("dat.pot.csv")
unique(dat.pot$Trap.ID) #94

# Checks---
 unique(dat.pot$Exclude.pots)
 unique(dat.pot$day.trap) #NA
 length(unique(dat.pot$day.trap)) #250

# # Make Hist data and Summarise the length data to abundance for the all/legal/sublegal----

# Make factors----
dat.factors<-dat.pot%>%
  select(Trap.ID,Pot.Number,Day,Site.Name,Date.Recovered)%>% 
  distinct()%>%
  glimpse()

unique(dat.factors$Trap.ID)
names(dat.pot) 
# # # Make histogram data----

dat.hist<-dat.length%>%
   left_join(dat.factors,by=c("Trap.ID"))%>%
   #filter(!is.na(Site))%>% # Didn't work, not sure if we need this line-AM.
   glimpse()

check<-dat.length%>% # this will be any that in lengths but not in pot.var
  dplyr::select(Trap.ID,Day)%>%
  anti_join(.,dat.pot)
 
# Make abundance of all----
dat.all<-dat.length%>%
  group_by(Trap.ID)%>%
  dplyr::summarize(Abundance=n()) %>%
  right_join(dat.factors,by=c("Trap.ID"))%>%
  replace_na(list(Abundance = 0))%>%
  mutate(Legal="All")%>%
   glimpse()


# Checks--
summary(dat.all$Abundance)
dat.all$Abundance

# Make abundance of legal/sublegal----
dat.sublegal<-dat.length%>%
  filter(Carapace.length<76)%>%
  group_by(Trap.ID)%>%
  summarize(Abundance=n())%>%
  right_join(dat.factors,by=c("Trap.ID"))%>%
  replace_na(list(Abundance = 0))%>%
  mutate(Legal="Sublegal")%>%
  glimpse()

dat.legal<-dat.length%>%
  filter(Carapace.length>76)%>%
  group_by(Trap.ID)%>%
  summarize(Abundance=n())%>%
  #rename(Abundance="n()")%>%
  right_join(dat.factors,by=c("Trap.ID"))%>%
  replace_na(list(Abundance = 0))%>%
  mutate(Legal="Legal")%>%
  bind_rows(dat.sublegal,dat.all)%>%
  glimpse()

# Check--
unique(dat.legal$Legal)


###
# Plots-----
#setwd(plots.dir) 
setwd("~/Google Drive/Analysis_WRL_Reds_2018/Plots")


# Plotting Themes ----
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=12),
    #legend.title = element_blank(),
    #legend.position = c(0.4, 0.8),
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
    plot.background = element_blank()) # Brooke added


### Histograms ----
dat.all$Site.Name<-factor(dat.all$Site.Name, levels = c("SM","RM", "IR","LR","SR","WL","SD","LH","WP","CHout(2)","CHout(1)","DM","CHin(1)", "CHin(2)","GR"))


hist.location<-ggplot(data = dat.length, aes(x = Carapace.length)) + 
  geom_histogram( aes(y = ..density..),binwidth=5,fill="white",colour="black") + 
  #scale_y_continuous(name='Relative density',breaks = seq(-0.08,0.08,0.02),labels=abs(seq(-8,8,2))) +
  # coord_flip()+
  Theme1+
  xlab("Carapace length (mm)")+
  geom_hline(aes(yintercept=0))+
  facet_grid(Site~.)+
  theme(strip.text.y = element_text(angle=360))+
  theme(axis.line.x=element_line(colour="white", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="white", size=0.5,linetype='solid'))+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
hist.location

#ggsave(hist.location,file=paste(study,"hist.location", "png",sep = "."), width = 10, height = 15,units = "cm")


#
# hist.location.stage<-ggplot(data = dat.hist, aes(x = Carapace.length, fill = Colour)) + 
#   geom_histogram(data = dplyr::filter(dat.hist, Colour == "White"), aes(y = ..density..),col="grey30",binwidth=5) + 
#   geom_histogram(data = dplyr::filter(dat.hist, Colour == "Red"), aes(y = -..density..),col="grey30",binwidth=5) +
#   scale_y_continuous(name='Relative density',breaks = seq(-0.08,0.08,0.02),labels=abs(seq(-8,8,2))) +
#    coord_flip()+
#   Theme1+
#   xlab("Carapace length (mm)")+
#   scale_fill_manual(values = c("red","white"),name="Lobster colouration",labels=c("Red","White"))+
#   geom_hline(aes(yintercept=0))+
#   facet_grid(Location~.)+
#   theme(legend.position="top")+
#   theme(axis.text.y=element_blank(),
#         axis.ticks.y=element_blank())
# hist.location.stage
 
# ggsave(hist.location.stage,file=paste(study,"hist.location.stage", "png",sep = "."), width = 10, height = 15,units = "cm")

length.grouped<-dat.length%>%
  mutate(length=ifelse(Carapace.length>=27.5&Carapace.length<32.5,30,
                ifelse(Carapace.length>=32.5&Carapace.length<37.5,35,
                ifelse(Carapace.length>=37.5&Carapace.length<42.5,40,
                ifelse(Carapace.length>=42.5&Carapace.length<47.5,45,
                ifelse(Carapace.length>=47.5&Carapace.length<52.5,50,
                ifelse(Carapace.length>=52.5&Carapace.length<57.5,55,
                ifelse(Carapace.length>=57.5&Carapace.length<62.5,60,
                ifelse(Carapace.length>=62.5&Carapace.length<67.5,65,
                ifelse(Carapace.length>=67.5&Carapace.length<72.5,70,
                ifelse(Carapace.length>=72.5&Carapace.length<77.5,75,
                ifelse(Carapace.length>=77.5&Carapace.length<82.5,80,
                ifelse(Carapace.length>=82.5&Carapace.length<87.5,85,
                ifelse(Carapace.length>=77.5&Carapace.length<92.5,90,
                Carapace.length))))))))))))))%>%
  group_by(Trap.ID,length)%>%
  dplyr::summarise(count=n())%>%
  ungroup()

dat.add<-dat.pot%>%
  dplyr::select(Trap.ID,Site.Name)

length.freq<-right_join(length.grouped,dat.add)%>%
  complete(length, nesting(Site.Name, Trap.ID),fill = list(count = 0))%>%
  filter(!is.na(length))

bar.test<-ggplot(data = length.freq, aes(x = length,y=count)) + 
  stat_summary(fun.y=mean, geom="bar",fill="white",colour="black") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  geom_hline(aes(yintercept=0))+
  xlab("Carapace length (mm)")+
  ylab("Lobster per pot (+/- SE)")+
  #theme_bw()+
  Theme1+ # Brooke added bw
  theme(axis.line.x=element_line(colour="white", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="white", size=0.5,linetype='solid'))+
  facet_grid(Site.Name~.,scales = "free_y")
bar.test

#ggsave(bar.test,file=paste(study,"hist.size", "png",sep = "."), width = 10, height = 15,units = "cm")

summary(dat.length)
names(dat.pot)
unique(dat.length$Carapace.length)

# Bar plots----

#dat.all$Site.Name<-factor(dat.all$Site.Name, levels = c("Cliff Head IN","Cliff Head OUT", "LH","Whitepoint", "River Mouth","Seven Mile"))

# Barplot of number----
summary(dat.all$Abundance)

bar.location.all.jitter<-ggplot(data = dat.all, aes(x = Site.Name,y=Abundance)) + 
  stat_summary(fun.y=mean, geom="bar",fill="white",colour="black") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  geom_jitter(width = 0.25, height = 0,alpha=0.25)+
  geom_hline(aes(yintercept=0))+
  xlab("")+
  ylab("Lobster per pot (+/- SE)")+
  theme_bw()+Theme1+ # Brooke added bw
  theme(axis.text.x = element_text(angle=45))+ # AM added
  theme(axis.line.x=element_line(colour="white", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="white", size=0.5,linetype='solid'))
  
bar.location.all.jitter
#ggsave(bar.location.all.jitter,file=paste(study,"bar.location.all.jitter", "png",sep = "."), width = 15, height = 15,units = "cm")


bar.location.all<-ggplot(data = dat.all, aes(x = Site.Name,y=Abundance)) + 
  stat_summary(fun.y=mean, geom="bar",fill="white",colour="black") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  geom_hline(aes(yintercept=0))+
  xlab("")+
  ylab("Lobster per pot (+/- SE)")+
  theme_bw()+Theme1+ # Brooke added bw
  theme(axis.line.x=element_line(colour="white", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="white", size=0.5,linetype='solid'))
bar.location.all
#ggsave(bar.location.all,file=paste(study,"bar.location.all", "png",sep = "."), width = 15, height = 15,units = "cm")

#dat.legal$Site.Name<-factor(dat.all$Site.Name, levels = c("Cliff Head IN","Cliff Head OUT", "LH","Whitepoint", "River Mouth","Seven Mile"))

bar.location.legal<-ggplot(data = dat.legal, aes(x = Site.Name,y=Abundance)) + 
  stat_summary(fun.y=mean, geom="bar",fill="white",colour="black") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  geom_hline(aes(yintercept=0))+
  xlab("")+
  ylab("Lobster per pot (+/- SE)")+
  theme_bw()+Theme1+ # Brooke added bw
  theme(axis.line.x=element_line(colour="white", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="white", size=0.5,linetype='solid'))+
  facet_grid(Legal~.)
bar.location.legal
#ggsave(bar.location.legal,file=paste(study,"bar.location.legal", "png",sep = "."), width = 15, height = 15,units = "cm")

#Remove all- Sort by year. free y axis

#dat.legal$Site.Name<-factor(dat.all$Site.Name, levels = c("Cliff Head IN","Cliff Head OUT", "LH","Whitepoint", "River Mouth","Seven Mile"))

bar.location.legal<-ggplot(data = dat.legal, aes(x = Site.Name,y=Abundance)) + 
  stat_summary(fun.y=mean, geom="bar",fill="white",colour="black") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  geom_hline(aes(yintercept=0))+
  xlab("")+
  ylab("Lobster per pot (+/- SE)")+
  theme_bw()+Theme1+ # Brooke added bw
  theme(axis.line.x=element_line(colour="white", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="white", size=0.5,linetype='solid'))+
  facet_grid(Legal~.)
bar.location.legal
#ggsave(bar.location.legal,file=paste(study,"bar.location.legal", "png",sep = "."), width = 15, height = 15,units = "cm")








# plot of mean length ----
glimpse(dat.length) #AM changed 'dat' to 'dat.length'

bar.length.location<-ggplot(data = dat.length, aes(x = Site,y=Carapace.length)) + 
  geom_point(alpha=0.05)+
  stat_summary(fun.y=mean, geom="point",fill="white",colour="red",size=3) +
  stat_summary(fun.ymin = sd.min, fun.ymax = sd.max, geom = "errorbar", width = 0.3,colour="red") +
  geom_hline(aes(yintercept=20))+
  xlab("")+ 
  ylab("Carapace length (+/- SD)")+
  Theme1+
  theme(axis.line.x=element_line(colour="white", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="white", size=0.5,linetype='solid'))
bar.length.location
ggsave(bar.length.location,file=paste(study,"bar.length.location", "png",sep = "."), width = 15, height = 15,units = "cm")

### Proportioned recapture month 1 #######

dat<-dat.length%>%
  select(Trap.ID,Recapture,Count)%>%
  full_join(dat.pot)%>%
  select(Trap.ID,Recapture,Count,Site.Name)%>%
  replace_na(list(Count = 0,Recapture="FALSE"))

dat.recap<-dat%>%
  filter(Recapture=="TRUE")%>%
  group_by(Site.Name,Trap.ID)%>%
  dplyr::summarise(total.recap=sum(Count))%>%
  ungroup()%>%
  full_join(dat.pot)%>%
  select(Trap.ID,Site.Name,total.recap)%>%
  replace_na(list(total.recap = 0))

dat.total<-dat%>%
  group_by(Site.Name,Trap.ID)%>%
  dplyr::summarise(total=sum(Count))

dat.recap1<-full_join(dat.recap,dat.total)
dat.recap1$prop <- dat.recap1$total.recap/dat.recap1$total
dat.recap1$prop[is.nan(dat.recap1$prop)] <-0
dat.prop<-aggregate(dat.recap1$prop, by=list(Site.Name=dat.recap1$Site.Name), FUN=sum)
names(dat.prop)[2]<- 'total.prop'
names(dat.prop) #seven Mile says 1.16 proportion recaptured.

##Plot
setwd(plots.dir)
dat.prop$Site.Name<-factor(dat.prop$Site.Name, levels = c("Cliff Head IN","Cliff Head OUT", "LH","Whitepoint", "River Mouth","Seven Mile"))

bar.total.prop<-ggplot(data = dat.recap, aes(x = Site.Name,y=prop)) + 
  stat_summary(fun.y=mean, geom="bar",fill="white",colour="black") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  geom_hline(aes(yintercept=0))+
  xlab("")+
  ylab("Proportion recaptured")+
  theme_bw()+Theme1+ 
  theme(axis.line.x=element_line(colour="white", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="white", size=0.5,linetype='solid'))
bar.total.prop

ggsave(bar.total.prop,file=paste(study,"bar.total.prop-draft-BG", "png",sep = "."), width = 18, height = 15,units = "cm")


######### Table ##########


dat.recap2<-full_join(dat.recap,dat.total)
#dat.recap2<-dat.recap2
  #mutate(Site.Name=fct_relevel(Site.Name,"Seven Mile","River Mouth","Whitepoint","LH","Cliff Head OUT","Cliff Head IN"))
#dat.recap2 <- as.data.frame(dat.recap2)
dat.recap.table <-aggregate(list(dat.recap2$total,dat.recap2$total.recap), by=list(dat.recap2$Site.Name)FUN = sum))
names(dat.recap.table)  
names(dat.recap.table)[1] <- "Site"
names(dat.recap.table)[2] <- "released"
names(dat.recap.table)[3] <- "recaptured"

library(janitor)
dat.recap.table.total <- dat.recap.table %>%
  adorn_totals("row")

summary_table(dat.recap.table.total)

#....to be continued
