
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

# Study name----
study<-"Reds.Catch"

# Set work directory----


# work.dir=("~/Google Drive/Projects/Project_WRL low-catch zone/Fieldwork and Reporting/03_Trapping/Analysis_WRL_Reds_2018") #For Tim
#work.dir=("~/Google Drive/Project_WRL low-catch zone/Fieldwork and Reporting/02_Diving/Analysis_WRL_Dive_Surveys") #For Ash..you sure? dive surveys?

work.dir=("~/Google Drive/Projects/Project_WRL low-catch zone/Fieldwork and Reporting/03_Trapping/Analysis_WRL_Reds_2018")

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
# setwd(data.dir) <- didn't work

setwd("~/Google Drive/Analysis_WRL_Reds_2018/Data")

dir()



# Import length data----
dat.length.gsheet <- gs_title("Lobsters_data_2018_Reds_20180511")%>%
  gs_read_csv(ws = "Lobster.var")%>%
  glimpse()



dat.length<-dat.length.gsheet%>%
  # mutate(Count=1)%>%
  # filter(!Carapace.length%in%c("UNKNOWN"))%>%
  # filter(!is.na(Carapace.length))%>%
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

#'ID' not found- 'ID' changed to 'Trap.ID' to work- AM

write.csv(dat.length,"lob.dat.csv")
# dat.length<-read.csv("lob.dat.csv")
dat.length<-read.csv("lob.dat.csv")

# Checks---
summary(dat.length$Carapace.length)
length(dat.length$Carapace.length)
# 569
length(unique(dat.length$day.site.trap)) 
# 92


# # Import pot data----
# dat.pot<-gs_title("Lobsters_data_20180214")%>%
#   gs_read_csv(ws = "Pot.var")%>%
#   mutate(day.trap=paste(Day,Trap.number,sep="."))%>%
#   dplyr::rename(Site=Group.site)%>% #TJL changed here to make consistent! 
#   mutate(Site=str_replace_all(.$Site,c("SM"="Seven Mile","CHin"="Cliff Head IN","CHout" = "Cliff Head OUT", "WP"="Whitepoint", "RM"="River Mouth")))%>% #AM added CHout and RM
#   mutate(Site.Name=fct_relevel(Site,"Seven Mile Beach", "Whitepoint"))%>%
# mutate(John.site.names=fct_relevel(John.site.names,"Seven Miles 5","Seven Miles 8","Seven Miles 10","Seven Miles 17"))%>%
   
#  mutate(Pot.type = fct_recode(Pot.type,
#                                "Commercial pots\nclosed-gap" = "C",
#                               "Fisheries pots\nclose battened" = "F"))%>%
#   filter(Exclude.pots=="NO")%>% # gets rid of 17 pots - only 4 crays?
#   filter(Repeated.samples=="1")%>%
#   glimpse()
 
# write.csv(dat.pot,"dat.pot.csv")
# dat.pot<-read.csv("dat.pot.csv")

# Checks---
# unique(dat.pot$Exclude.pots)
# unique(dat.pot$day.trap)
# length(unique(dat.pot$day.trap))

  
# 
# 
# # Make Hist data and Summarise the length data to abundance for the all/legal/sublegal----
# 
# Make factors----
dat.factors<-dat.length%>%
  select(Trap.ID,Trap.Number,Day,Site,Date)%>% 
  distinct()%>%
  glimpse()

# 
# # # Make histogram data----
dat.hist<-dat.length%>%
   left_join(dat.factors,by=c("Trap.ID"))%>%
   #filter(!is.na(Site))%>% # Didn't work, not sure if we need this line anyway-AM.
   glimpse()


# Make abundance of all----
dat.all<-dat.length%>%
  group_by(Trap.ID)%>%
  summarize(n())%>%
  rename(Abundance="n()")%>%
  right_join(dat.factors,by=c("Trap.ID"))%>%
  replace_na(list(Abundance = 0))%>%
  mutate(Legal="All")%>%
  glimpse()

# Checks--
summary(dat.all$Abundance)

# Make abundance of legal/sublegal----
dat.sublegal<-dat.length%>%
  filter(Carapace.length<76)%>%
  group_by(Trap.ID)%>%
  summarize(n())%>%
  rename(Abundance="n()")%>%
  right_join(dat.factors,by=c("Trap.ID"))%>%
  replace_na(list(Abundance = 0))%>%
  mutate(Legal="Sublegal")%>%
  glimpse()

dat.legal<-dat.length%>%
  filter(Carapace.length>=76)%>%
  group_by(Trap.ID)%>%
  summarize(n())%>%
  rename(Abundance="n()")%>%
  right_join(dat.factors,by=c("Trap.ID"))%>%
  replace_na(list(Abundance = 0))%>%
  mutate(Legal="Legal")%>%
  bind_rows(dat.sublegal,dat.all)%>%
  glimpse()

# Check--
unique(dat.legal$Legal)


###
# Plots-----
#setwd(plots.dir) #Didn't work for AM, used line below.
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
    text=element_text(size=12),
    strip.text.y = element_text(size = 12,angle = 270),
    axis.title.x=element_text(vjust=0.3, size=12),
    axis.title.y=element_text(vjust=0.6, angle=90, size=12),
    axis.text.x=element_text(size=12,colour="black"),
    axis.text.y=element_text(size=12,colour="black"),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.ticks.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())


### Histograms ----

hist.location<-ggplot(data = dat.length, aes(x = Carapace.length)) + 
  geom_histogram( aes(y = ..density..),binwidth=5,fill="white",colour="black") + 
  scale_y_continuous(name='Relative density',breaks = seq(-0.08,0.08,0.02),labels=abs(seq(-8,8,2))) +
  # coord_flip()+
  Theme1+
  xlab("Carapace length (mm)")+
  geom_hline(aes(yintercept=0))+
  facet_grid(Site~.)+
  theme(axis.line.x=element_line(colour="white", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="white", size=0.5,linetype='solid'))+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
hist.location

ggsave(hist.location,file=paste(study,"hist.location", "png",sep = "."), width = 10, height = 15,units = "cm")


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
 






# Bar plots----


# Barplot of number----
summary(dat.all$Abundance)

bar.location.all.jitter<-ggplot(data = dat.all, aes(x = Site,y=Abundance)) + 
  stat_summary(fun.y=mean, geom="bar",fill="white",colour="black") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  geom_jitter(width = 0.25, height = 0,alpha=0.25)+
  geom_hline(aes(yintercept=0))+
  xlab("")+
  ylab("Lobster per pot (+/- SE)")+
  Theme1+
  theme(axis.line.x=element_line(colour="white", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="white", size=0.5,linetype='solid'))
bar.location.all.jitter
ggsave(bar.location.all.jitter,file=paste(study,"bar.location.all.jitter", "png",sep = "."), width = 15, height = 15,units = "cm")


bar.location.all<-ggplot(data = dat.all, aes(x = Site,y=Abundance)) + 
  stat_summary(fun.y=mean, geom="bar",fill="white",colour="black") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  geom_hline(aes(yintercept=0))+
  xlab("")+
  ylab("Lobster per pot (+/- SE)")+
  Theme1+
  theme(axis.line.x=element_line(colour="white", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="white", size=0.5,linetype='solid'))
bar.location.all
ggsave(bar.location.all,file=paste(study,"bar.location.all", "png",sep = "."), width = 15, height = 15,units = "cm")


bar.location.legal<-ggplot(data = dat.legal, aes(x = Site,y=Abundance)) + 
  stat_summary(fun.y=mean, geom="bar",fill="white",colour="black") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  geom_hline(aes(yintercept=0))+
  xlab("")+
  ylab("Lobster per pot (+/- SE)")+
  Theme1+
  theme(axis.line.x=element_line(colour="white", size=0.5,linetype='solid'),
        axis.ticks.x=element_line(colour="white", size=0.5,linetype='solid'))+
  facet_grid(Legal~.)
bar.location.legal
ggsave(bar.location.legal,file=paste(study,"bar.location.legal", "png",sep = "."), width = 15, height = 15,units = "cm")








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


