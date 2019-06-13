# Part 3 - plots of the most parsimonious models----
rm(list=ls())
### now  make a nice plot of the most interesting models-----
library(gridExtra)
library(ggplot2)
library(grid)
detach("package:plyr", unload=TRUE)#will error - don't worry
library(tidyr)
library(dplyr)
options(dplyr.width = Inf) #enables head() to display all coloums
library(mgcv)
library(lubridate)
library(readr)

# Plotting Theme-

Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # legend.background = element_rect(fill="white"),
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=15),
    legend.title = element_blank(),
    legend.position = c(0.2, 0.8),
    text=element_text(size=15),
    strip.text.y = element_text(size = 15,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=15),
    axis.title.y=element_text(vjust=0.6, angle=90, size=15),
    axis.text.x=element_text(size=15),
    axis.text.y=element_text(size=15),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())

# Bring in and format the raw data----

# Set work directory----
work.dir=("Z://Analysis_Miller_lobster") #for Ash laptop

## Sub directories ----
data.dir<-paste(work.dir,"Data",sep="/")
map.dir<-paste(work.dir,"Map Layers",sep="/")
plots.dir<-paste(work.dir,"Plots",sep="/")
model.dir<-paste(work.dir,"Model_out_catch",sep="/")

# Bring in and format the data----
name<-"length"

setwd(data.dir)

dat<- read_csv("length.sw.sst.csv")%>%
  dplyr::rename(response=Carapace.length,
                Taxa=Colour)%>%
  drop_na(Taxa)%>%
  drop_na(response)%>%
  filter(response>0)%>%
  filter(Taxa%in%c("Red","White"))%>%
  filter(!Location=="Rivermouth")%>%
  #filter(!Location=="Golden Ridge")%>% #think about putting that back in
  # #   Transform variables
  mutate(Date=as.factor(yday(Date)))%>%
  mutate(Site=as.factor(Site))%>%
  mutate(Location=as.factor(Location))%>%
  mutate(Pot.number=as.factor(Pot.number))%>%
  mutate(Location=str_replace_all(.$Location,c("Golden Ridge"="Low", "Little Horseshoe"="Low","White Point"= "Medium","Irwin Reef"="Medium")))%>%
  glimpse()

names(dat)
unique(dat$Location)

ggplot(data=dat,aes(x=Location,y=response, colour=Location))+
  geom_boxplot()

# Model for White----
# Location- old model
# #Hs.m.sw+Location+sst

use.dat<-dat%>%
  filter(Taxa=="White")%>%
  glimpse()

mod=gam(response~s(Pot.number,bs='re')+s(sst,k=3,bs='cr')+ s(Hs.m.sw,k=3,bs='cr')+Location,data=use.dat)
#s(Site,bs='re')+ , family=tw()
summary(mod)

# Legal_predict - Location------
testdata <- expand.grid(Pot.number=(mod$model$Pot.number),
                        Hs.m.sw=mean(mod$model$Hs.m.sw),
                         sst=mean(mod$model$sst),
                        Location = c("Cliff Head","Low", "Medium", "Seven Mile"))%>% #"Golden Ridge", "Little Horseshoe"  , "White Point", "Irwin Reef",
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
#head(fits,2)
predicts.catch.white = testdata%>%data.frame(fits)%>%
  group_by(Location)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()%>%
  glimpse()
# Plotting Theme----
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # legend.background = element_rect(fill="white"),
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=15),
    legend.title = element_blank(),
    legend.position = c(0.2, 0.8),
    text=element_text(size=15),
    strip.text.y = element_text(size = 15,angle = 0, colour="black"),
    axis.title.x=element_text(vjust=0.3, size=15, colour="black",face="bold"),
    axis.title.y=element_text(vjust=1, angle=90, size=15, colour="black",face="bold"),
    axis.text.x=element_text(size=15, colour="black"),
    axis.text.y=element_text(size=15, colour="black"),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())

# Plot white catch - location ----
length.white.location<- ggplot(aes(x=Location,y=response), data=predicts.catch.white) +#,fill=Location,colour=Location
  ylab("Carapace length (mm)")+
  xlab('Location')+
  geom_point(stat = "identity", size=1.8)+
  geom_errorbar(aes(ymin = response-se.fit,ymax = response+se.fit),width = 0.5) +
  theme_bw()+
  # ylim(0, 100)+
  Theme1+
  theme(legend.position = "none")+
  ggtitle("Whites")
length.white.location


# Red_predict - T1.s.sw------


testdata <- expand.grid(
                        Date=(mod$model$Date),
                        Site=(mod$model$Site),
                        Location = c("Cliff Head","Golden Ridge","Irwin Reef","Seven Mile"))%>%
  distinct()%>%
  glimpse()


fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
#head(fits,2)
predicts.catch.legal.period = testdata%>%data.frame(fits)%>%
  group_by(T1.s.sw)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()%>%
  glimpse()


# Plot legal catch - T1.s.sw - Period ----
# PLOTS Bivalve M.striata lobster ----
catch.legal.period<- ggplot() +
  ylab(bquote('Lobster' *pot^-1*''))+
  xlab(bquote('Swell period (' *sec^-1*')'))+
  # scale_color_manual(labels = c("Fished", "SZ"),values=c("red", "black"))+
  # geom_point(data=use.dat,aes(x=lobster,y=response,colour=Status),  alpha=0.75, size=2,show.legend=FALSE)+
  geom_line(data=predicts.catch.legal.period,aes(x=T1.s.sw,y=response),alpha=0.5)+
  geom_line(data=predicts.catch.legal.period,aes(x=T1.s.sw,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.catch.legal.period,aes(x=T1.s.sw,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
# annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=5)+
# annotate("text", x = -Inf, y=Inf, label = "   Myadora striata",vjust = 1, hjust = -.1,size=5,fontface="italic")+
# geom_blank(data=dat.bms,aes(x=lobster,y=response*1.05))#to nudge data off annotations
catch.legal.period







# Model for Red----

#Hs.m.sw+Location+sst-New 
use.dat<-dat%>%
  filter(Taxa=="Red")%>%
  glimpse()

mod=gam(response~s(Pot.number,bs='re')+s(sst,k=3,bs='cr')+ s(Hs.m.sw,k=3,bs='cr')+Location,data=use.dat)
#s(Site,bs='re')+ , family=tw()
summary(mod)

# Red_predict - Location------
testdata <- expand.grid(Pot.number=(mod$model$Pot.number),
                        Hs.m.sw=mean(mod$model$Hs.m.sw),
                        sst=mean(mod$model$sst),
                        Location = c("Cliff Head","Low", "Medium", "Seven Mile"))%>% #"Golden Ridge", "Little Horseshoe"  , "White Point", "Irwin Reef",
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
#head(fits,2)
predicts.catch.red = testdata%>%data.frame(fits)%>%
  group_by(Location)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()%>%
  glimpse()


length.red.location<- ggplot(aes(x=Location,y=response), data=predicts.catch.red) +#,fill=Location,colour=Location
  ylab("Carapace length (mm)")+
  xlab('Location')+
  geom_point(stat = "identity", size=1.8)+
  geom_errorbar(aes(ymin = response-se.fit,ymax = response+se.fit),width = 0.5) +
  theme_bw()+
  # ylim(0, 100)+
  Theme1+
  theme(legend.position = "none")+
  ggtitle("Red")
length.red.location



# Save plots----
setwd("Z://Analysis_Miller_lobster/Plots")
#setwd("C:/GitHub/Analysis_Miller_lobster/Plots")

# To see what they will look like use grid.arrange() - make sure Plot window is large enough! - or will error!
grid.arrange(length.white.location, length.red.location,nrow=1,ncol=2)

# Use arrangeGrob ONLY - as we can pass this to ggsave! Note use of raw ggplot's
combine.plot<-arrangeGrob(length.white.location, length.red.location,nrow=1,ncol=2)

ggsave(combine.plot,file="length.location.new.png", width = 25, height = 12,units = "cm")

ggsave(length.white.location,file="length.white.location.png", width = 28, height = 15,units = "cm")
