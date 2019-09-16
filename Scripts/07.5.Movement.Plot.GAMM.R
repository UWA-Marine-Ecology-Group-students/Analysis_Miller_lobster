
rm(list=ls()) # Clears memory
# Part 3 - plots of the most parsimonious models----

### now  make a nice plot of the most interesting models-----
library(gridExtra)
library(ggplot2)
library(grid)
detach("package:plyr", unload=TRUE)#will error - don't worry
library(tidyr)
library(dplyr)
options(dplyr.width = Inf) #enables head() to display all coloums
library(mgcv)
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
    strip.text.y = element_text(size = 15,angle = 0, colour="black"),
    axis.title.x=element_text(vjust=0, size=15, colour="black",face="bold"),
    axis.title.y=element_text(vjust=0.6, angle=90, size=15,face="bold", colour="black"),
    axis.text.x=element_text(size=15, colour="black"),
    axis.text.y=element_text(size=15, colour="black"),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())


# Bring in and format the raw data----

# Set work directory----
work.dir=("Z://Analysis_Miller_lobster") #for laptop

## Sub directories ----
data.dir<-paste(work.dir,"Data",sep="/")
plots.dir<-paste(work.dir,"Plots",sep="/")
model.dir<-paste(work.dir,"Model_out_movement",sep="/")

# Bring in and format the data----
name<-"movement"

setwd(data.dir)
dir()

dat <-read_csv("North.West.data.csv")%>%
  gather(key=Taxa,value=response,WE,NS)%>%
  # 
  # dplyr::rename(response=Count,
  #               Taxa=sizeclass)%>%
  # # #   Transform variables
  # mutate(Date=as.factor(yday(Date)))%>% #as julian day
  # # mutate(recap.Date=as.factor(yday(recap.Date)))%>% #as julian day
  filter(!Location=="Seven Mile")%>%
  mutate(Location=as.factor(Location))%>%
  mutate(Location=str_replace_all(.$Location, c("Boundary"="Golden Ridge", "Mid"="Irwin Reef")))%>%
  glimpse()

#Order Location


dat$Location<-factor(dat$Location,levels=c("Irwin Reef","Cliff Head", "Golden Ridge")) 


unique(dat$Location)
dat<-as.data.frame(dat)
names(dat)
glimpse(dat)

# Model for WE----
mod=gam(response~s(Carapace.length,k=3,bs='cr')+Location+Sex+s(Ldys,k=3,bs='re'),family=tw(),data=dat%>%filter(Taxa=="WE")) #  +s(T1.s.sw,k=3,bs='cr')

summary(mod)
gam.check(mod)

# Location------
testdata <- expand.grid(Carapace.length=mean(mod$model$Carapace.length),
                        Ldys=mean(mod$model$Ldys),
                        Sex= c("Male","Female"),
                        Location = c("Irwin Reef","Cliff Head", "Golden Ridge"))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
#head(fits,2)
predicts.move.location = testdata%>%data.frame(fits)%>%
  group_by(Location)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()%>%
  glimpse()


# Plot WE movement- location ----
dev.off() #To fix error-AM
WE.location<- ggplot(aes(x=Location,y=response, fill=Location), 
                              data=predicts.move.location) +
  ylab("Distance (km)")+
  xlab('Location')+
  geom_bar(stat = "identity", colour="black")+
  geom_errorbar(aes(ymin = response-se.fit ,ymax = response+se.fit),width = 0.5) +
  #theme_classic()+
  theme_bw()+
  Theme1+
  theme(legend.position = "none")+
  # scale_y_continuous(breaks=seq(0,12,2))+
  #ylim(-10,10)+ #,8
  ggtitle("West-East Movement")
WE.location


#Movement- North-South-----
# Model for NS----
mod=gam(response~s(Carapace.length,k=3,bs='cr')+Location+Sex+s(Ldys,k=3,bs='re'),family=tw(),data=dat%>%filter(Taxa=="NS")) #  +s(T1.s.sw,k=3,bs='cr')

summary(mod)
gam.check(mod)

# Location------
testdata <- expand.grid(Carapace.length=mean(mod$model$Carapace.length),
                        Ldys=mean(mod$model$Ldys),
                        Sex= c("Male","Female"),
                        Location = c("Irwin Reef","Cliff Head", "Golden Ridge"))%>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
#head(fits,2)
predicts.move.location = testdata%>%data.frame(fits)%>%
  group_by(Location)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()%>%
  glimpse()


# Plot legal catch - location ----
dev.off() #To fix error-AM
NS.location<- ggplot(aes(x=Location,y=response,fill=Location
), 
data=predicts.move.location) +
  ylab("Distance (km)")+
  xlab('Location')+
  geom_bar(stat = "identity", colour="black")+
  geom_errorbar(aes(ymin = response-se.fit ,ymax = response+se.fit),width = 0.5) +
  #theme_classic()+
  theme_bw()+
  Theme1+
  theme(legend.position = "none")+
  # scale_y_continuous(breaks=seq(0,12,2))+
  ylim(0,0.4)+ #,8
  ggtitle("North-South Movement")
NS.location



#?????????????????????????????????????
#Predicts Plot- Distance by Carapace length----
# Legal_predict - T1.s.sw------
#For Tim-Sorry I give up

#NS
mod=gam(response~s(Carapace.length,k=3,bs='cr')+Location,family=tw(),data=dat%>%filter(Taxa=="NS")) #  +s(T1.s.sw,k=3,bs='cr')

summary(mod)
gam.check(mod)

testdata <- expand.grid(#distance=mean(mod$model$distance),
                        #Carapace.length=seq(min(use.dat$Carapace.length),max(use.dat$Carapace.length),length.out = 20),
                        #Hs.m.sw=seq(min(use.dat$Hs.m.sw),max(use.dat$Hs.m.sw),length.out = 20),
                        Carapace.length=(mod$model$Carapace.length),
                        Location = c("Irwin Reef","Cliff Head","Golden Ridge"))%>%
  distinct()%>%
  glimpse()


fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
#head(fits,2)
predicts.catch.legal.period = testdata%>%data.frame(fits)%>%
  group_by(Carapace.length)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()%>%
  glimpse()

# Plot legal catch - T1.s.sw - Period ----
# PLOTSPeriod lobster ----
carapace.distance<- ggplot() +
  ylab(bquote('Distance'))+
  xlab(bquote('Carapace length'))+
  # scale_color_manual(labels = c("Fished", "SZ"),values=c("red", "black"))+
  # geom_point(data=use.dat,aes(x=lobster,y=response,colour=Status),  alpha=0.75, size=2,show.legend=FALSE)+
  geom_line(data=predicts.catch.legal.period,aes(x=Carapace.length,y=response),alpha=0.5)+
  geom_line(data=predicts.catch.legal.period,aes(x=Carapace.length,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.catch.legal.period,aes(x=Carapace.length,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
# annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=5)+
# annotate("text", x = -Inf, y=Inf, label = "   Myadora striata",vjust = 1, hjust = -.1,size=5,fontface="italic")+
# geom_blank(data=dat.bms,aes(x=lobster,y=response*1.05))#to nudge data off annotations
carapace.distance

#Test Plot----
glimpse(dat)
test.plot<-ggplot(data=dat, aes(x=Carapace.length,y=distance))+
  geom_line()+
  geom_point()
test.plot

# Save plots----
setwd(plots.dir)
ggsave(catch.sublegal.location,file="", width = 20, height = 15,units = "cm")
# To see what they will look like use grid.arrange() - make sure Plot window is large enough! - or will error!
grid.arrange(catch.legal.location,catch.sublegal.location,nrow=1,ncol=2)

# Use arrangeGrob ONLY - as we can pass this to ggsave! Note use of raw ggplot's
combine.plot<-arrangeGrob(catch.legal.location,catch.sublegal.location,nrow=1,ncol=2)

# ggsave(combine.plot,file="catch.location.png", width = 40, height = 15,units = "cm")

ggsave(combine.plot,file="catch.location.new.axis.png", width = 30, height = 10,units = "cm")




