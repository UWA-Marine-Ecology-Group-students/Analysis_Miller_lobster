# Explore catch data----
rm(list=ls()) #clears memory

# librarys----
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(magrittr)
library(readr)
library(lubridate)
library(lme4)
library(lmerTest)
library(cluster)
library(mclust)

# Study name----
study<-"kmeanscluster"

# Set work directory----
work.dir=("Z:/Analysis_Miller_lobster") # FOr Ash's laptop using Git

# Set sub-directories----
data.dir=paste(work.dir,"Data",sep="/")
plot.dir=paste(work.dir,"Plots",sep="/")

#Bring in recapture data ----
#dat.rr<- read_csv("dat.rr.clean.csv")%>% #Updated: cleaned and no Jan-April
#dat.rr<-read_csv("dat.rr.csv")%>% #data we used with simon and jason
#dat.rr <- read_csv("dat.rr.all.csv")%>%  #updated data (cleaned)
#dat.rr<- read_csv("dat.rr.new.csv")%>% #No Jan-April and old data
#   glimpse()

k.dat<-read_csv("Growth.Data.csv")%>%
  glimpse()

glimpse(k.dat)
#check
unique(k.dat$Sex.recap)
unique(k.dat$Sex)
unique(k.dat$Site)
unique(k.dat$Location)
unique(k.dat$Location.recap)

#Removes negative growth data 
# k.dat1 <- k.dat%>%
#   filter(inc>0)%>%
#   glimpse()

#Plotting Themes----
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.background = element_blank(),
    text=element_text(size=14),
    strip.text.y = element_text(size = 14,angle = 270),
    axis.title.x=element_text(vjust=0.3, size=14),
    axis.title.y=element_text(vjust=0.6, angle=90, size=14),
    axis.text.x=element_text(size=12,colour="black"),
    axis.text.y=element_text(size=12,colour="black"),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.ticks.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank(),
    strip.text.x = element_text(size = 14),
    plot.background = element_blank()) 


#Seven Mile: ----
glimpse(k.dat)
#create length bins

k.dat %<>% mutate(lbin=as.numeric(cut(Carapace.length, c(0,50,60,70,100))), 
  Cluster=NA, moult=NA) %>% 
  filter(inc>=0)%>% 
  glimpse

#subset to SM
sm.kdat<-k.dat%>%  
  filter(Location=="Seven Mile")%>%  
  glimpse()

tapply(sm.kdat$inc, list(sm.kdat$lbin, sm.kdat$Sex), length)


# par(mfrow=c(3,3))
# for(s in unique(sm.kdat$Sex.int)){
#   for(lb in sort(unique(sm.kdat$lbin))){
#     tmp <- sm.kdat[sm.kdat$lbin==lb & sm.kdat$Sex.int==s,]
#     if(max(tmp$inc,na.rm=T)>18){ cent <- c(0,4,10)} else {cent <- c(0,4)} #,20 ,10
#     cl.sm <- kmeans(tmp$inc,cent)
#     tmp$Cluster <- cl.sm$cluster
#     plot(tmp$initial.cl, tmp$inc, ylim=c(0,20),col=as.numeric(as.factor(tmp$Cluster)),pch=16, main=paste(s, lb))
#     tmp2 <- tmp %>%
#     dplyr::group_by(Cluster) %>%
#     dplyr::summarise(mn=mean(inc))%>%
#     dplyr::filter(mn>=0) %>%
#     dplyr::arrange(mn)
#     tmp2$moults <- 0:(nrow(tmp2)-1)
#     tmp$moult <- tmp2$moults[match(tmp$Cluster,tmp2$Cluster)]
#     sm.kdat[sm.kdat$lbin==lb & sm.kdat$Sex.int==s,] <- tmp
#   }}

par(mfrow=c(2,2))
glimpse(sm.kdat)

for(s in unique(sm.kdat$Sex)){
   for(lb in sort(unique(sm.kdat$lbin))){
    tmp <- sm.kdat[sm.kdat$lbin==lb & sm.kdat$Sex==s,]
    #if(max(tmp$inc,na.rm=T)>18){ cent <- c(0,4,10)} else {cent <- c(0,4)} #,20 ,10
    
    # if(!(s=='Female'&lb==3)& !(s=='Male'&lb==3)& !(s=='Male'&lb==4) ){cent<-c(0,4)} else if (!(s=='Male'&lb==3) & !(s=='Male'&lb==4)) {cent<- c(0,4,10,12)} else {cent<- c(0,4,10)}
    
    if(!(s=='Female'&lb==3) & !(s=='Female'&lb==4)& !(s=='Male'&lb==3)& !(s=='Male'&lb==1) & !(s=='Male'&lb==4)){cent<-c(0,4)} else if (!(s=='Female'&lb==3)) {cent<- c(0,4,8)} else {cent<- c(0,4,8,10)}
    
    cl.sm <- kmeans(tmp$inc,cent)
    tmp$Cluster <- cl.sm$cluster
    plot(tmp$Carapace.length, tmp$inc, ylim=c(0,20),col=as.numeric(as.factor(tmp$Cluster)),pch=16, main=paste(s, lb))
    tmp2 <- tmp %>% 
    dplyr::group_by(Cluster) %>% 
    dplyr::summarise(mn=mean(inc))%>% 
    dplyr::filter(mn>=0) %>% 
    dplyr::arrange(mn) 
    tmp2$moults <- 0:(nrow(tmp2)-1)
    tmp$moult <- tmp2$moults[match(tmp$Cluster,tmp2$Cluster)]
    sm.kdat[sm.kdat$lbin==lb & sm.kdat$Sex==s,] <- tmp
  }}

#check
glimpse(tmp2)
glimpse(tmp)
glimpse(sm.kdat)


#Look at just moult no. 1
dat2 <- sm.kdat %>% 
  dplyr::filter(moult==1) %>% 
  dplyr::group_by(Sex, lbin) %>% 
  dplyr::summarise(mn=mean(inc)) %>%
  glimpse()

dat.sm <- dat2

#Plot mean inc per size class per sex for moult 1
#should properly Plot this!!
plot(dat.sm$lbin, dat.sm$mn, col=as.numeric(as.factor(dat.sm$Sex)),pch=16, ylim=c(0,7))

#Save moult data in k.dat dataframe
k.dat[k.dat$Location=="Seven Mile",] <- sm.kdat

#### Irwin Reef----
k.dat %<>% filter(!is.na(Location))

#subset to Irwin Reef
ir.kdat<-k.dat%>% 
  dplyr::filter(Location=="Irwin Reef" & !is.na(Location)) %>%  
  glimpse()

#Create length bins
ir.kdat %<>% mutate(lbin=as.numeric(cut(Carapace.length, c(0,50,60,70,100)))) %>% filter(!is.na(ir.kdat$lbin))

tapply(ir.kdat$inc, list(ir.kdat$lbin, ir.kdat$Sex), length)
#Loop to can apply kmean to each sex and size class combination
par(mfrow=c(2,2))
for(s in unique(ir.kdat$Sex)){
  for(lb in sort(unique(ir.kdat$lbin))){
    tmp <- ir.kdat %>% filter(lbin==lb & Sex==s & !is.na(Carapace.length))
    #if(max(tmp$inc,na.rm=T)>18){ cent <- c(0,4,10)} else {cent <- c(0,4)} #,20 ,10
    if( !(s=='Female'&lb==3)& !(s=='Female'&lb==2) & !(s=='Female'&lb==1)& !(s=='Male'&lb==3) & !(s=='Male'&lb==2)& !(s=='Male'&lb==4)){cent<-c(0,4)} else {cent<- c(0,4,10)} #
    cl.ir <- kmeans(tmp$inc,cent)
    tmp$Cluster <- cl.ir$cluster
    plot(tmp$Carapace.length, tmp$inc, ylim=c(0,22),col=as.numeric(as.factor(tmp$Cluster)),pch=16, main=paste(s, lb))
    tmp2 <- tmp %>% 
      dplyr::group_by(Cluster) %>% 
      dplyr::summarise(mn=mean(inc)) %>% 
      dplyr::arrange(mn)
    tmp2$moults <- 0:(nrow(tmp2)-1)
    tmp$moult <- tmp2$moults[match(tmp$Cluster,tmp2$Cluster)]
    ir.kdat[ir.kdat$lbin==lb & ir.kdat$Sex==s & !is.na(ir.kdat$Carapace.length),] <- tmp
    as.data.frame(ir.kdat[ir.kdat$lbin==lb & ir.kdat$Sex==s & !is.na(ir.kdat$lbin)& !is.na(ir.kdat$Carapace.length)& !is.na(ir.kdat$Sex),])
    
  }}

#check
glimpse(tmp2)
glimpse(tmp)
glimpse(ir.kdat)
#subset to only 1 moult recaps
dat2 <- ir.kdat %>%  
  dplyr::filter(moult==1) %>%  
  dplyr::group_by(Sex, lbin) %>%  
  dplyr::summarise(mn=mean(inc))%>%
  glimpse()

#Plot av. increase per length bin
plot(dat2$lbin, dat2$mn, col=as.numeric(as.factor(dat2$Sex)),pch=16, ylim=c(0,10))

dat.ir <-dat2

glimpse(ir.kdat)

k.dat[k.dat$Location=="Irwin Reef" & !is.na(k.dat$lbin)& !is.na(k.dat$Carapace.length)& !is.na(k.dat$Sex),] <- ir.kdat


#### Cliff Head-----
k.dat %<>% filter(!is.na(Location))
glimpse(k.dat)

#filter to Cliff Head only
ch.kdat<-k.dat%>% 
  dplyr::filter(Location=="Cliff Head" & !is.na(Location)) %>%  
  glimpse()

#Create length bins
ch.kdat %<>% 
  dplyr::mutate(lbin=as.numeric(cut(Carapace.length, c(0,50,60,70,100)))) %>%
  dplyr::filter(!is.na(ch.kdat$lbin))%>% 
  glimpse()

tapply(ch.kdat$inc, list(ch.kdat$lbin, ch.kdat$Sex), length)

#Very few in length 1 & 3-Will probably just exclude those

par(mfrow=c(3,3))
for(s in unique(ch.kdat$Sex)){
  for(lb in c(2,4)) {     #c(2,4)
    tmp <- ch.kdat %>% filter(lbin==lb & Sex==s & !is.na(Carapace.length))
    if(!(s=='Male'&lb==4) & !(s=='Male'&lb==2) & !(s=='Female'&lb==4)){cent<-c(1)} 
    else {cent<- c(0,4,10)}
    cl.ch <- kmeans(tmp$inc,cent)
    tmp$Cluster <- cl.ch$cluster
    plot(tmp$Carapace.length, tmp$inc, ylim=c(0,20),col=as.numeric(as.factor(tmp$Cluster)),pch=16, main=paste(s, lb))
    tmp2 <- tmp %>% 
      dplyr::group_by(Cluster) %>% 
      dplyr::summarise(mn=mean(inc)) %>% 
      dplyr::arrange(mn)
    tmp2$moults <- 0:(nrow(tmp2)-1)
    tmp$moult <- tmp2$moults[match(tmp$Cluster,tmp2$Cluster)]
    ch.kdat[ch.kdat$lbin==lb & ch.kdat$Sex==s & !is.na(ch.kdat$Carapace.length),] <- tmp
    as.data.frame(ch.kdat[ch.kdat$lbin==lb & ch.kdat$Sex==s & !is.na(ch.kdat$lbin)& !is.na(ch.kdat$Carapace.length)& !is.na(ch.kdat$Sex),])
    
  }}


dat2 <- ch.kdat %>% 
  dplyr::filter(moult==1) %>% 
  dplyr::group_by(Sex, lbin) %>% 
  dplyr::summarise(mn=mean(inc)) %>%
  glimpse() 

plot(dat2$lbin, dat2$mn, col=as.numeric(as.factor(dat2$Sex)),pch=16, ylim=c(0,20))

dat.ch <- dat2

k.dat[k.dat$Location=="Cliff Head" & !is.na(k.dat$lbin)& !is.na(k.dat$Carapace.length)& !is.na(k.dat$Sex),] <- ch.kdat
glimpse(ch.kdat)


#### Golden Ridge-----
gr.kdat<-k.dat%>% 
  dplyr::filter(Location=="Golden Ridge" & !is.na(Location)) %>%  
  glimpse()

gr.kdat %<>% mutate(lbin=as.numeric(cut(Carapace.length, c(0,50,60,70,100)))) %>% filter(!is.na(gr.kdat$lbin))
tapply(gr.kdat$inc, list(gr.kdat$lbin, gr.kdat$Sex), length)

#Not that many in length bin 1- might be okay.
#Or Male length bin 3

par(mfrow=c(3,3))
for(s in unique(gr.kdat$Sex)) {
  for(lb in c(1:4)) {
    if(!(s=='Male'&lb==3) & !(s=='Female'&lb==4)) {
    tmp <- gr.kdat %>% filter(lbin==lb & Sex==s & !is.na(Carapace.length))
      if(!(s=='Female' & lb>=3)){ cl.gr <- kmeans(tmp$inc,c(0,4,10))
      } else {cl.gr <- kmeans(tmp$inc,c(0,10))}
      tmp$Cluster <- cl.gr$cluster
      plot(tmp$Carapace.length, tmp$inc, ylim=c(0,20),col=as.numeric(as.factor(tmp$Cluster)),pch=16, main=paste(s, lb))
  #    plot(tmp$initial.cl, tmp$inc, ylim=c(0,20),col=1,pch=16, main=paste(s, lb))
      tmp2 <- tmp %>% 
        dplyr::group_by(Cluster) %>% 
        dplyr::summarise(mn=mean(inc)) %>% 
        dplyr::arrange(mn)
      tmp2$moults <- 0:(nrow(tmp2)-1)
      tmp$moult <- tmp2$moults[match(tmp$Cluster,tmp2$Cluster)]
      gr.kdat[gr.kdat$lbin==lb & gr.kdat$Sex==s & !is.na(gr.kdat$Carapace.length),] <- tmp}
  }}

#check
glimpse(tmp)
glimpse(tmp2)

#av inc per size class/sex for moult 1 only
dat2 <- gr.kdat %>% 
  dplyr::filter(moult==1) %>% 
  dplyr::group_by(Sex, lbin) %>% 
  dplyr::summarise(mn=mean(inc)) %>%
  glimpse()

plot(dat2$lbin, dat2$mn, col=as.numeric(as.factor(dat2$Sex)),pch=16, ylim=c(0,10))

dat.gr <-dat2

k.dat[k.dat$Location=="Golden Ridge" & !is.na(k.dat$lbin)& !is.na(k.dat$Carapace.length)& !is.na(k.dat$Sex),] <- gr.kdat

#White Point----
wp.kdat<-k.dat%>% 
  dplyr::filter(Location=="White Point" & !is.na(Location)) %>%  
  glimpse()

wp.kdat %<>% 
  mutate(lbin=as.numeric(cut(Carapace.length, c(0,50,60,70,100)))) %>%      filter(!is.na(wp.kdat$lbin))%>%
  glimpse()

tapply(wp.kdat$inc, list(wp.kdat$lbin, wp.kdat$Sex), length)

#Very few in length bin 1

par(mfrow=c(2,2))
for(s in unique(wp.kdat$Sex)) {
  for(lb in c(2:4)) {
    tmp <- wp.kdat %>% filter(lbin==lb & Sex==s & !is.na(Carapace.length))
    if(!(s=='Male'&lb==4) & !(s=='Female'&lb==4)& !(s=='Male'&lb==2)){cent <-c(0,4)} else if (!(s=='Female'&lb==4)){cent<- c(0,4,10)}
    else {cent<- c(0,4,10, 12)}
    cl.wp <- kmeans(tmp$inc,cent)
      tmp$Cluster <- cl.wp$cluster
      plot(tmp$Carapace.length, tmp$inc, ylim=c(0,20),col=as.numeric(as.factor(tmp$Cluster)),pch=16, main=paste(s, lb))
      #    plot(tmp$initial.cl, tmp$inc, ylim=c(0,20),col=1,pch=16, main=paste(s, lb))
      tmp2 <- tmp %>% 
        dplyr::group_by(Cluster) %>% 
        dplyr::summarise(mn=mean(inc)) %>% 
        dplyr::arrange(mn)
      tmp2$moults <- 0:(nrow(tmp2)-1)
      tmp$moult <- tmp2$moults[match(tmp$Cluster,tmp2$Cluster)]
      wp.kdat[wp.kdat$lbin==lb & wp.kdat$Sex==s & !is.na(wp.kdat$Carapace.length),] <- tmp}
  }

#check
glimpse(tmp)
glimpse(tmp2)

#av inc per size class/sex for moult 1 only
dat2 <- wp.kdat %>% 
  dplyr::filter(moult==1) %>% 
  dplyr::group_by(Sex, lbin) %>% 
  dplyr::summarise(mn=mean(inc)) %>%
  glimpse()

plot(dat2$lbin, dat2$mn, col=as.numeric(as.factor(dat2$Sex)),pch=16, ylim=c(0,10))

dat.wp <-dat2

k.dat[k.dat$Location=="White Point" & !is.na(k.dat$lbin)& !is.na(k.dat$Carapace.length)& !is.na(k.dat$Sex),] <- wp.kdat

#Little Horseshoe----

lh.kdat<-k.dat%>% 
  dplyr::filter(Location=="Little Horseshoe" & !is.na(Location)) %>%
  glimpse()

lh.kdat %<>% 
  mutate(lbin=as.numeric(cut(Carapace.length, c(0,50,60,70,100)))) %>%      filter(!is.na(lh.kdat$lbin))%>%
  glimpse()

tapply(lh.kdat$inc, list(lh.kdat$lbin, lh.kdat$Sex), length)

#Woah almost none in length 1,2 & 3

par(mfrow=c(1,2))
for(s in unique(lh.kdat$Sex)) {
  for(lb in 4) {
    tmp <- lh.kdat %>% filter(lbin==lb & Sex==s & !is.na(Carapace.length))
    if(!(s=='Male'&lb==4)){cent<-c(0,2,8,12)} 
    else {cent<- c(0,2,8)} #!(s=='Female'&lb==4)&
    cl.lh <- kmeans(tmp$inc,cent)
    tmp$Cluster <- cl.lh$cluster
    plot(tmp$Carapace.length, tmp$inc, ylim=c(0,20),col=as.numeric(as.factor(tmp$Cluster)),pch=16, main=paste(s, lb))
    #    plot(tmp$initial.cl, tmp$inc, ylim=c(0,20),col=1,pch=16, main=paste(s, lb))
    tmp2 <- tmp %>% 
      dplyr::group_by(Cluster) %>% 
      dplyr::summarise(mn=mean(inc)) %>% 
      dplyr::arrange(mn)
    tmp2$moults <- 0:(nrow(tmp2)-1)
    tmp$moult <- tmp2$moults[match(tmp$Cluster,tmp2$Cluster)]
    lh.kdat[lh.kdat$lbin==lb & lh.kdat$Sex==s & !is.na(lh.kdat$Carapace.length),] <- tmp}
}

#check
glimpse(tmp)
glimpse(tmp2)

#av inc per size class/sex for moult 1 only
dat2 <- lh.kdat %>% 
  dplyr::filter(moult==1) %>% 
  dplyr::group_by(Sex, lbin) %>% 
  dplyr::summarise(mn=mean(inc)) %>%
  glimpse()

plot(dat2$lbin, dat2$mn, col=as.numeric(as.factor(dat2$Sex)),pch=16, ylim=c(0,10))

dat.lh <-dat2

k.dat[k.dat$Location=="Little Horseshoe" & !is.na(k.dat$lbin)& !is.na(k.dat$Carapace.length)& !is.na(k.dat$Sex),] <- lh.kdat

glimpse(lh.kdat)
glimpse(wp.kdat)

glimpse(k.dat)


#save data----
glimpse(k.dat)
setwd(data.dir)
write.csv(k.dat, "kmeans.data.new.csv", row.names= FALSE)

# #Bring in Data-----
# k.dat.1<-read_csv("k.data.csv")%>%
#   glimpse()
#Changes diff from 'time' to 'int'
# Maybe that's chill- Who needs 'days' anyway? not me. 


#Plots----
#subset to only moult 1
glimpse(k.dat)

moult1 <- k.dat%>%
  filter(moult==1)%>%
  glimpse()

glimpse(moult1)
unique(moult1$Location)

#Order sites for boxplot
moult1$Location<-factor(moult1$Location, levels = c("Golden Ridge","Cliff Head","Little Horseshoe", "White Point", "Irwin Reef", "Seven Mile"))

boxplot <- ggplot(data=moult1, aes(x=Location, y= inc))+
  geom_boxplot(outlier.color = NA, notch=FALSE)+
  geom_jitter(width = 0.1, height = NULL, alpha=0.5)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"))+
  guides(fill=FALSE)+
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ #this is for the mean
  theme_bw()+Theme1+
  #ylim(0,8)+
  theme(axis.text.x = element_text(angle=90, size=12))+  
  theme(plot.title = element_text(hjust = 0, size=14, face = "plain"))+
  theme(axis.title.x = element_text(size= 14))+
  theme(axis.title.y = element_text(size = 14))+
  ylab("Growth per moult (mm)")+
  xlab("Location") +
  facet_grid(.~Sex)+
  #theme(strip.text.x = element_text(size=14))+
  theme(axis.text.y = element_text(size=14))
boxplot 

#Plot multiple moults
glimpse(k.dat.1)
moults <- k.dat%>%
  filter(moult!="0")%>%
  glimpse()

moults$Location<-factor(moults$Location, levels = c("Golden Ridge","Cliff Head","Little Horseshoe", "White Point", "Irwin Reef", "Seven Mile"))

unique(moults$moult)

boxplot1 <- ggplot(data=moults, aes(x=Location, y= inc))+
  geom_boxplot(outlier.color = NA, notch=FALSE)+
  geom_jitter(width = 0.1, height = NULL, alpha=0.5, col=moults$moult)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"))+
  guides(fill=FALSE)+
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ #this is adding the dot for the mean
  theme_bw()+Theme1+
  ylim(0,20)+
  theme(axis.text.x = element_text(angle=90, size=12))+  
  #ggtitle ("Moulting increment (mm)")+
  theme(plot.title = element_text(hjust = 0, size=14, face = "plain"))+
  theme(axis.title.x = element_text(size= 14))+
  theme(axis.title.y = element_text(size = 14))+
  ylab("Growth per moult (mm)")+
  xlab("Location") +
  facet_grid(.~Sex)+
  #theme(strip.text.x = element_text(size=14))+
  theme(axis.text.y = element_text(size=12))
boxplot1

# Save plots----
setwd("Z://Analysis_Miller_lobster/Plots")

#Plot for 1 moult
ggsave(boxplot,file="Growth.per.1moult.png", width = 18, height = 13,units = "cm")

#Plot for multiple moults
ggsave(boxplot1,file="Growth.Multi.Moults.png", width = 18, height = 13,units = "cm")



#Plot for release CL x growth for moult 1----
glimpse(moult1)
rlcl.plot<- ggplot(moult1, aes(x=Carapace.length, y=inc, col=Location, shape=Location))+
  geom_point(size=4)+
 # scale_color_manual(values = c("Irwin Reef"="plum3","Cliff Head"="darkorange3", "Seven Mile"= "turquoise4", "Golden Ridge"= "red1"))+
  #ylim(0, 7)+
  #scale_shape_manual(values=c(16, 17, 15, 4))+
  theme_bw()+Theme1+
  xlab("Release Carapace Length (CL:mm)")+
  ylab("Growth (mm)")+
  labs(col="Location" ,shape="Location")+
  facet_wrap(~Sex)
rlcl.plot


#Get proportion of year
glimpse(moult1)
moult1<-moult1%>%
  mutate(Lyrs=as.numeric(as.Date(recap.Date, '%d/%m/%Y')-as.Date(Date, '%d/%m/%Y'))/365)%>%
  glimpse()

lyrs.plot<- ggplot(moult1, aes(x=Lyrs, y=inc, col=Location.int, shape=Location.int))+#,shape=moult
  geom_point(size=4)+
  #scale_colour_manual(values = c("3", "blue", "red"))+
  scale_color_manual(values = c("Irwin Reef"="plum3","Cliff Head"="darkorange3", "Seven Mile"= "turquoise4", "Golden Ridge"= "red1"))+  theme_bw()+Theme1+
  ylim(1, 8)+
  xlab("Time at liberty (years: proportion)")+
  ylab("Growth (mm)")+
  labs(col="Location",shape="Location" )+ #,shape="Location"
  facet_wrap(~Sex.int)
lyrs.plot

#Plot for release CL x growth for multiple moults----
glimpse(moults)
moults%<>%
  mutate(moult=factor(moult))%>%
  glimpse()

rlcl.plot<- ggplot(moults, aes(x=initial.cl, y=inc, col=Location.int, shape=moult))+
  geom_point(size=4)+
  scale_color_manual(values = c("Irwin Reef"="plum3","Cliff Head"="darkorange3", "Seven Mile"= "turquoise4", "Golden Ridge"= "red1"))+
  #scale_color_manual(values = c("1"="plum3","2"="darkorange3", "3"= "turquoise4"))+
  ylim(0, 20)+
  scale_shape_manual(values=c(1, 2, 0))+
  #scale_shape_manual(values=c(16, 17, 15, 4))+
  theme_bw()+Theme1+
  xlab("Release Carapace Length (CL:mm)")+
  ylab("Growth (mm)")+
  labs(col="Location" ,shape="No. of Moults")+
  facet_wrap(~Sex.int)
rlcl.plot


#Get proportion of year
glimpse(moults)
moults<-moults%>%
  mutate(Lyrs=as.numeric(as.Date(recap.Date, '%d/%m/%Y')-as.Date(Date, '%d/%m/%Y'))/365)%>%
  glimpse()

lyrs.plot<- ggplot(moults, aes(x=Lyrs, y=inc, col=Location.int, shape=moult))+#,shape=moult
  geom_point(size=4)+
  #scale_colour_manual(values = c("3", "blue", "red"))+
  scale_color_manual(values = c("Irwin Reef"="plum3","Cliff Head"="darkorange3", "Seven Mile"= "turquoise4", "Golden Ridge"= "red1"))+
  theme_bw()+Theme1+
  ylim(1, 20)+
  scale_shape_manual(values=c(1, 2, 0))+
  xlab("Time at liberty (years: proportion)")+
  ylab("Growth (mm)")+
  labs(col="Location",shape="No. of Moults" )+ #,shape="Location"
  facet_wrap(~Sex.int)
lyrs.plot





#Models-----
#Tim wants to do a ful subset gaminstead of these models (typical)
#1. Using Moults >1

#No effect so becomes lm() model
lm <- lm(inc~Location.int+Sex.int+initial.cl+moult, data=k.dat.1[!is.na(k.dat.1$Cluster) & k.dat.1$moult>=1,]) #+moult
summary(lm)
anova(lm)
lm.pair<-lsmeans(lm, pairwise~Location.int)
summary(lm.pair)

#With random effect
lmer <- lmer(inc~Location.int+Sex.int+initial.cl + moult+(1|Site/Location.int), data=k.dat.1[!is.na(k.dat.1$Cluster) & k.dat.1$moult>=1,]) #+moult
summary(lmer)
lmer.pair<-lsmeans(lmer, pairwise~Location.int)
summary(lmer.pair)


#subset to only moult 1
moult1 <- k.dat.1%>%
  filter(moult==1)%>%
  glimpse()

lm1 <- lm(inc~Location.int+Sex.int+initial.cl, data=moult1) #+moult
summary(lm1)
lm1.pair<-lsmeans(lm1, pairwise~Location.int)
summary(lm1.pair)

lm2 <- lmer(inc~Location.int+Sex.int+initial.cl+(1|Site/Location.int), data=moult1)
summary(lm2)
lm2.pair<-lsmeans(lm2, pairwise~Location.int)
summary(lm2.pair)



#Don't think it's this model
glm2 <- glmer(inc~Location.int+Sex.int+initial.cl +(1|Site/Location.int), data=moult1) #+(1|Site/Location.int)
summary(glm2)
unique(k.dat$Site)

help('emmeans')
library(emmeans)

out <- predict(lm1, newdata=expand.grid(Location.int=unique(k.dat$Location.int), Sex.int=c('Female','Male'), initial.cl=70, moult=1), se.fit = T, interval = "confidence")
out





#Plot average growth per size class----

dat.sm <- dat.sm%>%
  mutate(Location="Seven Mile")%>%
  glimpse()

dat.ir <- dat.ir%>%
  mutate(Location="Irwin Reef")%>%
  glimpse()

dat.ch <- dat.ch%>%
  mutate(Location="Cliff Head")%>%
  glimpse()

dat.gr <- dat.gr%>%
  mutate(Location="Golden Ridge")%>%
  glimpse()

dat.all <- rbind(dat.sm, dat.ir, dat.ch, dat.gr)%>%
  glimpse()

glimpse(dat.all)

dat.all<-dat.all%>%
  dplyr::rename(Sex=Sex.int)%>%
  mutate(lbin = str_replace_all(.$lbin, c("2"="50-59", "3"="60-69", "4"="70-90",  "1"="0-49")))%>%
  glimpse()


plot<- ggplot(data=dat.all, aes(x=lbin, y=mn, shape=Sex, group=Sex))+
  geom_point(size=2.5)+
  scale_shape_manual(values= c(0, 17))+
  geom_line()+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"))+
  theme_bw()+Theme1+
  xlab("Size class (mm)")+
  ylab("Average moult increment (mm)")+
  facet_wrap(~Location)
plot
