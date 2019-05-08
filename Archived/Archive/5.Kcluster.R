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
library(lme4)
library(lmerTest)
library(factoextra)
library(cluster)
library(NbClust)
library(mclust)

# Study name----
study<-"kmeanscluster"

# Set work directory----

work.dir=("~/GitHub/Analysis_Miller_WRL") #for Tim's github
work.dir=("~/workspace/Analysis_Miller_WRL") #for ecocloud server

# Set sub-directories----

data.dir=paste(work.dir,"Data",sep="/")
plot.dir=paste(work.dir,"Plots",sep="/")

#Bring in recapture data from ecocloud----

setwd("~/workspace/Analysis_Miller_WRL/Data")
dat.rr<-read_csv("dat.rr.csv")%>%
  glimpse()

dat.rr<- dat.rr%>%
  mutate(inc=recap.cl-initial.cl)%>% #Makes column for growth increment
  dplyr::mutate(diff=recap.Date-Date)%>%
  dplyr::rename(Site=mini.site.x)%>%
  glimpse()

k.dat<- dat.rr%>%
  select(Date, Tag.number, Location.int, Sex.int, initial.cl, recap.cl, recap.Date, inc, diff, Site )%>%
  filter(Tag.number!="190428" & Tag.number!="190188" & Tag.number!="190124" &Tag.number!="190443")%>%
  filter(Tag.number!="K2400"&Tag.number!="K1617"&Tag.number!="K1221")%>%
  filter(is.na(Location.int)| Location.int!="Rivermouth")%>% 
  glimpse()

#check
sum(k.dat$Sex.int=="UNKNOWN")



#Plotting Themes----
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
    plot.background = element_blank())


#create length bins
k.dat %<>% mutate(lbin=as.numeric(cut(initial.cl, c(0,50,60,70,100))), Cluster=NA, moult=NA) %>% filter(inc>=0)

sm.kdat<-k.dat%>%
  filter(Location.int=="Seven Mile")%>%
  glimpse()

tapply(sm.kdat$inc, list(sm.kdat$lbin, sm.kdat$Sex.int), length)
par(mfrow=c(3,3))
for(s in unique(sm.kdat$Sex.int)){
  for(lb in sort(unique(sm.kdat$lbin))){
    tmp <- sm.kdat[sm.kdat$lbin==lb & sm.kdat$Sex.int==s,]
    #nb.other<- NbClust(data=tmp$inc, diss=NULL, distance="euclidean", min.nc=2, max.nc=4, method="kmeans")
    #tab <- table(nb.other$Best.nc['Number_clusters',1:6])
    #nclust <- as.numeric(names(tab)[which(tab==max(tab))])
    cl.sm <- kmeans(tmp$inc,c(0,4,10))%>% glimpse()
    tmp$Cluster <- cl.sm$cluster
    plot(tmp$initial.cl, tmp$inc, ylim=c(0,20),col=as.numeric(as.factor(tmp$Cluster)),pch=16, main=paste(s, lb))
    tmp2 <- tmp %>% group_by(Cluster) %>% summarise(mn=mean(inc)) %>%  filter(mn>=0) %>% arrange(mn)
    tmp2$moults <- 0:(nrow(tmp2)-1)
    tmp$moult <- tmp2$moults[match(tmp$Cluster,tmp2$Cluster)]
    sm.kdat[sm.kdat$lbin==lb & sm.kdat$Sex.int==s,] <- tmp
  }}

glimpse(tmp)

par(mfrow=c(3,3))
for(s in unique(sm.kdat$Sex.int)){
  for(lb in sort(unique(sm.kdat$lbin))){
    tmp <- sm.kdat[sm.kdat$lbin==lb & sm.kdat$Sex.int==s,]
    plot(tmp$initial.cl, tmp$inc, ylim=c(0,20),col=as.numeric(as.factor(tmp$Cluster)),pch=16, main=paste(s, lb))
  }}

## model each group
dat2 <- sm.kdat %>% filter(moult==1) %>% group_by(Sex.int, lbin) %>% summarise(mn=mean(inc)) 
plot(dat2$lbin, dat2$mn, col=as.numeric(as.factor(dat2$Sex.int)),pch=16, ylim=c(0,7))

k.dat[k.dat$Location.int=="Seven Mile",] <- sm.kdat

#### Irwin
k.dat %<>% filter(!is.na(Location.int))
sm.kdat<-k.dat%>% filter(Location.int=="Irwin Reef" & !is.na(Location.int)) %>%  glimpse()
sm.kdat %<>% mutate(lbin=as.numeric(cut(initial.cl, c(0,50,60,70,100)))) %>% filter(!is.na(sm.kdat$lbin))
tapply(sm.kdat$inc, list(sm.kdat$lbin, sm.kdat$Sex.int), length)

s <- 'Male'
lb <- 4
for(s in unique(sm.kdat$Sex.int)){
  for(lb in sort(unique(sm.kdat$lbin))){
    tmp <- sm.kdat %>% filter(lbin==lb & Sex.int==s & !is.na(initial.cl))
    nb.other<- NbClust(data=tmp$inc, diss=NULL, distance="euclidean", min.nc=2, max.nc=4, method="kmeans")
    tab <- table(nb.other$Best.nc['Number_clusters',1:6])
    nclust <- as.numeric(names(tab)[which(tab==max(tab))])
    #Seven Mile: Kmeans clustering----
    #nclust <- ifelse(s=='Male' & lb==50, 3, nclust)
    #if(s=='Male' & lb==30)   {  cl.sm <- kmeans(tmp$inc,3)%>% glimpse()
    #} else {
    cl.sm <- kmeans(tmp$inc,nclust)%>% glimpse()#}
    tmp$Cluster <- cl.sm$cluster
    plot(tmp$initial.cl, tmp$inc, ylim=c(0,20),col=as.numeric(as.factor(tmp$Cluster)),pch=16, main=paste(s, lb))
    tmp2 <- tmp %>% group_by(Cluster) %>% summarise(mn=mean(inc)) %>% arrange(mn)
    tmp2$moults <- 0:(nrow(tmp2)-1)
    tmp$moult <- tmp2$moults[match(tmp$Cluster,tmp2$Cluster)]
    sm.kdat[sm.kdat$lbin==lb & sm.kdat$Sex.int==s & !is.na(sm.kdat$initial.cl),] <- tmp
    as.data.frame(sm.kdat[sm.kdat$lbin==lb & sm.kdat$Sex.int==s & !is.na(sm.kdat$lbin)& !is.na(sm.kdat$initial.cl)& !is.na(sm.kdat$Sex.int),])
    
  }}

par(mfrow=c(3,3))
for(s in unique(sm.kdat$Sex.int)){
  for(lb in sort(unique(sm.kdat$lbin))){
    tmp <- sm.kdat[sm.kdat$lbin==lb & sm.kdat$Sex.int==s,]
    plot(tmp$initial.cl, tmp$inc, ylim=c(0,20),col=as.numeric(as.factor(tmp$Cluster)),pch=16, main=paste(s, lb))
  }}
## model each group
dat2 <- sm.kdat %>% filter(moult==1) %>% group_by(Sex.int, lbin) %>% summarise(mn=mean(inc)) 
plot(dat2$lbin, dat2$mn, col=as.numeric(as.factor(dat2$Sex.int)),pch=16, ylim=c(0,15))


k.dat[k.dat$Location.int=="Irwin Reef" & !is.na(k.dat$lbin)& !is.na(k.dat$initial.cl)& !is.na(k.dat$Sex.int),] <- sm.kdat


#### Cliff Head
k.dat %<>% filter(!is.na(Location.int))
sm.kdat<-k.dat%>% filter(Location.int=="Cliff Head" & !is.na(Location.int)) %>%  glimpse()
sm.kdat %<>% mutate(lbin=as.numeric(cut(initial.cl, c(0,60,70,100)))) %>% filter(!is.na(sm.kdat$lbin))
tapply(sm.kdat$inc, list(sm.kdat$lbin, sm.kdat$Sex.int), length)

for(s in unique(sm.kdat$Sex.int)){
  for(lb in c(1,3)){
    tmp <- sm.kdat %>% filter(lbin==lb & Sex.int==s & !is.na(initial.cl))
    nb.other<- NbClust(data=tmp$inc, diss=NULL, distance="euclidean", min.nc=2, max.nc=4, method="kmeans")
    tab <- table(nb.other$Best.nc['Number_clusters',1:6])
    nclust <- as.numeric(names(tab)[which(tab==max(tab))])
    #Seven Mile: Kmeans clustering----
    #nclust <- ifelse(s=='Male' & lb==50, 3, nclust)
    #if(s=='Male' & lb==30)   {  cl.sm <- kmeans(tmp$inc,3)%>% glimpse()
    #} else {
    cl.sm <- kmeans(tmp$inc,nclust)%>% glimpse()#}
    tmp$Cluster <- cl.sm$cluster
    plot(tmp$initial.cl, tmp$inc, ylim=c(0,20),col=as.numeric(as.factor(tmp$Cluster)),pch=16, main=paste(s, lb))
    tmp2 <- tmp %>% group_by(Cluster) %>% summarise(mn=mean(inc)) %>% arrange(mn)
    tmp2$moults <- 0:(nrow(tmp2)-1)
    tmp$moult <- tmp2$moults[match(tmp$Cluster,tmp2$Cluster)]
    sm.kdat[sm.kdat$lbin==lb & sm.kdat$Sex.int==s & !is.na(sm.kdat$initial.cl),] <- tmp
    as.data.frame(sm.kdat[sm.kdat$lbin==lb & sm.kdat$Sex.int==s & !is.na(sm.kdat$lbin)& !is.na(sm.kdat$initial.cl)& !is.na(sm.kdat$Sex.int),])
    
  }}

par(mfrow=c(3,3))
for(s in unique(sm.kdat$Sex.int)){
  for(lb in c(1,3)){
    tmp <- sm.kdat[sm.kdat$lbin==lb & sm.kdat$Sex.int==s,]
    plot(tmp$initial.cl, tmp$inc, ylim=c(0,20),col=as.numeric(as.factor(tmp$Cluster)),pch=16, main=paste(s, lb))
  }}
## model each group
dat2 <- sm.kdat %>% filter(moult==1) %>% group_by(Sex.int, lbin) %>% summarise(mn=mean(inc)) 
plot(dat2$lbin, dat2$mn, col=as.numeric(as.factor(dat2$Sex.int)),pch=16, ylim=c(0,15))

k.dat[k.dat$Location.int=="Cliff Head" & !is.na(k.dat$lbin)& !is.na(k.dat$initial.cl)& !is.na(k.dat$Sex.int),] <- sm.kdat


#### Golden Ridge
sm.kdat<-k.dat%>% filter(Location.int=="Golden Ridge" & !is.na(Location.int)) %>%  glimpse()
sm.kdat %<>% mutate(lbin=as.numeric(cut(initial.cl, c(0,60,70,100)))) %>% filter(!is.na(sm.kdat$lbin))
tapply(sm.kdat$inc, list(sm.kdat$lbin, sm.kdat$Sex.int), length)
s <- 'Male'
lb <- 3
for(s in unique(sm.kdat$Sex.int)){
  for(lb in c(1,3)){
    tmp <- sm.kdat %>% filter(lbin==lb & Sex.int==s & !is.na(initial.cl))
    nb.other<- NbClust(data=tmp$inc, diss=NULL, distance="euclidean", min.nc=2, max.nc=4, method="kmeans")
    tab <- table(nb.other$Best.nc['Number_clusters',1:6])
    nclust <- as.numeric(names(tab)[which(tab==max(tab))])
    #Seven Mile: Kmeans clustering----
    #nclust <- ifelse(s=='Male' & lb==50, 3, nclust)
    #if(s=='Male' & lb==30)   {  cl.sm <- kmeans(tmp$inc,3)%>% glimpse()
    #} else {
    cl.sm <- kmeans(tmp$inc,nclust)%>% glimpse()#}
    tmp$Cluster <- cl.sm$cluster
    plot(tmp$initial.cl, tmp$inc, ylim=c(0,20),col=as.numeric(as.factor(tmp$Cluster)),pch=16, main=paste(s, lb))
    tmp2 <- tmp %>% group_by(Cluster) %>% summarise(mn=mean(inc)) %>% arrange(mn)
    tmp2$moults <- 0:(nrow(tmp2)-1)
    tmp$moult <- tmp2$moults[match(tmp$Cluster,tmp2$Cluster)]
    sm.kdat[sm.kdat$lbin==lb & sm.kdat$Sex.int==s & !is.na(sm.kdat$initial.cl),] <- tmp
  }}

par(mfrow=c(3,3))
for(s in unique(sm.kdat$Sex.int)){
  for(lb in c(1:3)){
    tmp <- sm.kdat[sm.kdat$lbin==lb & sm.kdat$Sex.int==s,]
    plot(tmp$initial.cl, tmp$inc, ylim=c(0,20),col=as.numeric(as.factor(tmp$Cluster)),pch=16, main=paste(s, lb))
  }}
## model each group
dat2 <- sm.kdat %>% filter(moult==1) %>% group_by(Sex.int, lbin) %>% summarise(mn=mean(inc)) 
plot(dat2$lbin, dat2$mn, col=as.numeric(as.factor(dat2$Sex.int)),pch=16, ylim=c(0,15))

k.dat[k.dat$Location.int=="Cliff Head" & !is.na(k.dat$lbin)& !is.na(k.dat$initial.cl)& !is.na(k.dat$Sex.int),] <- sm.kdat




#Bring in Edited data: SM months 1-4 removed
# dat.rr.new<- read_csv("dat.rr.new.csv")%>%
#   glimpse()
# 
# dat.rr.new<- dat.rr.new%>%
#   mutate(inc=recap.cl-initial.cl)%>% #Makes column for growth increment
#   dplyr::mutate(diff=recap.Date-Date)%>%
#   dplyr::rename(Site=mini.site.x)%>%
#   glimpse()
# 
# k.dat.new<- dat.rr.new%>%
#   select(Date, Tag.number, Location.int, Sex.int, initial.cl, recap.cl, recap.Date, inc, diff, Site )%>%
#   filter(Tag.number!="190428" & Tag.number!="190188" & Tag.number!="190124" &Tag.number!="190443")%>%
#   filter(Tag.number!="K2400"&Tag.number!="K1617"&Tag.number!="K1221")%>%
#   filter(is.na(Location.int)| Location.int!="Rivermouth")%>% 
#   glimpse()


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
    plot.background = element_blank()) 

#All: K-means clustering----

#Determining the optimal number of clusters
#The Elbow Method----
# glimpse(k.dat)
# dat<- k.dat%>%
#   select(inc)%>%
#   glimpse()
# 
# k.max<-10
# glimpse(dat)
# wss<-sapply(1:k.max, function(k){kmeans(dat, k, nstart=50, iter.max=15)$tot.withinss})
# wss  
# plot(1:k.max, wss, type="b", pch=19, frame=FALSE, xlab="Number of Clusters K", ylab="Total within-clusters sum of squares")

#The Gap Statistic----
# data1<-k.dat1$inc
# set.seed(123)

# gap_stat<-clusGap(k.dat$inc, FUN=kmeans, nstart=25, K.max=10, B=100)
# View(data)
# gap_stat <- cluster::clusGap(dat, FUN=kmeans, nstart=20, K.max=24, B=50) #, 
# fviz_gap_stat(gap_stat)+theme_minimal()+ggtitle ("Gap Statistic")
# # gap_stat1<-fviz_nbclust(data, FUNcluster = MyKmeansFUN, method = "gap_stat", diss = NULL, k.max = 10, nboot = 100)

#Kmeans----
# help(as.numeric)
# help('kmeans')
# 
# cl <- kmeans(k.dat[,8],3)%>%
#   glimpse()
# 
# k.dat$Cluster <-as.factor(cl$cluster)
# glimpse(k.dat)


#plot with cluster identified by colour----
# cluster.plot <-ggplot(data=k.dat, aes(x=initial.cl, y=inc, col=Cluster))+ 
#   geom_point()+
#   theme_bw()+Theme1+
#   ggtitle ("K-means clustering K=3")+
#   ylab("Moulting Increment (mm)")+
#   xlab("Initial Carapace Length (mm)") 
# cluster.plot

#Model----

# glimpse(k.dat)
# #subset to only moult 2 and 4
# moult.1.dat <- k.dat%>%
#   filter(Cluster== c("3"))%>%  #"2", #hm?
#   glimpse()
# 
# mod.k <- lm(inc~Site.int+Sex.int+initial.cl, data=moult.1.dat)
# # mod.k <- lm(inc~Cluster+Sex.int+Site.int+initial.cl, data=k.dat[k.dat$Cluster %in% c(2,4),])
# summary(mod.k)
# 
# #box plot for Kmeans moult 1
# mod.k.box<-ggplot(data= mod.k ,aes(x=Site.int, y=inc),notch=FALSE,position = dodge1, outlier.shape = NA)+
#   geom_boxplot(outlier.color = NA, notch=FALSE)+
#   geom_jitter(width = 0.1, height = NULL, alpha=0.5)+ #
#   guides(fill=FALSE)+
#   theme(panel.background = element_blank(),axis.line = element_line(colour = "black"))+
#   #stat_boxplot(geom='errorbar')+          # Adds error bars
#   stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ #this is adding the dot for the mean
#   theme_bw()+Theme1+
#   #coord_cartesian(ylim=c(0,3.5))+
#   ggtitle ("kmeans clustering K=3")+
#   theme(plot.title = element_text(hjust = 0, size=12, face = "plain"))+
#   theme(axis.text.x = element_text(angle=90))+     #Changes angle of site names
#   ylab("Moulting Increment (mm)")+
#   xlab("Location") #+
#   #facet_grid(. ~ Sex.int) 
# mod.k.box


#Seven Mile: ----
glimpse(k.dat)



# #better way
# nb.other<- NbClust(data=tmp, diss=NULL, distance="euclidean", min.nc=2, max.nc=8, method="kmeans")
# #factoextra::fviz_nbclust(nb.other)+theme_minimal()+ggtitle(("Seven Mile: NbClust's optimal number of clusters"))
# 
# 
# 
# sm.kdat$Cluster[sm.kdat$lbin==40 & sm.kdat$Sex.int=='Female'] <-as.factor(cl.sm$cluster)
# tmp <- sm.kdat[sm.kdat$lbin==40 & sm.kdat$Sex.int=='Female',]
# 
# 
# tmp2 <- tmp %>% filter()
# 
# 
# 
# #Seven Mile: plot with cluster identified by colour----
# sm.cluster.plot <-ggplot(data=tmp, aes(x=initial.cl, y=inc, col=Cluster))+ 
#   geom_point()+
#   theme_bw()+Theme1+
#   ggtitle ("Seven Mile: K-means clustering K=4")+
#   ylab("Relative Growth (mm)")+
#   xlab("Initial Carapace Length (mm)") 
# sm.cluster.plot
# 
# #subset to only 1 moult 
# sm.m1<- sm.kdat%>%
#   filter(Cluster=="4")%>%
#   glimpse()
# 
# 
# #Seven Mile with Jan-April removed----
# glimpse(k.dat.new)
# 
# #filter to Seven Mile
# sm.new<-k.dat.new%>%
#   filter(Location.int=="Seven Mile")%>%
#   glimpse()
# 
# #Filter to only increase data
# sm.k.new<- sm.new%>%
#   select(inc)%>%
#   glimpse()
# 
# #Find optimal number of clusters-----
# #The "Elbow" Method----
# 
# fviz_nbclust(sm.k.new, kmeans, method = "wss")+
#   geom_vline(xintercept=4, linetype=2)+
#   labs(subtitle = "Seven Mile wihtout Jan-Apr: Elbow Method")
# 
# #The Gap Statistic----
# 
# fviz_nbclust(sm.k.new, kmeans, nstart=25, method = "gap_stat", nboot=50)+
#   labs(subtitle = "Seven Mile: Gap Statistic method")
# 
# 
# #Silhouette method----
# 
# fviz_nbclust(sm.k.new, kmeans, method = "silhouette")+
#   labs(subtitle = "Seven Mile: Silhouette method")
# 
# 
# #Model-Based Clustering----
# help('Mclust')
# 
# sm_clust<- Mclust(sm.k.new, G=1:10)
# sm_clust$BIC
# plot(sm_clust, "BIC", palette="jco")
# summary(sm_clust)
# 
# #Seven Mile: Nbclust Package----
# library(NbClust)
# help('NbClust')
# 
# 
# Nbclust.sm.new<- NbClust(data=sm.k.new, diss=NULL, distance="euclidean", min.nc=2, max.nc=8, method="kmeans")
# 
# factoextra::fviz_nbclust(Nbclust.sm.new)+theme_minimal()+ggtitle(("Seven Mile: NbClust's optimal number of clusters"))
# 
# 
# #Seven Mile Edited: Kmeans clustering----
# glimpse(sm.new)
# cl.sm.new <- kmeans(sm.new[,8],4)%>%
#   glimpse()
# 
# sm.new$Cluster <-as.factor(cl.sm.new$cluster)
# glimpse(sm.new)
# 
# 
# #Seven Mile: plot with cluster identified by colour----
# sm.new.plot <-ggplot(data=sm.new, aes(x=initial.cl, y=inc, col=Cluster))+ 
#   geom_point()+
#   theme_bw()+Theme1+
#   ggtitle ("Seven Mile: Jan-Apr removed: K-means clustering K=4")+
#   ylab("Relative Growth (mm)")+
#   xlab("Initial Carapace Length (mm)") 
# sm.new.plot
# 
# #subset to only 1 moult 
# sm.m1.new<- sm.new%>%
#   filter(Cluster=="4")%>%
#   glimpse()
# 
# #Irwin Reef: Kmeans Cluster Analaysis----
# ir.kdat<-k.dat%>%
#   filter(Location.int=="Irwin Reef")%>%
#   glimpse()
# 
# dat.ir<- ir.kdat%>%
#   select(inc)%>%
#   glimpse()
# 
# 
# #The "Elbow" Method----
# 
# 
# fviz_nbclust(dat.ir, kmeans, method = "wss")+
#   geom_vline(xintercept=4, linetype=2)+
#   labs(subtitle = "Irwin Reef: Elbow Method")
# 
# #Silhouette method----
# 
# fviz_nbclust(dat.ir, kmeans, method = "silhouette")+
#   labs(subtitle = "Irwin Reef: Silhouette method")
# 
# #The Gap Statistic----
# 
# fviz_nbclust(dat.ir, kmeans, nstart=25, method = "gap_stat", nboot=50)+
#   labs(subtitle = "Irwin Reef: Gap Statistic method")
# 
# 
# #Model-Based Clustering: Irwin Reef----
# help('Mclust')
# 
# ir_clust<- Mclust(dat.ir, G=1:10)
# ir_clust$BIC
# plot(ir_clust, "BIC", palette="jco")
# summary(ir_clust)
# 
# #Nbclust Package----
# nb.ir<- NbClust(data=dat.ir, diss=NULL, distance="euclidean", min.nc=2, max.nc=8, method="kmeans")
# 
# factoextra::fviz_nbclust(nb.ir)+theme_minimal()+ggtitle(("Seven Mile: NbClust's optimal number of clusters"))
# 
# 
# 
# #IR: Kmean cluster analysis----
# cl.ir <- kmeans(ir.kdat[,8],3)%>%
#   glimpse()
# 
# ir.kdat$Cluster <-as.factor(cl.ir$cluster)
# glimpse(ir.kdat)
# 
# 
# 
# #Irwin Reef: plot with cluster identified by colour----
# ir.cluster.plot <-ggplot(data=ir.kdat, aes(x=initial.cl, y=inc, col=Cluster))+ 
#   geom_point()+
#   theme_bw()+Theme1+
#   ggtitle ("Irwin Reef: K-means clustering K=3")+
#   ylab("Relative Growth (mm)")+
#   xlab("Initial Carapace Length (mm)") 
# ir.cluster.plot
# 
# #subset to only 1 moult 
# ir.m1<-ir.kdat%>%
#   filter(Cluster=="1")%>%
#   glimpse()
# 
# #Cliff Head: Kmeans Cluster Analaysis----
# glimpse(k.dat)
# 
# ch.kdat<-k.dat%>%
#   filter(Location.int=="Cliff Head")%>%
#   glimpse()
# 
# dat.ch<- ch.kdat%>%
#   select(inc)%>%
#   glimpse()
# 
# #The "Elbow" Method----
# 
# fviz_nbclust(dat.ch, kmeans, method = "wss")+
#   geom_vline(xintercept=4, linetype=2)+
#   labs(subtitle = "Cliff Head: Elbow Method")
# 
# 
# #CH: The Gap Statistic Analysis----
# #better way
# fviz_nbclust(dat.ch, kmeans, nstart=25, method = "gap_stat", nboot=50)+
#   labs(subtitle = "Cliff Head: Gap Statistic method")
# 
# 
# #Silhouette method----
# 
# fviz_nbclust(dat.ch, kmeans, method = "silhouette")+
#   labs(subtitle = "Cliff Head: Silhouette method")
# 
# #Model-Based Clustering: Cliff Head----
# help('Mclust')
# 
# ch_clust<- Mclust(dat.ch, G=1:10)
# ch_clust$BIC
# plot(ch_clust, "BIC", palette="jco")
# summary(ch_clust)
# 
# #Nbclust Package----
# nb.ch<- NbClust(data=dat.ch, diss=NULL, distance="euclidean", min.nc=2, max.nc=8, method="kmeans")
# 
# factoextra::fviz_nbclust(nb.ch)+theme_minimal()+ggtitle(("Seven Mile: NbClust's optimal number of clusters"))
# 
# #Kmean cluster analysis----
# cl.ch <- kmeans(ch.kdat[,8],3)%>%
#   glimpse()
# ch.kdat$Cluster <-as.factor(cl.ch$cluster)
# glimpse(ch.kdat)
# 
# #cliff head: plot with cluster identified by colour---
# 
# ch.cluster.plot <-ggplot(data=ch.kdat, aes(x=initial.cl, y=inc, col=Cluster))+ 
#   geom_point()+
#   theme_bw()+Theme1+
#   ggtitle ("Cliff Head: K-means clustering K=3")+
#   ylab("Relative Growth (mm)")+
#   xlab("Initial Carapace Length (mm)") 
# ch.cluster.plot
# 
# #Subset to 1 moult
# ch.m1<-ch.kdat%>%
#   filter(Cluster=="2")%>%
#   glimpse()
# 
# 
# 
# #Golden Ridge: Kmeans Cluster Analaysis----
# glimpse(k.dat)
# gr.kdat<-k.dat%>%
#   filter(Location.int=="Golden Ridge")%>%
#   glimpse()
# 
# 
# dat.gr<- gr.kdat%>%
#   select(inc)%>%
#   glimpse()
# 
# #The "Elbow" Method----
# 
# fviz_nbclust(dat.gr, kmeans, method = "wss")+
#   geom_vline(xintercept=3, linetype=2)+
#   labs(subtitle = "Golden Ridge: Elbow Method")
# 
# #GR: The Gap Statistic Analysis----
# #better way
# fviz_nbclust(dat.gr, kmeans, nstart=25, method = "gap_stat", nboot=50)+
#   labs(subtitle = "Golden Ridge: Gap Statistic method")
# 
# #Silhouette method----
# 
# fviz_nbclust(dat.gr, kmeans, method = "silhouette")+
#   labs(subtitle = "Golden Ridge: Silhouette method")
# 
# #Model-Based Clustering: Golden Rdige----
# help('Mclust')
# 
# gr_clust<- Mclust(dat.gr, G=1:10)
# gr_clust$BIC
# plot(gr_clust, "BIC", palette="jco")
# summary(gr_clust)
# 
# #Nbclust Package----
# nb.gr<- NbClust(data=dat.gr, diss=NULL, distance="euclidean", min.nc=2, max.nc=8, method="kmeans")
# 
# factoextra::fviz_nbclust(nb.gr)+theme_minimal()+ggtitle(("Golden Ridge: NbClust's optimal number of clusters"))
# 
# #Kmean analysis----
# cl.gr <- kmeans(gr.kdat[,8],3)%>%
#   glimpse()
# 
# gr.kdat$Cluster <-as.factor(cl.gr$cluster)
# glimpse(gr.kdat)
# 
# #Golden Ridge: plot with cluster identified by colour
# gr.cluster.plot <-ggplot(data=gr.kdat, aes(x=initial.cl, y=inc, col=Cluster))+ 
#   geom_point()+
#   theme_bw()+Theme1+
#   ggtitle ("Golden Ridge: K-means clustering K=3")+
#   ylab("Relative Growth (mm)")+
#   xlab("Initial Carapace Length (mm)") 
# gr.cluster.plot
# 
# #Subset to 1 moult
# gr.m1<-gr.kdat%>%
#   filter(Cluster=="1")%>%
#   glimpse()
# 
# 
# #combine all moult 1----
# 
# all.m1<-rbind(sm.m1, ir.m1, ch.m1, gr.m1)%>%
#   glimpse()
# 
# #and w.o Jan-Ap for SM
# all.m1.edit<-rbind(sm.m1.new, ir.m1, ch.m1, gr.m1)%>%
#   glimpse()
# 
# 
# 
# #Cluster plot----
# all.cluster.plot <-ggplot(data=all.m1, aes(x=initial.cl, y=inc, col=Location.int))+ 
#   geom_point()+
#   theme_bw()+Theme1+
#   ggtitle ("Seven Mile: K-means clustering K=3")+
#   ylab("Relative Growth (mm)")+
#   xlab("Initial Carapace Length (mm)")#+
#   #facet_wrap(~Site.int)
# all.cluster.plot
# 
# 
# #Models----
# glimpse(all.m1)
# 
# #Scenario 1----
# #Growth increase by Location, Sex, initial CL as covariates and Site nested randomly within Location 
# mod.1 <- lmer(inc~Location.int+Sex.int+initial.cl+ (1|Site/Location.int), data=all.m1)
# summary(mod.1)
# anova(mod.1)
# 
# #Scenario 2----
# #On edited Data: No Jan-April
# mod.2 <- lmer(inc~Location.int+Sex.int+initial.cl+ (1|Site/Location.int), data=all.m1.edit)
# summary(mod.2)
# 
# mod.3 <- lmer(inc~Location.int+Sex.int+initial.cl+ (1|Site/Location.int), data=all.m1.3)
# summary(mod.2)
# 
# 
# #Box Plots------
# #Order Location
# glimpse(all.m1)
# all.m1$Location.int<-factor(all.m1$Location.int, levels = c("Seven Mile", "Irwin Reef", "Cliff Head", "Golden Ridge"))
# all.m1.edit$Location.int<-factor(all.m1.edit$Location.int, levels = c("Seven Mile", "Irwin Reef", "Cliff Head", "Golden Ridge"))
# 
# #For Kmeans moult 1
# k.dat.box<-ggplot(data= all.m1.edit,aes(x=Location.int, y=inc),notch=FALSE,position = dodge1, outlier.shape = NA)+
#   geom_boxplot(outlier.color = NA, notch=FALSE)+
#   geom_jitter(width = 0.1, height = NULL, alpha=0.5)+ #
#   guides(fill=FALSE)+
#   theme(panel.background = element_blank(),axis.line = element_line(colour = "black"))+
#   #stat_boxplot(geom='errorbar')+          # Adds error bars
#   stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ #this is adding the dot for the mean
#   theme_bw()+Theme1+
#   #coord_cartesian(ylim=c(0,3.5))+
#   ggtitle ("Growth per Moult: SM(k=4) & adjusted, IR(k=3), CH(k=3), GR(k=3)")+
#   theme(plot.title = element_text(hjust = 0, size=12, face = "plain"))+
#   theme(axis.text.x = element_text(angle=90))+     #Changes angle of site names
#   ylab("Growth (mm)")+
#   xlab("Location")# +
#   #facet_grid(. ~ Sex.int) 
# k.dat.box
