# A simple function for full subsets multiple regression in ecology with R
# 
# R. Fisher
# S.K. Wilson
# S.M. Sin
# A.C. Lee
# Dr Tim J. Langlois

# Explore catch data----
rm(list=ls()) # Clears memory

# librarys----
detach("package:plyr", unload=TRUE)#will error - don't worry
library(tidyr)
library(dplyr)
options(dplyr.width = Inf) #enables head() to display all coloums
library(readr)
library(lubridate)
library(mgcv)
library(MuMIn)
library(car)
library(doBy)
library(ggplot2)
library(RColorBrewer)
library(doSNOW)
library(gamm4)
library(RCurl) #needed to download data from GitHub


# install package----
# devtools::install_github("beckyfisher/FSSgam_package") #run once
library(FSSgam)

# Set work directory----


work.dir=("~/workspace/Analysis_Miller_WRL") #for ecocloud server
work.dir=("Z://Analysis_Miller_lobster") #for laptop


## Sub directories ----
data.dir<-paste(work.dir,"Data",sep="/")
map.dir<-paste(work.dir,"Map Layers",sep="/")
plots.dir<-paste(work.dir,"Plots",sep="/")
model.dir<-paste(work.dir,"Model_out_growth",sep="/")


# Bring in and format the data----
name<-"growth"

setwd(data.dir)
dir()

#Import Growth Data

dat<- read_csv("FINAL.growth.swell.sst.csv")%>%
  arrange(recap.Date,lat.rec)%>%
  fill(19:30, .direction = c("down"))%>% #Some data is missing from certain days. k. cool. whatever.
  # #   Transform variables
  mutate(recap.Date=yday(recap.Date))%>%
  mutate(Site=as.factor(Site))%>%
  mutate(Location=as.factor(Location.int))%>%
  rename(response=inc,
         Taxa=Sex.int)%>%
  filter(moult%in%c("1","2"))%>% #only 1 moult
  # filter(moult%in%c("1"))%>% #only 1 moult
  # drop_na(Site)%>% 
  filter(!Location.int=="Golden Ridge")%>%
  glimpse()

names(dat)

table(dat$Taxa, dat$Site)
table(dat$Taxa, dat$Location.int)


ggplot(data=dat,aes(x=Location,y=response))+
  geom_boxplot(notch=T)+
  facet_grid(moult~Taxa)


# Set predictor variables---
pred.vars.fact=c("Location")
# "Site"

pred.var.linear=c("moult")

pred.vars.cont=c("avg.Hs.m.sw",
                 "avg.T1.s.sw",
                 "avg.sst"
                 ) 
# Removed correlated
# "Date",
# "Hs(m).tot","Tp(s).tot","T1(s).tot","Tp(s).sea","Tp(s).sw","Dir(deg).sw","Dir(deg).sea",

# Check for correalation of predictor variables- remove anything highly correlated (>0.95)---
round(cor(dat[,pred.vars.cont]),2)



# 
# # Plot of likely transformations - thanks to Anna Cresswell for this loop!
# par(mfrow=c(3,2))
# for (i in pred.vars.cont) {
#   x<-dat[ ,i]
#   x = as.numeric(unlist(x))
#   hist((x))#Looks best
#   plot((x),main = paste(i))
#   hist(sqrt(x))
#   plot(sqrt(x))
#   hist(log(x+1))
#   plot(log(x+1))
# }

# Review of individual predictors - we have to make sure they have an even distribution---
#If the data are squewed to low numbers try sqrt>log or if squewed to high numbers try ^2 of ^3
# Decided that X4mm, X2mm, X1mm and X500um needed a sqrt transformation
#Decided Depth, x63um, InPreds and BioTurb were not informative variables. 




# Check to make sure Response vector has not more than 80% zeros----
unique.vars=unique(as.character(dat$Taxa))
unique.vars.use=character()
for(i in 1:length(unique.vars)){
  temp.dat=dat[which(dat$Taxa==unique.vars[i]),]
  if(length(which(temp.dat$response==0))/nrow(temp.dat)<0.8){
    unique.vars.use=c(unique.vars.use,unique.vars[i])}
}
unique.vars.use     




# Run the full subset model selection----
setwd(model.dir) #Set wd for example outputs - will differ on your computer

# Presets
glimpse(dat)
names(dat)
resp.vars=unique.vars.use
use.dat=dat
out.all=list()
var.imp=list()


# Loop through the FSS function for each Taxa----
for(i in 1:length(resp.vars)){
  use.dat=dat[which(dat$Taxa==resp.vars[i]),]
  
  Model1=gam(response~s(avg.sst,k=3,bs='cr')+s(recap.Date,bs='re')+s(initial.cl,k=3,bs='cr'),family=tw(),offset=diff ,  data=use.dat)
  # par(mfrow=c(2,2))
  # gam.check(Model1)
  # quasipoisson(link = "log")
  
  
  model.set=generate.model.set(use.dat=use.dat,
                               test.fit=Model1,
                               pred.vars.cont=pred.vars.cont,
                               pred.vars.fact=pred.vars.fact,
                               factor.factor.interactions = F,
                               smooth.smooth.interactions = F,
                               factor.smooth.interactions = F,
                               linear.vars = pred.var.linear,
                               k=3,
                               cov.cutoff = 0.5,
                               null.terms="s(recap.Date,bs='re')+s(initial.cl,k=3,bs='cr')") #maybe should be mid date?
  

  
  out.list=fit.model.set(model.set,
                         max.models=600,
                         parallel=T)
  names(out.list)
  
  out.list$failed.models # examine the list of failed models
  mod.table=out.list$mod.data.out  # look at the model selection table
  mod.table=mod.table[order(mod.table$AICc),]
  mod.table$cumsum.wi=cumsum(mod.table$wi.AICc)
  out.i=mod.table[which(mod.table$delta.AICc<=4),]
  out.all=c(out.all,list(out.i))
  var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Either raw importance score
  # var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Or importance score weighted by r2
  
  # plot the best models
  for(m in 1:nrow(out.i)){
    best.model.name=as.character(out.i$modname[m])
    
    png(file=paste(name,m,resp.vars[i],"mod_fits.png",sep="_"))
    if(best.model.name!="null"){
      par(mfrow=c(3,1),mar=c(9,4,3,1))
      best.model=out.list$success.models[[best.model.name]]
      plot(best.model,all.terms=T,pages=1,residuals=T,pch=16)
      mtext(side=2,text=resp.vars[i],outer=F)}  
    dev.off()
  }
}




# Model fits and importance---
names(out.all)=resp.vars
names(var.imp)=resp.vars
all.mod.fits=do.call("rbind",out.all)
all.var.imp=do.call("rbind",var.imp)
write.csv(all.mod.fits[,-2],file=paste(name,"all.mod.fits.csv",sep="_"))
write.csv(all.var.imp,file=paste(name,"all.var.imp.csv",sep="_"))
