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
library(lubridate)
library(readr)
options(dplyr.width = Inf) #enables head() to display all coloums
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
# library(devtools) #run once
# devtools::install_github("beckyfisher/FSSgam_package") #run once
library(FSSgam)

# Set work directory----

work.dir=("~/workspace/Analysis_Miller_WRL") #for ecocloud server
work.dir=("Z://Analysis_Miller_lobster") #for laptop



## Sub directories ----
data.dir<-paste(work.dir,"Data",sep="/")
map.dir<-paste(work.dir,"Map Layers",sep="/")
plots.dir<-paste(work.dir,"Plots",sep="/")
model.dir<-paste(work.dir,"Model_out_catch",sep="/")


# Bring in and format the data----
name<-"catch"

setwd(data.dir)
dir()


dat <-read_csv("sub.legal.sw.sst.csv")%>%
   
  dplyr::rename(response=Count,
                Taxa=sizeclass)%>%
  # #   Transform variables
  mutate(Date=as.factor(yday(Date)))%>% #as julian day
  # mutate(recap.Date=as.factor(yday(recap.Date)))%>% #as julian day
  mutate(Site=as.factor(Site))%>%
  mutate(Location=as.factor(Location))%>%
  # mutate(Taxa="catch")%>%
  # na.omit()%>%
  glimpse()

names(dat)


ggplot(data=dat,aes(y=response,x=Location))+
  # geom_smooth(method="gam")+
  geom_boxplot(notch=T)+
  geom_point(alpha=0.25)+
  facet_grid(.~Taxa)



# Set predictor variables---
pred.vars.fact=c("Location")
# "Site"


pred.vars.cont=c("Hs.m.sw",
                 "T1.s.sw",
                 # "Hs.m.sea",
                 "sst") 
# Removed correlated
# "Date","T1.s.sw",
# "T1.s.sea",
# "Hs(m).tot","Tp(s).tot","T1(s).tot","Tp(s).sea","Tp(s).sw","Dir(deg).sw","Dir(deg).sea",

# Check for correalation of predictor variables- remove anything highly correlated (>0.95)---
round(cor(dat[,pred.vars.cont]),2)




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
unique.vars.use=unique(as.character(dat$Taxa))



# Run the full subset model selection----
setwd(model.dir) #Set wd for example outputs - will differ on your computer

# Presets
glimpse(dat)
names(dat)
resp.vars=unique.vars.use
use.dat=dat
out.all=list()
var.imp=list()


# # Loop through the FSS function for each Taxa----
for(i in 1:length(resp.vars)){
  use.dat=dat[which(dat$Taxa==resp.vars[i]),]
  
  Model1=gam(response~s(sst,k=3,bs='cr')+ s(Site,bs='re')+s(Date,bs='re'),family=tw(),  data=use.dat)
  # gam.check(Model1)
  # plot.gam(Model1)
  # summary(Model1)
  
  model.set=generate.model.set(use.dat=use.dat,
                               test.fit=Model1,
                               pred.vars.cont=pred.vars.cont,
                               pred.vars.fact=pred.vars.fact,
                               factor.factor.interactions = F,
                               smooth.smooth.interactions = F,
                               factor.smooth.interactions = F,
                               k=3,
                               cov.cutoff = 0.7,
                               null.terms="s(Site,bs='re')+s(Date,bs='re')")
                               # r2.type="r2",

  
  out.list=fit.model.set(model.set,
                         max.models=600,
                         parallel=T)
  names(out.list)
  
  out.list$failed.models # examine the list of failed models
  mod.table=out.list$mod.data.out  # look at the model selection table
  mod.table=mod.table[order(mod.table$AICc),]
  mod.table$cumsum.wi=cumsum(mod.table$wi.AICc)
  out.i=mod.table[which(mod.table$delta.AICc<=10),]
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

