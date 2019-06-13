#Cormack-Jolly Seber
rm(list=ls()) #clears memory

# librarys----

library(FSA)
library(marked)
library(dplyr)
library(reshape)
library(tidyr)
library(tidyverse)
library(ggplot2)

# Set work directory----

work.dir=("~/GitHub/Analysis_Miller_WRL") #for Tim's github
work.dir=("~/workspace/Analysis_Miller_WRL") #for ecocloud server

# Set sub-directories----

data.dir=paste(work.dir,"Data",sep="/")
plot.dir=paste(work.dir,"Plots",sep="/")

#Bring in recapture data from ecocloud----

setwd("~/workspace/Analysis_Miller_WRL/Data")

dat.rr<- read_csv("growth.edited.csv")%>% #Edited: cleaned and no Jan-April
  filter(Tag.number!="190428" & Tag.number!="190188" & Tag.number!="190124" &Tag.number!="190443")%>%
  filter(Tag.number!="K2400"&Tag.number!="K1617"&Tag.number!="K1221")%>%
  filter(is.na(mini.site)| mini.site!="Rivermouth")%>% 
  dplyr::rename("Location"="Site")%>%
  glimpse()

#for model

dat.mod <- dat.rr%>%
  select(Tag.number, Sex, Location, Trip, Carapace.length, mini.site)%>%
  glimpse()


#need to filter out recaptures caught same trip for this-----
#unless I use 'day number' as the sampling incident instead of 'trip'

ddd <- dat.mod %>%
  group_by(Tag.number)%>% #Site removed
  dplyr::summarise(min=min(Trip),max=max(Trip),no.trips=n())%>%
  filter(max>min)%>% #Fi
  glimpse()

dd <- semi_join(dat.mod, ddd)%>% 
  group_by(Tag.number,Trip)%>%
  mutate("recap"=1)%>%
  tibble::rowid_to_column("ID")%>%
  glimpse()

#Spread function for trips----

d  <- tidyr::spread(dd, key="Trip", value= "recap")%>%
  select(Tag.number, Sex, Location,mini.site, T0, T1, T2, T3, T4, T5, T6, T7, T8, T9)%>%
  glimpse()

d %>%
  mutate_if(is.numeric, ~replace_na(., 0))%>%
  glimpse()

write.csv(d, "data.cjs.csv")

#Now need to collapse duplicates----
dat<- d%>%
  group_by(Tag.number)%>%
  summarise_all(funs(if(is.numeric(.)) max(., na.rm = TRUE) else first(.)))%>%
  ungroup()%>%
  glimpse()

#Replace '-Inf' with zero (couldn't find a quicker way)---
dat <- dat%>%
  mutate(T0=str_replace_all(.$T0,c("-Inf"="0")))%>%
  mutate(T1=str_replace_all(.$T1,c("-Inf"="0")))%>%
  mutate(T2=str_replace_all(.$T2,c("-Inf"="0")))%>%
  mutate(T3=str_replace_all(.$T3,c("-Inf"="0")))%>%
  mutate(T4=str_replace_all(.$T4,c("-Inf"="0")))%>%
  mutate(T5=str_replace_all(.$T5,c("-Inf"="0")))%>%
  mutate(T6=str_replace_all(.$T6,c("-Inf"="0")))%>%
  mutate(T7=str_replace_all(.$T7,c("-Inf"="0")))%>%
  mutate(T8=str_replace_all(.$T8,c("-Inf"="0")))%>%
  mutate(T9=str_replace_all(.$T9,c("-Inf"="0")))%>%
  glimpse()



#917 unique tag number that have been re-caught

dat.1<- dat%>%
  unite(ch, T0, T1, T2,T3,T4,T5,T6,T7,T8,T9, sep = "", remove=FALSE)%>%
  ungroup()%>%
  glimpse()

#Create dataframe for the CJS model----

dat.cjs <-dat.1%>%
  select(Tag.number, Sex, Location,mini.site, ch)%>%
  dplyr::rename(site=mini.site)%>%
  dplyr::rename(sex=Sex)%>%
  mutate(sex=as.factor(sex))%>%
  glimpse()

setwd(data.dir)
write.csv(dat.cjs, "dat.cjs.txt")
write.csv(dat.cjs, "dat.cjs.csv") #looses zeros?
# 
# 
# #Clear History
# rm(list=ls())
# #Bring in Data----
# 
dat.cjs1 <- read.csv("dat.cjs.csv")%>%
  select(Tag.number, sex, Location, site, ch)%>%
  mutate(ch = as.character(ch))%>%
  glimpse()


dat <-read_csv("dat.cjs.txt")%>% #txt fixes the loss of the leading zeros
  select(Tag.number, sex, Location, site, ch)%>%
  mutate(ch = as.character(ch))%>%
  glimpse()
  glimpse()

#Example
data("dipper")
glimpse(dipper)

#y fitting the simplest CJS model with constant φ and p
model=crm(dipper)
model
# 
# dat.cjs1<-dat.cjs%>%
#   select(ch, Sex)%>%
#   glimpse()
# 
# 
# model1=crm(dat.cjs1)
# model1
# 
# model2=crm(dat.cjs)
# model2
#Estimates of precision are not shown because the default is hessian=FALSE and
#it must be set to TRUE to get estimates of precision.
model=cjs.hessian(model)
model
#OR
model1=cjs.hessian(model1)
model1

# You’ll never fit only one model to data, so the most efficient approach is to call
# process.data and make.design.data separately and pass the results to crm so they
# can be used for each fitted model as shown below:
dipper.proc=process.data(dipper)
dipper.ddl=make.design.data(dipper.proc)
Phi.sex=list(formula=~sex)
model=crm(dipper.proc,dipper.ddl,model.parameters=list(Phi=Phi.sex), accumulate=FALSE)
model

#OR
# summary(dat.cjs1)
# dat.proc=process.data(dat.cjs1)
# dat.ddl=make.design.data(dat.proc)
# Phi.sex.cjs=list(formula=~sex)
# model1=crm(dat.proc, dat.ddl, model.parameters = list(Phi=Phi.sex.cjs), accumulate = FALSE)
# model1


# dipper.proc=process.data(dipper)
# dipper.ddl=make.design.data(dipper.proc)
# #below doesn't work
# fit.models=function()
#     + Phi.sex=list(formula=~sex)
#     + Phi.time=list(formula=~time)
#     + p.sex=list(formula=~sex)
#     + p.dot=list(formula=~1)
#     + cml=create.model.list(c("Phi","p"))
#     + results=crm.wrapper(cml,data=dipper.proc, ddl=dipper.ddl,
#                           + external=FALSE,accumulate=FALSE)
#     + return(results)
#     
# dipper.models=fit.models()

#Adding covariates: 
#For φ we will add a static one for 'weight' and a time-varying covariate 'Flood'
#For p, we will add a time-varying individual covariate td (trap dependence)
#which is the 0/1 value of the capture from the previous occasion
#Static covariates are entered in the dataframe in a single column and 
#time-varying covariates havea column and name for each occasion with the 
#appropriate time appended at the end of each name

data(dipper)
# Add a dummy weight field which are random values from 1 to 10
set.seed(123)
dipper$weight=round(runif(nrow(dipper),0,9),0)+1
# Add Flood covariate
Flood=matrix(rep(c(0,1,1,0,0,0),each=nrow(dipper)),ncol=6)
colnames(Flood)=paste("Flood",1:6,sep="")
dipper=cbind(dipper,Flood)
# Add td covariate, but exclude first release as a capture
# splitCH and process.ch are functions in the marked package
td=splitCH(dipper$ch)
td=td[,1:6]
releaseocc=process.ch(dipper$ch)$first
releaseocc=cbind(1:length(releaseocc),releaseocc)
releaseocc=releaseocc[releaseocc[,2]<nchar(dipper$ch[1]),]
td[releaseocc]=0
colnames(td)=paste("td",2:7,sep="")
dipper=cbind(dipper,td)
glimpse(dipper)
# show names
names(dipper)
#Next we process the data with the default CJS model and make the design data
#with parameters that identify which covariates to use for each parameter.
#After specifying the design lists for
#φ and p, they are used in the call to make.design.data and the resulting dataframe
#names are shown below:
# Process data
view(td)
glimpse(dipper)
dipper.proc=process.data(dipper)
# Create design data with static and time varying covariates
design.Phi=list(static=c("weight"),time.varying=c("Flood"))
design.p=list(static=c("sex"),time.varying=c("td"), age.bins=c(0,1,20))
design.parameters=list(Phi=design.Phi,p=design.p)
ddl=make.design.data(dipper.proc,parameters=design.parameters)
names(ddl$Phi)
names(ddl$p) 

#Next we define the models for φ and p that we want to fit and call crm.
Phi.sfw=list(formula=~Flood+weight)
p.ast=list(formula=~age+sex+td)
model=crm(dipper.proc,ddl,hessian=TRUE, model.parameters=list(Phi=Phi.sfw,p=p.ast))

#Below we create a range of data values to compute predicted φ values and then
#plot the results for Flood and non-Flood years for a range of weights.
newdipper=expand.grid(sex=c("Male","Female"),weight=1:10,Flood1=0,
                      Flood2=1,Flood3=1,Flood4=0,Flood5=0,Flood6=0,td2=0,td3=c(0,1),
                      td4=c(0,1),td5=c(0,1),td6=c(0,1),td7=c(0,1))

reals=predict(model,newdata=newdipper,se=TRUE)
reals$Phi$Flood=factor(reals$Phi$Flood,labels=c("Non-flood","Flood"))

library(ggplot2)
dev.off()
#Plot
plot <- ggplot(reals$Phi,aes(weight,estimate,ymin=lcl,ymax=ucl))+
  geom_errorbar(width=0.2)+
  geom_point()+
  geom_line()+
  xlab("\nWeight")+
  ylab("Survival\n")+
  facet_grid(Flood~.)
plot





#New example----
setwd("~/workspace/Analysis_Miller_WRL/Data")
# hw.dat<- read_csv("dataset1.txt")%>%
hw.dat<- read_table("dataset1.txt", col_names = FALSE)%>%
  dplyr::rename(ch = X1)%>%
  glimpse()

colnames(hw.dat)
summary(hw.dat)

attach(hw.dat)
#start with a basic CJS model
hw.proc = process.data(hw.dat, model="CJS")
hw.ddl = make.design.data(hw.proc)

#Specify effects for survival and detection
# survival process
Phi.ct = list(formula=~1) # constant
Phi.time = list(formula=~time) # year effect
# detection process
p.ct = list(formula=~1) # constant
p.time = list(formula=~time) # year effect

install.packages("mark", dependencies = TRUE) 
#Rstudio version to old for mark?

#Run four models with and without year effect
# constant survival, constant recapture
Model.1 = mark(hw.proc,hw.ddl,model.parameters=list(Phi=Phi.ct,p=p.ct),output = FALSE,delete=T)
# constant survival, time-dependent recapture
Model.2 = mark(hw.proc,hw.ddl,model.parameters=list(Phi=Phi.ct,p=p.time),output = FALSE,delete=T)
# time-dependent survival, constant recapture
Model.3 = mark(hw.proc,hw.ddl,model.parameters=list(Phi=Phi.time,p=p.ct),output = FALSE,delete=T)
# time-dependent survival, time-dependent recapture
Model.4 = mark(hw.proc,hw.ddl,model.parameters=list(Phi=Phi.time,p=p.time),output = FALSE,delete=T)



summary(Model.1)$AICc #etc
#lower the better for AIC
phitable = get.real(Model.2,"Phi", se= TRUE)
# names(phitable)
phitable[c("estimate","se","lcl","ucl")][1,]
ptable = get.real(Model.2,"p", se= TRUE)
ptable[c("estimate","se","lcl","ucl")][1:7,]

#Now it’s easy to estimate abundance estimates by calculating the ratios of the 
#number of individuals detected at each occasion over the corresponding estimate of 
#recapture probability. Note that we estimate recapture probabilities, so that we 
#cannot estimate abundance on the first occasion.
# calculate the nb of recaptured individiduals / occasion
obs = gregexpr("1", hw.dat$ch)
n_obs = summary(as.factor(unlist(obs)))
estim_abundance = n_obs[-1]/ptable$estimate[1:7] 
estim_abundance

#We use a boostrap approach to get an idea of the uncertainty surrounding these 
#estimates, in particular to obtain the confidence intervals.
#We first define the number of bootstrap iterations (10 here for the sake of illustration, 
#should be 500 instead, or even 1000 if the computational burden is not too heavy), 
#the number of capture occasions and format the dataset in which we’d like to resample (with replacement).
nb_bootstrap = 10
nb_years = 8
target = data.frame(hw.dat,stringsAsFactors=F)
popsize = matrix(NA,nb_bootstrap, nb_years-1)
set.seed(5)
pseudo = target # initialization

#Finally, we define the model structure and the effects on parameter (same for all bootstrap samples).
# define model structure
hw.proc = process.data(pseudo, model="CJS")
hw.ddl = make.design.data(hw.proc)
# define parameter structure
phi.ct = list(formula=~1)
p.time = list(formula=~time)

for (k in 1:nb_bootstrap){
  # resample in the original dataset with replacement
  pseudo$ch = sample(target$ch, replace=T)
  # fit model with Mark
  res = mark(hw.proc,hw.ddl,model.parameters=list(Phi=phi.ct,p=p.time),delete=TRUE,output=FALSE)
  # get recapture prob estimates
  ptable = get.real(res,"p", se= TRUE)
  # calculate the nb of recaptured individiduals / occasion
  allobs = gregexpr("1", pseudo$ch)
  n = summary(as.factor(unlist(allobs)))
  popsize[k,] <- n[-1]/ptable$estimate[1:(nb_years-1)]
}

#Now we can get confidence intervals:

ci_hw = apply(popsize,2,quantile,probs=c(2.5/100,97.5/100),na.rm=T)
ci_hw
#A plot
plot(1:(nb_years-1),estim_abundance, col="black", type="n", pch=21, xlab="Years", lty=3, ylab="Estimated abundance", main="dataset 1",lwd=3,ylim=c(0,150))
polygon(c(rev(1:(nb_years-1)), 1:(nb_years-1)), c(rev(ci_hw[2,]), ci_hw[1,]), col = 'grey80', border = NA)
lines(1:(nb_years-1), estim_abundance, col="black",lty=3,type='o',lwd=3,pch=21)


#A different model example-----

# CJS with R
mod=crm(dipper,model="CJS",model.parameters=list(Phi=list(formula=~sex),p=list(formula=~time)))
mod$results$beta
# CJS with ADMB
prepare_admb()
mod=crm(dipper,model="CJS",use.admb=TRUE,model.parameters=list(Phi=list(formula=~sex),p=list(formula=~time)))
mod$results$beta
# CJS with HMM in R
mod=crm(dipper,model="hmmCJS",use.admb=TRUE,model.parameters=list(Phi=list(formula=~sex),p=list(formula=~time)))
mod$results$par
# CJS with RMark
summary(mark(dipper,model="CJS",output=FALSE,groups="sex",model.parameters=list(Phi=list(formula=~sex),p=list(formula=~time))))$beta

# JS with R
mod=crm(dipper,model="JS",groups="sex",model.parameters=list(Phi=list(formula=~sex),p=list(formula=~time)))
mod$results$beta
# JS with RMark
summary(mark(dipper,model="POPAN",output=FALSE,groups="sex",model.parameters=list(Phi=list(formula=~sex),p=list(formula=~time))))$beta

#MSCJS with HMM
data(mstrata)
mod=crm(mstrata,model="hmmMSCJS",model.parameters=list(S=list(formula=~stratum),p=list(formula=~stratum),Psi=list(formula=~-1+stratum:tostratum)))
mod$results$beta

# MSCJS with RMark
summary(mark(mstrata,model="Multistrata",output=FALSE,model.parameters=list(S=list(formula=~stratum),p=list(formula=~stratum),Psi=list(formula=~-1+stratum:tostratum))))$beta


> # Add a dummy weight field which are random values from 1 to 10
  > set.seed(123)
> dipper$weight=round(runif(nrow(dipper),0,9),0)+1
> # Add Flood covariate
  > Flood=matrix(rep(c(0,1,1,0,0,0),each=nrow(dipper)),ncol=6)
> colnames(Flood)=paste("Flood",1:6,sep="")
> dipper=cbind(dipper,Flood)
> # Add td covariate, but exclude first release as a capture
  > # splitCH and process.ch are functions in the marked package
  > td=splitCH(dipper$ch)
> td=td[,1:6]
> releaseocc=process.ch(dipper$ch)$first
> releaseocc=cbind(1:length(releaseocc),releaseocc)
> releaseocc=releaseocc[releaseocc[,2]<nchar(dipper$ch[1]),]
> td[releaseocc]=0
> colnames(td)=paste("td",2:7,sep="")
> dipper=cbind(dipper,td)
> # show names
  > names(dipper)



# This example is excluded from testing to reduce package check time
data(example.data)
run.example=function()
{
  PhiTime=list(formula=~time)
  pTimec=list(formula=~time,fixed=list(time=7,value=1))
  pTime=list(formula=~time)
  PhiAge=list(formula=~age)
  Phidot=list(formula=~1)
  PhiweightTime=list(formula=~weight+time)
  PhiTimeAge=list(formula=~time+age)
  mod1=mark(example.data,groups=c("sex","age","region"),
            initial.ages=c(0,1,2))
  mod2=mark(example.data,model.parameters=list(p=pTimec,Phi=PhiTime),
            groups=c("sex","age","region"),initial.ages=c(0,1,2))
  mod3=mark(example.data,model.parameters=list(Phi=Phidot,p=pTime),
            groups=c("sex","age","region"),initial.ages=c(0,1,2))
  mod4=mark(example.data,model.parameters=list(Phi=PhiTime),
            groups=c("sex","age","region"),initial.ages=c(0,1,2))
  mod5=mark(example.data,model.parameters=list(Phi=PhiTimeAge),
            groups=c("sex","age","region"),initial.ages=c(0,1,2))
  mod6=mark(example.data,model.parameters=list(Phi=PhiAge,p=pTime),
            groups=c("sex","age","region"),initial.ages=c(0,1,2))
  mod7=mark(example.data,model.parameters=list(p=pTime,Phi=PhiweightTime),
            groups=c("sex","age","region"),initial.ages=c(0,1,2))
  mod8=mark(example.data,model.parameters=list(Phi=PhiTimeAge,p=pTime),
            groups=c("sex","age","region"),initial.ages=c(0,1,2))
  return(collect.models())
}
example.results=run.example()



