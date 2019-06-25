# Explore catch data----
rm(list=ls()) # Clears memory

# Study name----
study<-"Fabens.Simon"

#library----
library(scales)
library(lubridate)
library(tidyr)
library(magrittr)
library(dplyr)
library(googlesheets)
library(stringr)
library(ggplot2)

# Set work directory----

work.dir=("Z://Analysis_Miller_lobster") #for Ash's laptop

# ## Sub directories ----
data.dir<-paste(work.dir,"Data",sep="/")
map.dir<-paste(work.dir,"Map Layers",sep="/")
plots.dir<-paste(work.dir,"Plots",sep="/")
 
# # Functions----
se <- function(x) sd(x) / sqrt(length(x))
se.min <- function(x) (mean(x)) - se(x)
se.max <- function(x) (mean(x)) + se(x)
sd.min <- function(x) (mean(x)) - sd(x)
sd.max <- function(x) (mean(x)) + sd(x)
scaleFUN <- function(x) sprintf("%.0f", x)

# #Import Data
setwd(data.dir)

#New data making four location
dat.rr<- read.csv("Growth.Data.csv")%>%
  ##   Transform variables
  mutate(Date=as.Date(Date))%>%
  mutate(Date.recap=as.Date(Date.recap))%>%
  #Change location names=to four locations
  mutate(Location=str_replace_all(.$Location,c("Cliff Head"="Low-catch", "Golden Ridge"="Boundary", "Little Horseshoe"="Boundary","White Point"= "Mid","Irwin Reef"="Mid", "Seven Mile"="Control")))%>%
  mutate(Location.recap=str_replace_all(.$Location,c("Cliff Head"="Low-catch", "Golden Ridge"="Boundary", "Little Horseshoe"="Boundary","White Point"= "Mid","Irwin Reef"="Mid", "Seven Mile"="Control")))%>%
  #filter 
  filter(!is.na(Carapace.length))%>%
  filter(!is.na(Colour))%>%
  filter(!is.na(Sex))%>%
glimpse()

unique(dat.rr$Location) 
unique(dat.rr$Location.recap)
head(dat.rr)


length(unique(dat.rr$Tag.number)) #1348
unique(dat.rr$Location) 
names(dat.rr)

dat.rr%<>%
  dplyr::rename(rel.date = Date, 
                rec.date = Date.recap, 
                rlloc= Location, 
                rec.length=Carapace.length.recap, 
                rlclength=Carapace.length, 
                site=Site,
                growth=inc)

dat.rr%<>%mutate(Lyrs=(rec.date-rel.date)/365) %>% 
  select(rec.date, Tag.number, rec.length, Sex, rlloc, site, Total.damage, rel.date, rlclength, growth, Lyrs)%>%
  glimpse()

dat.rr%<>%
  mutate(Lyrs=as.numeric(as.Date(rec.date, '%d/%m/%Y')-as.Date(rel.date, '%d/%m/%Y'))/365,growth=rec.length-rlclength)%>%
  mutate(sex=tolower(substr(Sex,1,1))) %>% 
  filter(sex%in% c('f','m'), !is.na(sex)) %>%
  mutate(rcyear=format(as.Date(rec.date,'%d/%m/%Y'),'%Y'),
         rlyear=format(as.Date(rel.date,'%d/%m/%Y'),'%Y')) %>%
  mutate(rcmonth=format(as.Date(rec.date,'%d/%m/%Y'),'%m'),
         rlmonth=format(as.Date(rel.date, '%d/%m/%Y'),'%m'))%>%
  glimpse()


rec<-dat.rr%>%
  dplyr::mutate(rlmonth = as.integer(rlmonth))%>%
  dplyr::mutate(rcmonth = as.integer(rcmonth))%>%
  dplyr::mutate(rlyear = as.integer(rlyear))%>%
  dplyr::mutate(rcyear = as.integer(rcyear))%>%
  glimpse()

unique(rec$rlloc)

head(rec)
#Set Fabens model
fab <- function(pin,tmp=tmp,flag='solve',sex='split'){
  pin <- as.numeric(pin)
  if(sex=='split'){Linf <- ifelse(tmp$sex=='f', pin[1],pin[3])
  K <- ifelse(tmp$sex=='f', pin[2],pin[4])
  sd=pin[5]}
  if(sex=='comb'){Linf <- pin[1]
  K <- pin[2]
  sd=pin[3]}
  est = (Linf-tmp$rlclength)*(1-exp(-K*tmp$Lyrs))
  LL <- -sum(dnorm(tmp$growth, est, sd,T))
  if(flag=='print') return(est)
  if(flag=='solve') return(LL)
}


rec %<>% 
  filter(rlloc!='Control') %>% #remove due to unbalance
  mutate(loc=as.numeric(as.factor(rlloc))) %>% 
  mutate(sloc=(as.numeric(as.factor(sex))-1)*3+loc)
unique(rec$sloc)
tapply(rec$sloc, list(rec$sex,rec$rlloc), mean)
tapply(rec$sloc, list(rec$sex,rec$rlloc), length)


## SMB is very diffferent , most losters released in Noveember, whereas at other sites all released in May and june
# Not sure whether we can compare like this without a seasonal growth curve.
# might try
tapply(tmp$Sex, list(tmp$loc, trunc(tmp$Lyrs/0.1)*0.1), length)
##  Ned to have comparable times at liberty, maybe this is a good upper and lower limit
## need to get balanced data
tmp <- rec[rec$Lyrs>0.081 & rec$Lyrs<1.1 & rec$rlloc!='Seven Mile',] #  #Filtered to more than 2 months at liberty
tapply(tmp$sloc, list(tmp$sex,tmp$rlloc), length)

tmp <- tmp[!is.na(tmp$Sex),]

head(tmp)
sort(unique(rec$rlloc))
pin <- log(c(1,rep(0.3,6),1)) 
head(tmp)
pin

#Test for three locations-Seven Mile removed----
unique(tmp$sloc)
head(tmp)

#Split by all- 8 parameters (3 x location, 2 x sex, 2 x parameters)
fab3 <- function(pin,tmp=tmp,flag='solve',split=T){   ## split by all
  pin <- exp(as.numeric(pin))
  Linf <- pin[1]*100
  K <- pin[2:7][tmp$sloc] 
  sd <- pin[8]
  est <- (Linf-tmp$rlclength)*(1-exp(-K*tmp$Lyrs))
  LL <- -sum(dnorm(tmp$growth, est, sd,T))
  if(flag=='print') return(est)
  if(flag=='solve') return(LL)
}


#Split by sex- four same parameters
fab5 <- function(pin,tmp=tmp,flag='solve'){   ## split by sex
  pin <- exp(as.numeric(pin))
  Linf <- pin[1]*100
  K <- pin[2:3][as.numeric(as.factor(tmp$sex))] 
  sd <- pin[4]
  est <- (Linf-tmp$rlclength)*(1-exp(-K*tmp$Lyrs))
  LL <- -sum(dnorm(tmp$growth, est, sd,T))
  if(flag=='print') return(est)
  if(flag=='solve') return(LL)
}

#No splits-Three parameters
fab4 <- function(pin,tmp=tmp,flag='solve'){  ## no splits
  pin <- exp(as.numeric(pin))
  Linf <- pin[1]*100
  K <- pin[2]
  sd <- pin[3]
  est <- (Linf-tmp$rlclength)*(1-exp(-K*tmp$Lyrs))
  LL <- -sum(dnorm(tmp$growth, est, sd,T))
  if(flag=='print') return(est)
  if(flag=='solve') return(LL)
}

#### splits by location-5 parameters
fab2 <- function(pin,tmp=tmp,flag='solve',split=T){  
  pin <- exp(as.numeric(pin))
  Linf <- pin[1]*100
  if(split){ 
    K <- pin[2:4][tmp$loc] 
    sd <- pin[5]
  }else {
    K <- pin[2]
    sd <- pin[3]}
  est <- (Linf-tmp$rlclength)*(1-exp(-K*tmp$Lyrs))
  LL <- -sum(dnorm(tmp$growth, est, sd,T))
  if(flag=='print') return(est)
  if(flag=='solve') return(LL)
}

#Maybe don't need this with new data & only three locations
# twoloc <- function(pin,tmp=tmp,flag='solve'){  ### splits by loc
#   pin <- exp(as.numeric(pin))
#   Linf <- pin[1]*100
#   K <- pin[2:3][tmp$twoloc] 
#   sd <- pin[4]
#   est <- (Linf-tmp$rlclength)*(1-exp(-K*tmp$Lyrs))
#   LL <- -sum(dnorm(tmp$growth, est, sd,T))
#   if(flag=='print') return(est)
#   if(flag=='solve') return(LL)
# }

#Simons impressive pair-wise stuff----
# tmp %<>% mutate(twoloc=ifelse(loc==1,2,1))
# head(tmp[grepl('cliff', tmp$rlloc,ignore.case = T),])
# head(tmp[grepl('seven', tmp$rlloc,ignore.case = T),])
# (aout.2loc1 <- nlminb(log(c(1,0.5,0.5,1)), twoloc, tmp=tmp, control = list(iter.max=1000, eval.max=1000)))
# tmp %<>% mutate(twoloc=ifelse(loc==4,2,1))
# (aout.2loc4 <- nlminb(log(c(1,0.5,0.5,1)), twoloc, tmp=tmp, control = list(iter.max=1000, eval.max=1000)))
# tmp %<>% mutate(twoloc=ifelse(loc==2,2,1))
# (aout.2loc2 <- nlminb(log(c(1,0.5,0.5,1)), twoloc, tmp=tmp, control = list(iter.max=1000, eval.max=1000)))
# tmp %<>% mutate(twoloc=ifelse(loc==3,2,1))
# (aout.2loc3 <- nlminb(log(c(1,0.5,0.5,1)), twoloc, tmp=tmp, control = list(iter.max=1000, eval.max=1000)))
# head(tmp)

#Split-----
#Split by all- Now 8 pars
(aout.8par <- nlminb(pin, fab3, tmp=tmp, control = list(iter.max=1000, eval.max=1000)))
exp(aout.8par$par)*c(100,1,1,1,1,1,1,1)

#Split by Location -Now 5 pars
(aout.5par <- nlminb(log(c(1,0.5,0.5,0.5,1)), fab2, tmp=tmp, control = list(iter.max=1000, eval.max=1000)))
exp(aout.5par$par)*c(100,1,1,1,1)

#Split by Sex - Same
(aout.4par <- nlminb(log(c(1,0.5,0.5,1)), fab5, tmp=tmp, control = list(iter.max=1000, eval.max=1000)))
exp(aout.4par$par)*c(100,1,1,1)

#All combine-No split
(aout.comb <- nlminb(log(c(1,0.5,1)), fab4, tmp=tmp, control = list(iter.max=1000, eval.max=1000)))
exp(aout.comb$par)*c(100,1,1)

## I have made likelihood ratio test into a function, just include the less complex model first  (less parameters)
#  You should compare everything to the simplest model, u are testing whether a more complex model is a better fit and justified based on the incerase in parameters

LR_test_func <- function(x,y) return(1-pchisq(abs(x$objective-y$objective), length(y$par)-length(x$par)))

#no split vs all split 
LR_test_func(aout.comb,aout.8par)  ## 0.02644449 Significantly different

#no split vs sex split 
LR_test_func(aout.comb,aout.4par)  ## not significant different-So sex is not signifcant

#no split vs loc split 
LR_test_func(aout.comb,aout.5par)  ## 0.004277475 Significantly different- So Location is significant

#Results show that only Location is significant-So use the 5 parameter model
pars <- exp(aout.5par$par)*c(100,1,1,1,1)
View(pars)

#Simons fancy two-location split
#no split vs loc1 split 
#LR_test_func(aout.comb,aout.2loc1)  ## not significant different
#no split vs loc2 split 
#LR_test_func(aout.comb,aout.2loc2)  ## ************** boarderline significant different loc == Low

#Plot Residuals (in ggplot-sorry Simon)----
# Theme-----
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # legend.position = "none",
    text=element_text(size=12),
    strip.text.y = element_text(size = 12,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=12, face = "bold"),
    axis.title.y=element_text(vjust=0.6, angle=90, size=12, face = "bold"),
    axis.text.x=element_text(size=12,angle = 0, colour = "black"), #, hjust=1,vjust=0.5
    axis.text.y=element_text(size=12,colour = "black"),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.text = element_text(size=14, face="bold"),
    strip.background = element_blank())

#Set up data----
glimpse(rec)
tmp.rec <- rec[rec$Lyrs>0.081 & rec$growth<20,] #>0.1
glimpse(tmp.rec)
tmp.rec <- tmp.rec[!is.na(tmp.rec$sex),]
tmp.rec$est <- fab3(aout.spl$par, tmp=tmp.rec, flag='print',  split=T)
glimpse(tmp.rec)
tmp.rec$rr<-tmp.rec$growth-tmp.rec$est
glimpse(tmp.rec)

#Plot ----
#Release month residuals
#Set order of locations
tmp.rec$rlloc= factor(tmp.rec$rlloc, levels=c('Low-catch','Boundary','Mid'))


residual.month<-ggplot(data=tmp.rec, aes(x=rlmonth, y=rr, col=sex))+
  geom_point(size=1.7, alpha=0.5)+
  scale_colour_manual(values = c("f"="red", "m"="blue"))+ #2=red
  theme_bw()+Theme1+
  ylab("(a)")+
  xlab("Release Month")+
  geom_hline(aes(yintercept=0), linetype="dashed")+
  theme(legend.position = "none")+
  facet_grid(~rlloc) #, labeller = as_labeller(labels)
residual.month

#Carapace length residuals
glimpse(tmp.rec)
residual.cl<-ggplot(data=tmp.rec, aes(x=rlclength, y=rr, col=sex))+
  geom_point(size=1.7, alpha=0.5)+
  scale_colour_manual(values = c("f"="red", "m"="blue"))+ 
  theme_bw()+Theme1+
  ylab("(b) Residual")+
  xlab("Carapace length at release (mm)")+
  theme(strip.text = element_blank())+
  geom_hline(aes(yintercept=0), linetype="dashed")+
  theme(legend.position = "none")+
  facet_grid(~rlloc)
residual.cl

#Liberty residuals

residual.liberty<-ggplot(data=tmp.rec, aes(x=Lyrs, y=rr, col=sex))+
  geom_point(size=1.7, alpha=0.5)+
  scale_colour_manual(values = c("f"="red", "m"="blue"))+ 
  theme_bw()+Theme1+
  ylab("(c)")+
  xlab("Time at liberty (yr)")+
  geom_hline(aes(yintercept=0), linetype="dashed")+
  theme(strip.text = element_blank())+
  theme(legend.position = "none")+
  facet_grid(~rlloc)
residual.liberty

#Save ggplots----
library(grid)
library(gridExtra)
setwd(plots.dir)

# To see what they will look like use grid.arrange() 
grid.arrange(residual.month,residual.cl,residual.liberty,nrow=3,ncol=1)

# Use arrangeGrob ONLY - as we can pass this to ggsave! 
combine.plot<-arrangeGrob(residual.month,residual.cl,residual.liberty,nrow=3,ncol=1)

#save
ggsave(combine.plot,file="fabens.residuals.190619.png", width = 20, height = 15,units = "cm")


#Dummy plot----
glimpse(tmp)
unique(tmp$rlloc)
pars

dum <- data.frame(age=2:10) #Or (age=2:10)
#Five parameter model
dum$boundary<- pars[1]*(1-exp(-pars[2]*dum$age))
dum$low<- pars[1]*(1-exp(-pars[3]*dum$age))
dum$mid<- pars[1]*(1-exp(-pars[4]*dum$age))

par(mfrow=c(1,1))

plot(dum$age, dum$boundary, type='l', col="#E7B800",lwd=2, xlab='Relative age(years)', ylab='Carapace length (mm)', ylim=c(40,110),  bty='l', cex.lab=1.3, cex.axis=1.2) #or  ylim=c(20,110)
lines(dum$age, dum$low, type='l', col="red", lwd=2)
lines(dum$age, dum$mid, type='l', col="#00AFBB", lwd=2)

legend("bottomright", title = "Location",
       legend=c("Low-catch", "Boundary", "Mid"), 
       col = c("red","#E7B800","#00AFBB"), 
       lty= c(1,1,1), lwd=c(2,2,2), box.lty=0, ncol=1, cex=1.3)


#Attempt to plot in ggplot-----
glimpse(dum)
library(ggrepel)

long_dum<- dum%>%
  gather(Location, CL, low, boundary, mid)%>%
  mutate(Location=str_replace_all(.$Location, c("boundary"="Boundary","low"="Low-catch", "mid"="Mid")))%>%
  mutate(label = if_else(age == max(age), as.character(Location), NA_character_)) %>%
  mutate(label=str_replace_all(.$label, c("Boundary"="(a,b) Boundary","Low-catch"=" (a) Low-catch", "Mid"="   (b) Mid")))%>%
  glimpse()

faben.plot<-ggplot(data=long_dum, aes(x=age, y=CL, col=Location))+
  geom_smooth(method = "loess")+
  ylab("Carapace length (mm)")+
  xlab("Relative age (years)")+
  scale_color_manual(values = c("#E7B800","red", "#00AFBB"))+
  geom_text(aes(label = label), hjust = -.15)+ 
  xlim(2, 12)+
  ylim(30, 110)+
  theme_bw()+
  Theme1+
  theme(legend.position = "none")
faben.plot

#save plot
setwd(plots.dir)
ggsave(faben.plot,file="fabens.plot.220619.png", width = 15, height = 11,units = "cm")

#Pairwise comparisions----

tmp <- rec[rec$Lyrs>0.081 & rec$rlloc%in%c('Low-catch',"Mid"),]
tmp <- tmp[!is.na(tmp$Sex),]
glimpse(tmp)

(sout.low_boundary <- nlminb(log(c(1,0.5,0.5,0.5,1)), fab2, tmp=tmp, split=T))
exp(sout.low_boundary$par)*c(100,1,1,1,1)
split<- exp(sout.low_boundary$par)*c(100,1,1,1,1)

## Locations combined
glimpse(tmp)
(bout.low_boundary <- nlminb(log(c(1,0.5,1)), fab4, tmp=tmp)) #0.5, 0.5 , split=F
exp(bout.low_boundary$par)*c(100,1,1) #1,1
combined<- exp(bout.low_boundary$par)*c(100,1,1) #1,1

bout.low_boundary
sout.low_boundary$objective
##Test whether we need two lots of parameters
LR_test = 1-pchisq(abs(bout.low_boundary$objective-sout.low_boundary$objective), 2)
LR_test  ## very much an improvement


#Low vs. Boundary= 0.5594925
#Low vs. Mid = 0.01089531
#Mid vs. Boundary = 0.1026145

#Get full resulst, think I use chisq.test, just not sure what on?
chisq.test(combined, split)
