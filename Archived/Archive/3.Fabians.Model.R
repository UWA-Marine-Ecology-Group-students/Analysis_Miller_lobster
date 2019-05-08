# Explore catch data----
rm(list=ls()) # Clears memory

# librarys----
library(tidyr)
library(dplyr)
library(googlesheets)
library(stringr)
# library(measurements)
# library(lubridate)

# Study name----
study<-"Fabians.Data"

# Set work directory----

work.dir=("~/GitHub/Analysis_Miller_WRL") #for Tim's github
work.dir=("~/workspace/Analysis_Miller_WRL") #for ecocloud server

## Sub directories ----
data.dir<-paste(work.dir,"Data",sep="/")
map.dir<-paste(work.dir,"Map Layers",sep="/")
plots.dir<-paste(work.dir,"Plots",sep="/")

# Functions----
se <- function(x) sd(x) / sqrt(length(x))
se.min <- function(x) (mean(x)) - se(x)
se.max <- function(x) (mean(x)) + se(x)
sd.min <- function(x) (mean(x)) - sd(x)
sd.max <- function(x) (mean(x)) + sd(x)
scaleFUN <- function(x) sprintf("%.0f", x)

#Import Data
# Bring in from ecocloud----
# dir()

dat.rr <-read_csv("dat.rr.csv")%>%
  glimpse()

dat.rr <- dat.rr%>%
  mutate(Lyrs=(Recap.Date-Date)/365, growth=Recap.cl-Initial.cl)%>%
  glimpse()


# rec %<>% mutate(Lyrs=as.numeric(as.Date(Date, '%d/%m/%Y')-as.Date(rel.date, '%d/%m/%Y'))/365,growth=Carapace.length-rlclength) %>%
#   mutate(sex=tolower(substr(Sex,1,1))) %>% filter(sex%in% c('f','m'), !is.na(sex)) %>%
#   mutate(rcyear=format(as.Date(Date, '%d/%m/%Y'),'%Y'),rlyear=format(as.Date(rel.date, '%d/%m/%Y'),'%Y')) %>%
#   mutate(rcmonth=format(as.Date(Date, '%d/%m/%Y'),'%m'),rlmonth=format(as.Date(rel.date, '%d/%m/%Y'),'%m'))
# 
# rec2 <- rec %>% dplyr::select(Date, Tag.number,Carapace.length, Sex,rlloc, Total.damage, rel.date, rlclength)
# smdat %<>% mutate(Date=as.Date(Date,'%Y-%m-%d'), rel.date=as.Date(rel.date,'%Y-%m-%d'))
# rec2 %<>% mutate(Date=as.Date(Date,'%d/%m/%Y'), rel.date=as.Date(rel.date,'%d/%m/%Y'))
# 
# rec2 <- rbind(rec2,smdat)
# 
# rec2 %<>% mutate() %>% 
#   mutate(Lyrs=as.numeric(as.Date(Date)-as.Date(rel.date))/365,growth=Carapace.length-rlclength) %>%
#   mutate(sex=tolower(substr(Sex,1,1))) %>% filter(sex%in% c('f','m'), !is.na(sex)) %>%
#   mutate(rcyear=format(as.Date(Date),'%Y'),rlyear=format(as.Date(rel.date),'%Y')) %>%
#   mutate(rcmonth=format(as.Date(Date),'%m'),rlmonth=format(as.Date(rel.date),'%m'))
# 
# head(rec2)
# 
# write.csv(rec2, 'data.w.smb.csv', row.names = F)

rec <- read.csv('C:/Users/snd/Rock Lobster/Minor stuff/Other people/Ash/data.w.smb.csv')

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


tmp <- rec[rec$Lyrs>0.2 & rec$rlloc=='Irwin Reef',]
tmp <- tmp[!is.na(tmp$Sex),]
## Sexes split
(sout.ir <- nlminb(c(120,0.5,120,0.5,1), fab, tmp=tmp, control = list(iter.max=1000, eval.max=1000)))
## Sexes combined
(bout.ir <- nlminb(c(120,0.5,1), fab, tmp=tmp, sex='comb', control = list(iter.max=1000, eval.max=1000)))

##Test whether we need two lots of parameters
LR_test = 1-pchisq(abs(bout.ir$objective-sout.ir$objective), 2)
LR_test  ## very much an improvement

# ## Try with the same K
# fab2 <- function(pin,tmp=tmp,flag='solve',sex='split'){
#   pin <- as.numeric(pin)
#   if(sex=='split'){Linf <- ifelse(tmp$sex=='f', pin[1],pin[3])
#   K <- ifelse(tmp$sex=='f', pin[2], pin[2])
#   sd=pin[4]}
#   if(sex=='comb'){Linf <- pin[1]
#   K <- pin[2]
#   sd=pin[3]}
#   est = (Linf-tmp$rlclength)*(1-exp(-K*tmp$Lyrs))
#   LL <- -sum(dnorm(tmp$growth, est, sd,T))
#   if(flag=='print') return(est)
#   if(flag=='solve') return(LL)
# }
# 
# ## Sexes 1/2 combined
# (bout <- nlminb(c(120,0.5,120,1), fab2, tmp=tmp))
# ##Test whether we need two lots of parameters
# 
# ##Test whether we need two lots of parameters
# LR_test = 1-pchisq(abs(bout$objective-sout$objective), 1)
# LR_test  ## Not significant move to the three parameter model

##residual plot

tmp$est <- fab(bout.ir$par,tmp=tmp,flag='print',sex='comb')
tmp$resid <- tmp$growth-tmp$est
par(mfrow=c(3,3),mar=c(5,5,2,2),las=1)
with(tmp,plot(rlmonth, resid, pch=16, ylab='IR Residual', xlab='Release Month', col=alpha(ifelse(sex=='f',2,4),0.2), bty='l'))
abline(h=0, lty=3)
with(tmp,plot(rlclength, resid, pch=16, ylab='IR Residual', xlab='Release Carapace Length (mm)', col=alpha(ifelse(sex=='f',2,4),0.2), bty='l'))
abline(h=0, lty=3)
with(tmp,plot(Lyrs, resid, pch=16, ylab='IR Residual', xlab='Liberty (years)', col=alpha(ifelse(sex=='f',2,4),0.2), bty='l'))
abline(h=0, lty=3)

# ## Golden Ridge
# tmp <- rec[rec$Lyrs>0.2 & rec$rlloc=='Golden Ridge',]
# tmp <- tmp[!is.na(tmp$Sex),]
# 
# (sout.gr <- nlminb(c(120,0.5,120,0.5,1), fab, tmp=tmp))
# ## Sexes combined
# (bout.gr <- nlminb(c(120,0.5,1), fab, tmp=tmp, sex='comb'))
# ##Test whether we need two lots of parameters
# LR_test = 1-pchisq(abs(bout.gr$objective-sout.gr$objective), 2)
# LR_test  ## very much an improvement
# 
# tmp$est <- fab(bout.gr$par,tmp=tmp,flag='print',sex='comb')
# tmp$resid <- tmp$growth-tmp$est
# with(tmp,plot(rlmonth, resid, pch=16, ylab='GR Residual', xlab='Release Month', col=alpha(ifelse(sex=='f',2,4),0.2), bty='l'))
# abline(h=0, lty=3)
# with(tmp,plot(rlclength, resid, pch=16, ylab='GR Residual', xlab='Release Carapace Length (mm)', col=alpha(ifelse(sex=='f',2,4),0.2), bty='l'))
# abline(h=0, lty=3)
# with(tmp,plot(Lyrs, resid, pch=16, ylab='GR Residual', xlab='Liberty (years)', col=alpha(ifelse(sex=='f',2,4),0.2), bty='l'))
# abline(h=0, lty=3)

## Cliff Head
tmp <- rec[rec$Lyrs>0.2 & rec$rlloc=='Cliff Head',]
tmp <- tmp[!is.na(tmp$Sex),]

(sout.ch <- nlminb(c(120,0.5,120,0.5,1), fab, tmp=tmp, control = list(iter.max=1000, eval.max=1000)))
## Sexes combined
(bout.ch <- nlminb(c(120,0.5,1), fab, tmp=tmp, sex='comb'))
LR_test = 1-pchisq(abs(bout.ch$objective-sout.ch$objective), 2)
LR_test  ## very much an improvement

tmp$est <- fab(bout.ch$par,tmp=tmp,flag='print',sex='comb')
tmp$resid <- tmp$growth-tmp$est
with(tmp,plot(rlmonth, resid, pch=16, ylab='CH Residual', xlab='Release Month', col=alpha(ifelse(sex=='f',2,4),0.2), bty='l'))
abline(h=0, lty=3)
with(tmp,plot(rlclength, resid, pch=16, ylab='CH Residual', xlab='Release Carapace Length (mm)', col=alpha(ifelse(sex=='f',2,4),0.2), bty='l'))
abline(h=0, lty=3)
with(tmp,plot(Lyrs, resid, pch=16, ylab='CH Residual', xlab='Liberty (years)', col=alpha(ifelse(sex=='f',2,4),0.2), bty='l'))
abline(h=0, lty=3)

## Seven Mile Beach
tmp <- rec[rec$Lyrs>0.2 & rec$Lyrs<1.3 & rec$rlloc=='smb',]
tmp <- tmp[!is.na(tmp$sex),]

(sout.sm <- nlminb(c(120,0.5,120,0.5,1), fab, tmp=tmp, control = list(iter.max=1000, eval.max=1000)))
## Sexes combined
(bout.sm <- nlminb(c(120,0.5,1), fab, tmp=tmp, sex='comb'))
LR_test = 1-pchisq(abs(bout.sm$objective-sout.sm$objective), 2)
LR_test  ## very much an improvement

tmp$est <- fab(bout.sm$par,tmp=tmp,flag='print',sex='comb')
tmp$resid <- tmp$growth-tmp$est
with(tmp,plot(rlmonth, resid, pch=16, ylab='SM Residual', xlab='Release Month', col=alpha(ifelse(sex=='f',2,4),0.2), bty='l'))
abline(h=0, lty=3)
with(tmp,plot(rlclength, resid, pch=16, ylab='SM Residual', xlab='Release Carapace Length (mm)', col=alpha(ifelse(sex=='f',2,4),0.2), bty='l'))
abline(h=0, lty=3)
with(tmp,plot(Lyrs, resid, pch=16, ylab='SM Residual', xlab='Liberty (years)', col=alpha(ifelse(sex=='f',2,4),0.2), bty='l'))
abline(h=0, lty=3)


dum <- data.frame(age=2:10)
dum$ir<- bout.ir$par[1]*(1-exp(-bout.ir$par[2]*dum$age))
#dum$gr<- bout.gr$par[1]*(1-exp(-bout.gr$par[2]*dum$age))
dum$ch<- bout.ch$par[1]*(1-exp(-bout.ch$par[2]*dum$age))
dum$sm<- bout.sm$par[1]*(1-exp(-bout.sm$par[2]*dum$age))

par(mfrow=c(1,1))
plot(dum$age, dum$ir, type='l', col='red', xlab='Relative age (years)', ylab='Carapace length (mm)', ylim=c(20,110), bty='l')
#lines(dum$age, dum$gr, type='l', col='blue')
lines(dum$age, dum$ch, type='l', col=3)
lines(dum$age, dum$sm, type='l', col=6)

### Now test locations
fab2 <- function(pin,tmp=tmp,flag='solve',loc='split',loc1){
  pin <- as.numeric(pin)
  if(loc=='split'){Linf <- ifelse(tmp$rlloc==loc1, pin[1],pin[3])
  K <- ifelse(tmp$rlloc==loc1, pin[2],pin[4])
  sd=pin[5]}
  if(loc=='comb'){Linf <- pin[1]
  K <- pin[2]
  sd=pin[3]}
  est = (Linf-tmp$rlclength)*(1-exp(-K*tmp$Lyrs))
  LL <- -sum(dnorm(tmp$growth, est, sd,T))
  if(flag=='print') return(est)
  if(flag=='solve') return(LL)
}
pin <- c(120,0.5,125,0.55,1)

## Locations split
tmp <- rec[rec$Lyrs>0.2 & rec$rlloc%in%c('Cliff Head',"Irwin Reef"),]
tmp <- tmp[!is.na(tmp$Sex),]
(sout.ch_ir <- nlminb(c(120,0.5,120,0.5,1), fab2, tmp=tmp, loc1='Cliff Head'))

## Locations combined
(bout.ch_ir <- nlminb(c(120,0.5,1), fab2, tmp=tmp, loc='comb', loc1='Cliff Head'))
##Test whether we need two lots of parameters
LR_test = 1-pchisq(abs(bout.ch_ir$objective-sout.ch_ir$objective), 2)
LR_test  ## very much an improvement

## Locations split
tmp <- rec[rec$Lyrs>0.2 & rec$rlloc%in%c('Cliff Head',"smb"),]
tmp <- tmp[!is.na(tmp$Sex),]
(sout.ch_sm <- nlminb(c(120,0.5,120,0.5,1), fab2, tmp=tmp, loc1='Cliff Head'))

## Locations combined
(bout.ch_sm <- nlminb(c(120,0.5,1), fab2, tmp=tmp, loc='comb', loc1='Cliff Head'))
##Test whether we need two lots of parameters
LR_test = 1-pchisq(abs(bout.ch_sm$objective-sout.ch_sm$objective), 2)
LR_test  ## very much an improvement

## Locations split
tmp <- rec[rec$Lyrs>0.2 & rec$rlloc%in%c('Irwin Reef',"smb"),]
tmp <- tmp[!is.na(tmp$Sex),]
(sout.ir_sm <- nlminb(c(120,0.5,120,0.5,1), fab2, tmp=tmp, loc1='Irwin Reef'))

## Locations combined
(bout.ir_sm <- nlminb(c(120,0.5,1), fab2, tmp=tmp, loc='comb', loc1='Irwin Reef'))
##Test whether we need two lots of parameters
LR_test = 1-pchisq(abs(bout.ir_sm$objective-sout.ir_sm$objective), 2)
LR_test  ## very much an improvement

tmp <- rec[rec$Lyrs>0.2 & rec$rlloc%in%c('Irwin Reef'),]
## Bootsstrap to get better error estimates
bstrap <- 1000
ages <- seq(2,10,0.1)
bsout <- matrix(NA,ncol=3,nrow=bstrap)
bsout2 <- array(NA,dim=c(bstrap, length(ages),1))
for (i in 1:bstrap){
  tmp2 <- tmp[sample(1:nrow(tmp),nrow(tmp),T),]
  sout <- nlminb(c(120,0.5,1), fab, tmp=tmp2, sex='comb')
  bsout[i,]<- sout$par
  bsout2[i,,1] <- sout$par[1]*(1-exp(-sout$par[2]*ages))
}

ir.par_out <- apply(bsout, 2, quantile,probs=c(0.125,0.5,0.875))
ir.est_out <- apply(bsout2, c(2,3), quantile,probs=c(0.125,0.5,0.875))


tmp <- rec[rec$Lyrs>0.2 & rec$rlloc%in%c('Cliff Head'),]
## Bootsstrap to get better error estimates
bstrap <- 1000
ages <- seq(2,10,0.1)
bsout <- matrix(NA,ncol=3,nrow=bstrap)
bsout2 <- array(NA,dim=c(bstrap, length(ages),1))
for (i in 1:bstrap){
  tmp2 <- tmp[sample(1:nrow(tmp),nrow(tmp),T),]
  sout <- nlminb(c(120,0.5,1), fab, tmp=tmp2, sex='comb')
  bsout[i,]<- sout$par
  bsout2[i,,1] <- sout$par[1]*(1-exp(-sout$par[2]*ages))
}

ch.par_out <- apply(bsout, 2, quantile,probs=c(0.125,0.5,0.875))
ch.est_out <- apply(bsout2, c(2,3), quantile,probs=c(0.125,0.5,0.875))

tmp <- rec[rec$Lyrs>0.2 & rec$rlloc%in%c('smb'),]
## Bootsstrap to get better error estimates
bstrap <- 1000
ages <- seq(2,10,0.1)
bsout <- matrix(NA,ncol=3,nrow=bstrap)
bsout2 <- array(NA,dim=c(bstrap, length(ages),1))
for (i in 1:bstrap){
  tmp2 <- tmp[sample(1:nrow(tmp),nrow(tmp),T),]
  sout <- nlminb(c(120,0.5,1), fab, tmp=tmp2, sex='comb')
  bsout[i,]<- sout$par
  bsout2[i,,1] <- sout$par[1]*(1-exp(-sout$par[2]*ages))
}

sm.par_out <- apply(bsout, 2, quantile,probs=c(0.125,0.5,0.875))
sm.est_out <- apply(bsout2, c(2,3), quantile,probs=c(0.125,0.5,0.875))

par(las=1, mfrow=c(1,1))
plot(ages, ch.est_out['50%',,], col=4, lwd=2, xlab='Approximate age (years)',ylab='Carapace length (mm)',axes=F, type='l', ylim=c(0, 120), xlim=c(0,10))
axis(1)
axis(2)
#polygon(c(ages,rev(ages)), c(ch.est_out[1,,],rev(ch.est_out[3,,])),col=rgb(0,0,1,0.3),border=F)
arrows(ages,ch.est_out[1,,],y1=ch.est_out[3,,],code=3,angle=90,length=0.02, col=rgb(0,0,1,0.3))

lines(ages, sm.est_out['50%',,1], col=2, lwd=2)
#polygon(c(ages,rev(ages)), c(sm.est_out[1,,1],rev(sm.est_out[3,,1])),col=rgb(1,0,0,0.3),border=F)
arrows(ages,sm.est_out[1,,],y1=sm.est_out[3,,],code=3,angle=90,length=0.02, col=rgb(1,0,0,0.3))

lines(ages, ir.est_out['50%',,1], col=3, lwd=2)
#polygon(c(ages,rev(ages)), c(ir.est_out[1,,1],rev(ir.est_out[3,,1])),col=rgb(0,1,0,0.3),border=F)
arrows(ages,ir.est_out[1,,],y1=ir.est_out[3,,],code=3,angle=90,length=0.02, col=rgb(0,1,0,0.3))


