# Explore catch data----
rm(list=ls()) # Clears memory

# Study name----
study<-"Fabens.Simon"

library(scales)

#For Simon
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# getwd()

# Set work directory----

work.dir=("~/GitHub/Analysis_Miller_WRL") #for Tim's github
work.dir=("~/workspace/Analysis_Miller_WRL") #for ecocloud server

## Sub directories ----
data.dir<-paste(work.dir,"Data",sep="/")
map.dir<-paste(work.dir,"Map Layers",sep="/")
plots.dir<-paste(work.dir,"Plots",sep="/")


##Open up link to IBSS and down load latest
# library(TagLoss)
# 
# dat <- read.csv('data.csv')
# dat %<>% filter(!is.na(Tag.number), !is.na(Carapace.length), !is.na(Sex)) %>%
#   mutate(Site=str_replace_all(.$Site,c("Seven Mile Beach"= "Seven Mile","Little Horseshoe"="Horse", "Cliff Head North"="Cliff Head","Cliff Head Mid"= "Cliff Head","Cliff Head South"="Cliff Head","Cliff Head OUT1"= "Horse","CHM"="Cliff Head", "Davids Marks"="Horse","CHM"= "Cliff Head", "CHS"="Cliff Head", "CHN"="Cliff Head", "Jim Bailey"="Irwin Reef", "Rivermouth"="River", "Long Reef"="Irwin Reef", "South Dummy"="Irwin Reef","South Rig"= "White","Whites Lump"= "White","WP"= "White","Whitepoint"="White")))
# 
# rec <- dat %>% filter(!is.na(Recapture))
# rec$rel.date <- dat$Date[match(rec$Tag.number, dat$Tag.number)]
# rec$rlclength <- dat$Carapace.length[match(rec$Tag.number, dat$Tag.number)]
# rec$rlloc <- dat$Site[match(rec$Tag.number, dat$Tag.number)]
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

#For Simon
# rec <- read.csv('C:/Users/snd/Rock Lobster/Minor stuff/Other people/Ash/data.w.smb.csv')

#For Ash

rec<- gs_title("data.w.smb")%>%
  gs_read_csv(ws="data.w.smb")%>%
  glimpse()

# rec1<-read_csv("dat.rr.all.csv")%>%
#   glimpse()
# 
# rec1 <- rec1%>%
#   dplyr::rename(rel.date = Date, Date= recap.Date, rlloc= Location.int, Sex=Sex.int, Carapace.length= recap.cl, Total.damage=Total.damage.int, Colour= Colour.int, rlclength=initial.cl, site=mini.site.x)%>%
#   select(Date, Tag.number, Carapace.length, Sex, rlloc, Total.damage, rel.date, rlclength)%>%
#   glimpse()
# 
# rec1<-rec1%>%
#   #mutate(Lyrs=(rel.date-Date)/365, growth=rlclength-Carapace.length)%>%
#   mutate(Lyrs=as.numeric(as.Date(Date, '%d/%m/%Y')-as.Date(rel.date, '%d/%m/%Y'))/365,growth=Carapace.lengthrlclength)%>%
#   mutate(sex=tolower(substr(Sex,1,1))) %>% filter(sex%in% c('f','m'), !is.na(sex)) %>%
#   mutate(rcyear=format(as.Date(Date, '%d/%m/%Y'),'%Y'),rlyear=format(as.Date(rel.date, '%d/%m/%Y'),'%Y')) %>%
#   mutate(rcmonth=format(as.Date(Date, '%d/%m/%Y'),'%m'),rlmonth=format(as.Date(rel.date, '%d/%m/%Y'),'%m'))%>%
#   glimpse()
# 
# 
# rec1<-rec1%>%
#   dplyr::mutate(rlmonth = as.integer(rlmonth))%>%
#   dplyr::mutate(rcmonth = as.integer(rcmonth))%>%
#   dplyr::mutate(rlyear = as.integer(rlyear))%>%
#   dplyr::mutate(rcyear = as.integer(rcyear))%>%
#   glimpse()
# 
# rec<-rec1
# 

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

#subset to Irwin Reef

tmp <- rec[rec$Lyrs>0.2 & rec$rlloc=='Irwin Reef',]
tmp <- tmp[!is.na(tmp$Sex),]


#Conduct a nlminb for Irwin Reef
#nlminb= Nonlinear Minimization subject to Box Constraints 
#(local minimizer for smooth nonlinear functions to bound-contrained parameters)

## Sexes split
(sout.ir <- nlminb(c(120,0.5,120,0.5,1), fab, tmp=tmp, control = list(iter.max=1000, eval.max=1000)))
#Convergence= 0 : A integer code of 0 indicates a successful convergence

## Sexes combined
(bout.ir <- nlminb(c(120,0.5,1), fab, tmp=tmp, sex='comb', control = list(iter.max=1000, eval.max=1000)))


##Test whether we need two lots of parameters
LR_test = 1-pchisq(abs(bout.ir$objective-sout.ir$objective), 2) # calculates significance with 2 d.o.f, 

#pchisq() :chi-squared distribution function, gives the distribution function

LR_test  ## = p-value very much an improvement
#IR P-value=0.5217802 : not significant

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
# # # 
# # ## Sexes 1/2 combined
# (bout <- nlminb(c(120,0.5,120,1), fab2, tmp=tmp))
# # ##Test whether we need two lots of parameters
# # 
# # ##Test whether we need two lots of parameters
# LR_test = 1-pchisq(abs(bout$objective-bout$objective), 1)
# LR_test  ## Not significant move to the three parameter model

#Parameters change if you split (sex) or non-split (sex)
#Need 5 parameters for split, and 3 for non-split!
#Preferable to only use 3 parameters! i.e. prefer to not have to split sex!

##residual plot
tmp <- data.frame(tmp)

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
unique(rec$rlloc)
tmp <- rec[rec$Lyrs>0.2 & rec$Lyrs<1.3 & rec$rlloc=='Seven Mile',]
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

#Dummy Variable----
dum <- data.frame(age=2:30)
dum$ir<- bout.ir$par[1]*(1-exp(-bout.ir$par[2]*dum$age))
#dum$gr<- bout.gr$par[1]*(1-exp(-bout.gr$par[2]*dum$age))
dum$ch<- bout.ch$par[1]*(1-exp(-bout.ch$par[2]*dum$age))
dum$sm<- bout.sm$par[1]*(1-exp(-bout.sm$par[2]*dum$age))
glimpse(bout.sm)

par(mfrow=c(1,1))
plot(dum$age, dum$ir, type='l', col='red', xlab='Relative age (years)', ylab='Carapace length (mm)', ylim=c(20,110), bty='l')

#lines(dum$age, dum$gr, type='l', col='blue')
lines(dum$age, dum$ch, type='l', col=3)
lines(dum$age, dum$sm, type='l', col=6)
legend("bottomright", legend=c("IR", "CH", "SM"), col = c("red", "3", "6"), lty=1, box.lty=0)

glimpse(dum$sm)
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
warnings()
View(sm.est_out)
View(ch.est_out)

lines(ages, ir.est_out['50%',,1], col=3, lwd=2)
#polygon(c(ages,rev(ages)), c(ir.est_out[1,,1],rev(ir.est_out[3,,1])),col=rgb(0,1,0,0.3),border=F)
arrows(ages,ir.est_out[1,,],y1=ir.est_out[3,,],code=3,angle=90,length=0.02, col=rgb(0,1,0,0.3))
legend("bottomright", legend=c("CH", "SM", "IR"), col = c("4", "2", "3"), lty=1, box.lty=0)
warnings()
