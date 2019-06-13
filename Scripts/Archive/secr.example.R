#Clear history
rm(list=ls())

library(secr)

#example spatially explicit capture recapture analysis using SECR
setwd(system.file('extdata', package='secr'))
myCH <- read.capthist('capt.txt', 'trap.txt', fmt='XY')%>% #Import data, using no-default format
  glimpse() 

#Next fit two simple models and compare them with AIC
#Set trace=FALSE to reduce the volume of output, but the default trace=TRUE is usually better
secr0 <-secr.fit(myCH, model = g0~1, trace= FALSE) #Null model
secrb <- secr.fit(myCH, model=g0~b, trace=FALSE) #Trap response model

AIC(secr0, secrb) #compare
#A model with learned trap response (g0~b) showed no improvement in fit over a null model (g0~1).
#Thus, the estimates of denisty from the two modles were also very close, and we rely on the null model for estimation.
# before displaying estimates, need to check that the likelihood is stable as we vary the mask buffer width (rows) and spacing (columns)

mask.check(secr0)
#Seems it would be better to use a buffer slightly wider than the default (100m), so we repeat the fit and display the results:

secr.fit(myCH, model= g0~1, buffer = 150, trace=FALSE)

secr.fit(capthist = myCH, model=g0~1, buffer=150, trace=FALSE)
#The density estimate is 5.475ha-1 (95% confidence interval 4.35-6.90ha-1)
#Can compare these estimates to those from the initial fit with a narrower buffer; estimated density differs only in the third decimal place:
predict(secr0)

#sSave file

# Write data
setwd("~/workspace/Analysis_Miller_WRL/Scripts")

