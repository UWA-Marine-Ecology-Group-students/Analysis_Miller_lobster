# Explore catch data----
rm(list=ls()) #clears memory

# librarys----
library(tidyr)
library(dplyr)
library(googlesheets)
library(stringr)
library(ggplot2)
library(lubridate)
library(lme4)
library(lmerTest)

# Study name----
study<-"glm"

# Set work directory----

work.dir=("~/GitHub/Analysis_Miller_WRL") #for Tim's github
work.dir=("~/workspace/Analysis_Miller_WRL") #for ecocloud server

# Set sub-directories----

data.dir=paste(work.dir,"Data",sep="/")
plot.dir=paste(work.dir,"Plots",sep="/")

#Bring in data----
Rule.4 <-read_csv("Rule.4.csv")%>%
glimpse()


#Glm----
library(lme4)
help('glm')

mod.a<-glm(inc~Location+ Tag.number + site/Location, data=Rule.4)
summary(mod)


mod<-glmer(inc~Location+ (1|Tag.number)+(1|site/Location), data=Rule.4)
summary(mod)

#Two random factors, Site nested within location=  +(1|site/location), 
mod.r<-lmer(inc~Location+ (1|Tag.number)+(1|site/Location), data=Rule.4)
summary(mod.r)

mod.a<-glmer(inc~Location+ (1|Tag.number)+(1|site/Location), data=Rule.4)
summary(mod.a)
