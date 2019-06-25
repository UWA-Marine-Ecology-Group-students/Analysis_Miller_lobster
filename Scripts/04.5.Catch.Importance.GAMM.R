rm(list=ls()) # Clears memory
# Part 2 - custom plot of importance scores----
#Librarys-----
library(readr)
library(gridExtra)
library(ggplot2)
library(tidyr)
library(dplyr)

# Bring in and format the raw data----
# Set work directory----

#work.dir=("~/workspace/Analysis_Miller_WRL") #for ecocloud server
work.dir=("Z://Analysis_Miller_lobster") #for laptop

## Sub directories ----
data.dir<-paste(work.dir,"Data",sep="/")
map.dir<-paste(work.dir,"Map Layers",sep="/")
plots.dir<-paste(work.dir,"Plots",sep="/")
model.dir<-paste(work.dir,"Model_out_catch",sep="/")

# Bring in and format the data----
name<-"catch"
setwd(model.dir)

dat.taxa <- read.csv("catch_all.var.imp.190619.csv")%>% #from local copy
  rename(resp.var=X)%>%
  gather(key=predictor,value=importance,2:ncol(.))%>%
  filter(!resp.var=="All")%>%
  glimpse()


# Plotting defaults----

# Theme-
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill="white"),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=10),
    legend.title = element_text(size=10, face="bold"),
    legend.position = "top",
    legend.direction="horizontal",
    text=element_text(size=10),
    strip.text.y = element_text(size = 10,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=10),
    axis.title.y=element_text(vjust=0.6, angle=90, size=10),
    axis.text.x=element_text(size=12,angle = 0,face = "bold", colour = "black"), #, hjust=1,vjust=0.5
    axis.text.y=element_text(size=12,face="bold", colour = "black"),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())


# colour ramps-
re <- colorRampPalette(c("mistyrose", "red2","darkred"))(200)

# Labels-
legend_title<-"Importance"

# Annotations-
unique(dat.taxa$predictor)
unique(dat.taxa$resp.var)


dat.taxa.label<-dat.taxa%>%
  mutate(label=NA)%>%
  mutate(label=ifelse(predictor=="Hs.m.sw"&resp.var=="Sublegal","X",
                      ifelse(predictor=="sst"&resp.var=="Sublegal","X",
                             ifelse(predictor=="Location"&resp.var=="Sublegal","X",label))))%>%
  mutate(label=ifelse(predictor=="sst" & resp.var=="Legal","X",
                      ifelse(predictor=="T1.s.sw"&resp.var=="Legal","X",
                      ifelse(predictor=="Hs.m.sw"&resp.var=="Legal","X"
                             ,label))))%>%
  
  
  
  glimpse()

# Plot gg.importance.scores ----
gg.importance.scores <- ggplot(dat.taxa.label, aes(x=predictor,y=resp.var,fill=importance))+
  geom_tile(show.legend=T) +
  scale_fill_gradientn(legend_title,colours=c("white", re), na.value = "grey98", limits = c(0.01, max(dat.taxa.label$importance)))+
  scale_x_discrete(limits=c("Location","Hs.m.sw","T1.s.sw","sst"),
                   labels=c("Location","Swell height","Swell period","SST"
                   ))+
  scale_y_discrete(limits = c("Legal", "Sublegal"),
  labels=c("Legal", "Sublegal"))+
  xlab(NULL)+
  ylab(NULL)+
  theme_classic()+
  Theme1+
  geom_text(aes(label=label))
gg.importance.scores

# Save plots----
setwd(plots.dir)

# ggsave(gg.importance.scores,file="catch.location.importance.new.png", width = 30, height = 10,units = "cm")

ggsave(gg.importance.scores,file="catch.location.importance.190619.png", width = 20, height = 6,units = "cm")
