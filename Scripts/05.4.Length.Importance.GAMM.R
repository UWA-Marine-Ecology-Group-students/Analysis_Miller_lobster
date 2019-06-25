
# Part 2 - custom plot of importance scores----
rm(list =ls())
# Bring in and format the raw data----
#Librarys----
library(readr)
library(gridExtra)
library(ggplot2)
library(tidyr)
library(dplyr)

# Set work directory----

# work.dir=("~/workspace/Analysis_Miller_WRL") #for ecocloud server
work.dir=("Z://Analysis_Miller_lobster") #for laptop

## Sub directories ----
data.dir<-paste(work.dir,"Data",sep="/")
map.dir<-paste(work.dir,"Map Layers",sep="/")
plots.dir<-paste(work.dir,"Plots",sep="/")
model.dir<-paste(work.dir,"Model_out_length",sep="/")

# Bring in and format the data----
name<-"length"

setwd(model.dir)

dat.taxa <-read.csv("length_all.var.imp.200619.csv")%>% #from local copy
  rename(resp.var=X)%>%
  gather(key=predictor,value=importance,2:ncol(.))%>%
  filter(!resp.var=="All")%>%
  glimpse()

# Plotting defaults----
library(ggplot2)

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
  # mutate(label=ifelse(predictor=="sst"&resp.var=="White","X",ifelse(predictor=="T1.s.sw"&resp.var=="White","X",ifelse(predictor=="Location"&resp.var=="White","X",label))))%>%
  mutate(label=ifelse(predictor=="sst"&resp.var=="Red","X",ifelse
                      (predictor=="Location"&resp.var=="Red","X",ifelse
                        (predictor=="T1.s.sw"&resp.var=="Red","X",label))))%>%
  mutate(label=ifelse(predictor=="Location"&resp.var=="White","X",ifelse
                      (predictor=="sst"&resp.var=="White","X", ifelse
                        (predictor=="T1.s.sw"&resp.var=="White","X",label))))%>%
  
  

  glimpse()

# Plot gg.importance.scores ----
gg.importance.scores <- ggplot(dat.taxa.label, aes(x=predictor,y=resp.var,fill=importance))+
  geom_tile(show.legend=T) +
  scale_fill_gradientn(legend_title,colours=c("white", re), na.value = "grey98",limits = c(0.00, max(dat.taxa.label$importance)))+
  scale_x_discrete(limits=c("Location","Hs.m.sw","T1.s.sw","sst"),
                   labels=c("Location","Swell height","Swell period","SST"
                   ))+
  scale_y_discrete(limits = c("Red", "White"),
  labels=c("Red", "White"))+
  xlab(NULL)+
  ylab(NULL)+
  theme_classic()+
  Theme1+
  geom_text(aes(label=label))
gg.importance.scores



# Save plots----
setwd("Z:/Analysis_Miller_lobster/Plots")
# setwd("C:/GitHub/Analysis_Miller_lobster/Plots")

ggsave(gg.importance.scores,file="length.location.importance.190619.png", width = 20, height = 6,units = "cm")


