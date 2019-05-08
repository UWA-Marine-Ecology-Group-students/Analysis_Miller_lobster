# Explore catch data----
rm(list=ls()) # Clears memory

# librarys----
library(tidyr)
library(dplyr)
library(googlesheets)
library(stringr)
library(lubridate)
library(tidyverse)



# Study name----
study<-"Lobster.Data"

# Set work directory----

#For Tims Github 

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
get.sst<-function(data){
  #function to retrieve sst given a dataframe with Latitude, Longitude, data and time
  #provide a data frame with a column called Lat and a column called Long
  #should be a column called Date which should be a column called Date which is in data format: "2018-07-14" (use as.Date)
  
  require(rerddap)
  require(ncdf4)
  require(data.table)
  require(dplyr)
  require(sp)
  require(rgeos)
  require(geosphere)
  require(magrittr)
  library(lubridate)
  
  #Checks and fixes
  data$Lat<-ifelse(data$Lat>0, data$Lat*-1, data$Lat) #convert to negative lats if not already
  if(!("Lat" %in% colnames(data))){print("Need a column called Lat in decimal degrees")}
  if(!("Long" %in% colnames(data))){print("Need a column called Long in decimal degrees")}
  if(!("Date" %in% colnames(data))){print("Need a column called Date in date fromat using as.Date")}
  if(is.Date(data$Date)==F) {print("Convert your dates using as.Date")}
  
  
  rerddap::info(datasetid = "ncdc_oisst_v2_avhrr_by_time_zlev_lat_lon", url = "https://www.ncei.noaa.gov/erddap/")
  
  OISST_sub <- function(times){ #function to download sst data from noaa
    oisst_res <- griddap(x = "ncdc_oisst_v2_avhrr_by_time_zlev_lat_lon", 
                         url = "https://www.ncei.noaa.gov/erddap/", 
                         time = times, 
                         depth = c(0, 0),
                         latitude = c(min(data$Lat)-0.25, max(data$Lat)+0.25),
                         longitude = c(min(data$Long)-0.25, max(data$Long)+0.25),
                         fields = "sst")
  }
  
  OISST_prep <- function(nc_file){ #function for converting formats
    
    # Open the NetCDF connection
    nc <- nc_open(nc_file$summary$filename)
    
    # Extract the SST values and add the lon/lat/time dimension names
    res <- ncvar_get(nc, varid = "sst")
    dimnames(res) <- list(lon = nc$dim$longitude$vals,
                          lat = nc$dim$latitude$vals,
                          t = nc$dim$time$vals)
    
    # Convert the data into a 'long' dataframe for use in the 'tidyverse' ecosystem
    res <- as.data.frame(reshape2::melt(res, value.name = "temp"), row.names = NULL) %>% 
      mutate(t = as.Date(as.POSIXct(t, origin = "1970-01-01 00:00:00")),
             temp = round(temp, 2))
    
    # Close the NetCDF connection and finish
    nc_close(nc)
    return(res)
  }
  
  #put 
  
  data$years<-format(data$Date,"%Y")
  years<-unique(data$years)
  
  for (i in 1:length(years)){ #loop to get start and end dates
    
    start<-paste0(years[i], "-01-01T00:00:00Z")
    end<-paste0(years[i], "-12-31T00:00:00Z")
    temp <- OISST_sub(c(start, end)) #download data
    temp_prepped <- OISST_prep(temp) #prep data
    
    if(i==1){OISST_all<-temp_prepped}else{ 
      OISST_all <- rbind(OISST_all, temp_prepped)}#bind data
    
  } #this downloads the data for interesting years and geographic range and binds it all together
  
  #retrieve unique spatial points for each data set
  sst_points <- unique(OISST_all[,c("lon", "lat")])
  
  data_points<- unique(data[ ,c("Long", "Lat", "SiteNo")])
  
  #calculate pairwise distances
  d <- distm(data_points[ ,c("Long", "Lat")], sst_points[ ,c("lon", "lat")], fun=distGeo)
  row.names(d)<-data_points$SiteNo 
  
  #attach lats and longs of four closest sst locations
  first<-sst_points[apply(d, 1, function(x) order(x, decreasing=F)[1]),] ; colnames(first)<-paste0(colnames(first),".1" )
  second<-sst_points[apply(d, 1, function(x) order(x, decreasing=F)[2]),] ; colnames(second)<-paste0(colnames(second),".2" )
  third<-sst_points[apply(d, 1, function(x) order(x, decreasing=F)[3]),] ; colnames(third)<-paste0(colnames(third),".3" )
  fourth<-sst_points[apply(d, 1, function(x) order(x, decreasing=F)[4]),] ; colnames(fourth)<-paste0(colnames(fourth),".4" )
  
  
  data_points<-cbind(data_points, first, second, third, fourth)
  
  #merge in lats and longs for sst into main dataset
  data$SiteNo<-as.character(data$SiteNo) ; data_points$SiteNo<-as.character(data_points$SiteNo)
  data %<>% left_join(.,data_points[, 3:ncol(data_points)], c("SiteNo" = "SiteNo"))
  
  data$ID<-paste0(data$lon.1,data$lat.1,data$Date)
  data$ID2<-paste0(data$lon.2,data$lat.2,data$Date)
  data$ID3<-paste0(data$lon.3,data$lat.3,data$Date)
  data$ID4<-paste0(data$lon.4,data$lat.4,data$Date)
  
  OISST_all$ID<-paste0(OISST_all$lon,OISST_all$lat,OISST_all$t)
  
  data$sst<-OISST_all$temp[match(data$ID, OISST_all$ID)] #check first
  data$sst<-ifelse(is.na(data$sst),OISST_all$temp[match(data$ID2, OISST_all$ID)], data$sst ) #check second
  data$sst<-ifelse(is.na(data$sst),OISST_all$temp[match(data$ID3, OISST_all$ID)], data$sst ) #check third
  data$sst<-ifelse(is.na(data$sst),OISST_all$temp[match(data$ID4, OISST_all$ID)], data$sst ) #check fourth
  if(anyNA(data$sst)){print("Email matt and say: There are a lot of sst NAs. You need to increase the number of nearest points to search")}
  return(data$sst)
} #Matts Function for sst data
# Import k-mean cluster data----

k.dat <- read_csv("kcluster.csv")%>%
  mutate(Date=as.Date(Date))%>%
  mutate(recap.Date=as.Date(recap.Date))%>%
  glimpse()

# Edit for loop----

lobsters<-k.dat%>%
  select(Date,Tag.number,recap.Date)%>% # filtered to make simpler
  mutate(Date=as.Date(Date))%>%
  mutate(recap.Date=as.Date(recap.Date))%>%
  mutate(obs=1:nrow(.))%>% # create a row with observations to use in loop
  glimpse()


# Import Swell data----

swell <- read_csv("dat.swell.csv")%>%
  mutate(Date=as.Date(Date))%>%
  glimpse()

# Loop to match with swell data: Thanks Brooke !!!----
# Create loop

uniq <- unique(unlist(lobsters$obs)) # create a list with obs

lobster.swell<- data.frame() # create a blank data frame to add in summarised data


for (i in 1:length(uniq)){
  
  temp.lob<-subset(lobsters, obs==uniq[i]) # filter lobster data to observation
  release.date<-as.Date(temp.lob$Date) # create a value for release date
  recapture.date<-as.Date(temp.lob$recap.Date) # create a value for recapture date
  
  temp.swell<-swell%>%
    filter(Date>=release.date&Date<=recapture.date)%>% # filter swell data to release and recapture dates
    mutate(date=as.Date(Date))%>%
    
    summarise(Date=first(date), # create release date
              recap.Date=last(date), # create recapture date
              min.Hs.m.sw=min(Hs.m.sw),
              max.Hs.m.sw=max(Hs.m.sw),
              avg.Hs.m.sw=mean(Hs.m.sw),
              min.Hs.m.sea=min(Hs.m.sea),
              max.Hs.m.sea=max(Hs.m.sea),
              avg.Hs.m.sea=mean(Hs.m.sea),
              min.T1.s.sw=min(T1.s.sw),
              max.T1.s.sw=max(T1.s.sw),
              avg.T1.s.sw=mean(T1.s.sw),
              min.T1.s.sea=min(T1.s.sea),
              max.T1.s.sea=max(T1.s.sea),
              avg.T1.s.sea=mean(T1.s.sea))
  
  lobster.swell <- rbind(lobster.swell,temp.swell)%>% # add data into blank dataframe
    
    distinct()
  
}

#Combine Growth and Swell data----

growth.swell<-left_join(k.dat,lobster.swell)%>% # add the growth data back in
  glimpse()

setwd(data.dir)
write.csv(growth.swell, "growth.swell.csv", row.names = F)

#FIGURE OUT SST----

#Find Averages GPS per Location so can find average sst per block
glimpse(k.dat)

av.lat.lon <- k.dat%>%
  select(Location.int, long.int, lat.int)%>%
  glimpse()

test<-av.lat.lon%>%
  group_by(Location.int)%>%
  # summarise(minlon= min(long.int), maxlon= max(long.int),minlat=min(lat.int), maxlat=max(lat.int))%>%
  summarise(meanlon= mean(long.int), meanlat= mean(lat.int) )%>%
  glimpse()
  
test1<-av.lat.lon%>%
  summarise(minlon= min(long.int), maxlon= max(long.int),minlat=min(lat.int), maxlat=max(lat.int))%>%
  glimpse()


#BRING IN AVERAGE SITENO (lat and long) FOR EACH LOCATION----

#SEVENMILE-----
sm.sst<-gs_title("SiteNo")%>% # To use GoogleSheets
  gs_read_csv(ws = "SevenMile" )%>%
  mutate(Date=as.Date(Date,format= "%m/%d/%Y"))%>%
  glimpse()

sm.sst%<>%
  unite(SiteNo, c(Lat,Long), sep = "", remove = F)%>% 
  glimpse()

#Use Matts SST function----
sm.sst$sst<-get.sst(sm.sst)
glimpse(sm.sst)
sm.sst%<>%
  dplyr::rename("sm.sst"="sst")%>%
  glimpse()

setwd(data.dir)
write.csv(sm.sst,"sm.sst.csv", row.names=F)

#Cliff Head----
ch.sst<-gs_title("SiteNo")%>% # To use GoogleSheets
  gs_read_csv(ws = "CliffHead" )%>%
  mutate(Date=as.Date(Date,format= "%m/%d/%Y"))%>%
  glimpse()

ch.sst%<>%
  unite(SiteNo, c(Lat,Long), sep = "", remove = F)%>% 
  glimpse()

#Use Matts SST function----
ch.sst$sst<-get.sst(ch.sst)
glimpse(ch.sst)
ch.sst%<>%
  dplyr::rename("ch.sst"="sst")%>%
  glimpse()

setwd(data.dir)
write.csv(ch.sst,"ch.sst.csv", row.names=F)

#Irwin Reef----
ir.sst<-gs_title("SiteNo")%>% # To use GoogleSheets
  gs_read_csv(ws = "IrwinReef" )%>%
  mutate(Date=as.Date(Date,format= "%m/%d/%Y"))%>%
  glimpse()

ir.sst%<>%
  unite(SiteNo, c(Lat,Long), sep = "", remove = F)%>% 
  glimpse()

#Use Matts SST function----
ir.sst$sst<-get.sst(ir.sst)
glimpse(ir.sst)
ir.sst%<>%
  dplyr::rename("ir.sst"="sst")%>%
  glimpse()

setwd(data.dir)
write.csv(ir.sst,"ir.sst.csv", row.names=F)

#GoldenRidge----
gr.sst<-gs_title("SiteNo")%>% # To use GoogleSheets
  gs_read_csv(ws = "GoldenRidge" )%>%
  mutate(Date=as.Date(Date,format= "%m/%d/%Y"))%>%
  glimpse()

gr.sst%<>%
  unite(SiteNo, c(Lat,Long), sep = "", remove = F)%>% 
  glimpse()

#Use Matts SST function----
gr.sst$sst<-get.sst(gr.sst)
glimpse(gr.sst)
gr.sst%<>%
  dplyr::rename("gr.sst"="sst")%>%
  glimpse()

setwd(data.dir)
write.csv(gr.sst,"gr.sst.csv", row.names=F)
#COMBINE SST FOR ALL LOCATIONS----

#HMMHMHMHMHMHMHMHMHMHMmmmmmmmmmmmMMMMMMMMM




# #Bring in SST data (Thanks Matt)----
# 
# dat.sst<-gs_title("dat.all.sst")%>% # To use GoogleSheets
#   gs_read_csv(ws = "Sheet1", row.names=FALSE)%>%
#   select(Date, Location, Site, sst, SiteNo)%>%
#   distinct()%>%
#   glimpse()
# 
# #Easiest way to join sst data & k-means kluster data is by either:
# #1. lat and long (SiteNo) will need two columns: 1 for initial capture location and one for recapture location
# #OR
# #Find average per site and join by site...and date, will still need two columns, hmm
# 
# dat.sst%<>%
#   select(Date, Site, sst)%>%
#   group_by(Date, Site) %>%
#   summarise_all(funs(mean))%>%
#   glimpse()
# 
# glimpse(dat.sst)
# 
# #Create data frame to intial tag data----
# 
# k.dat.int<-k.dat%>%
#   select(Date, Site)%>%
#   glimpse()
# 
# k.dat.int.sst <- left_join(k.dat.int, dat.sst)%>%
#   glimpse() #Don't have all sst data- need to ask Matt!
# 
# #Join sst data with Kmeans cluster data----
# 
# k.dat.sst<- left_join(k.dat, dat.sst)%>%
#   glimpse()
# 
# #Bring in swell data----
# 
# dat.swell.18 <-gs_title("JDW2018")%>%
#   gs_read_csv(ws="Sheet1", header=TRUE)%>%
#   mutate(Date=as.Date(Date,format= "%d/%m/%Y"))%>%
#   fill(2:12, .direction = c("down"))%>%
#   group_by(Date) %>%
#   dplyr::summarise_all(funs(mean))%>%
#   distinct()%>%
#   dplyr::rename("Hs.m.sw"="Hs(m).sw",
#                 "Hs.m.sea"="Hs(m).sea",
#                 "T1.s.sw"="T1(s).sw",
#                 "T1.s.sea"="T1(s).sea")%>%
#   select(Date, Hs.m.sw, Hs.m.sea, T1.s.sw, T1.s.sea)%>%
#   ungroup()%>%
#   glimpse()
# 
# #2017 data----
# #Damn. Forgot about that. Added as sheet 2 to 2018 data. 
# 
# dat.swell.17 <-gs_title("JDW2018")%>%
#   gs_read_csv(ws="Sheet2", header=TRUE)%>%
#   mutate(Date=as.Date(Date,format= "%d/%m/%Y"))%>%
#   #mutate(Date= lubridate::ymd_hm(Date))%>%
#   fill(2:12, .direction = c("down"))%>%
#   group_by(Date) %>%
#   summarise_all(funs(mean))%>%
#   distinct()%>%
#   dplyr::rename("Hs.m.sw"="Hs(m).sw",
#                 "Hs.m.sea"="Hs(m).sea",
#                 "T1.s.sw"="T1(s).sw",
#                 "T1.s.sea"="T1(s).sea")%>%
#   select(Date, Hs.m.sw, Hs.m.sea, T1.s.sw, T1.s.sea)%>%
#   ungroup()%>%
#   glimpse()
# 
# #combine 2017 & 2018 swell data----  
# dat.swell<- rbind(dat.swell.17, dat.swell.18)%>%
#   glimpse()
# 
# #Want to find the min, max and average value for Hs.m and T1.s for each recapture...soooo...
# #let's reduce the data to only the sampling time-frame: Between 27-11-2017 to 05-12-2018
# 
# dat.swell%<>%
#   filter(Date >= as.Date("2017-11-27") & Date<= as.Date("2018-12-05"))%>%
#   glimpse()
# 
# setwd(data.dir)
# write.csv(dat.swell, "dat.swell.csv", row.names = F)
# 
# 
# #Initial data----
# k.dat.int<-k.dat%>%
#   select(Date)%>%
#   left_join(dat.swell)%>%
#   dplyr::rename("Hs.m.sw.int"="Hs.m.sw",
#                 "Hs.m.sea.int"="Hs.m.sea",
#                 "T1.s.sw.int"="T1.s.sw",
#                 "T1.s.sea.int"="T1.s.sea")%>%
#   dplyr::rename("date.int"="Date")%>%
#   glimpse()
# 
# #Recap date data----
# 
# k.dat.recap<-k.dat%>%
#   select(recap.Date)%>%
#   dplyr::rename("Date"="recap.Date")%>% #change recap.date to 'Date' so can join with swell data
#   left_join(dat.swell)%>%
#   dplyr::rename("Hs.m.sw.rec"="Hs.m.sw",
#                 "Hs.m.sea.rec"="Hs.m.sea",
#                 "T1.s.sw.rec"="T1.s.sw",
#                 "T1.s.sea.rec"="T1.s.sea")%>%
#   dplyr::rename("date.rec"="Date")%>%
#   
#   glimpse()
# 
# #Join k.dat dat with both sets of swell data
# glimpse(k.dat)
# 
# k.dat.sw<- k.dat%>%
#   dplyr::rename("date.int"="Date", "date.rec"="recap.Date")%>%
#   left_join(k.dat.int, by="date.int")%>%
#   left_join(k.dat.recap, by="date.rec")%>%
#   glimpse()
#   
# #Save just swell data for now, will eventually have sst from Matt!!
# setwd(data.dir)
# write_csv(k.dat.sw, "k.dat.sw.csv")




