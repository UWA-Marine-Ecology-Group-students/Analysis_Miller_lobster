# Explore length data----
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
work.dir=("Z:/Analysis_Miller_lobster") # FOr Ash's laptop using Git

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
# Import length data----
setwd("Z://Analysis_Miller_lobster/Data")

dat.length<-read_csv("length.csv")%>%
  filter(!is.na(Carapace.length))%>%
  filter(!Carapace.length<0)%>%
  select(Sample, Tag.number, Carapace.length, Sex, Colour)%>%
  glimpse()

#Check
length(unique(dat.length$Tag.number)) #9698
length(dat.length$Carapace.length) #14448
glimpse(dat.length)

#Import Pot data----
dat.pot<-read.csv("metadata.csv")%>%
  dplyr::mutate(Date=as_date(ymd(Date)))%>%
  filter(!Location=="Rivermouth")%>% #Removes NAs from Fishers as well. good.
  select(Date, Location, Site,Day.pull, Sample, Pot.number, Longitude, Latitude )%>%
  glimpse()

#Check
unique(dat.pot$Location)#SM, CH, LHS, IR, GR, WP
unique(dat.pot$Sample)

#Join Length and Pot data----
glimpse(dat.length) #14,448
glimpse(dat.pot) #2,011
length(unique(dat.pot$Sample)) #2008
length(unique(dat.length$Sample)) # 1759

dat.all<- dplyr::semi_join(dat.length, dat.pot)%>% #Semi as it will exclude Fisher recaps lengths that have NAs in pot data
  dplyr::left_join(.,dat.pot)%>%
  glimpse()

length(unique(dat.all$Sample)) #1550
unique(dat.all$Location)

#Bring in swell data for 2018----

dat.swell.18 <-gs_title("JDW2018")%>%
  gs_read_csv(ws="Sheet1", header=TRUE)%>%
  mutate(Date=as.Date(Date,format= "%d/%m/%Y"))%>%
  fill(2:12, .direction = c("down"))%>% #Some data is missing from certain days
  group_by(Date) %>%
  summarise_all(funs(mean))%>% #Find average per day
  distinct()%>%
  dplyr::rename("Hs.m.sw"="Hs(m).sw",
                "Hs.m.sea"="Hs(m).sea",
                "T1.s.sw"="T1(s).sw",
                "T1.s.sea"="T1(s).sea")%>%
  select(Date, Hs.m.sw, Hs.m.sea, T1.s.sw, T1.s.sea)%>%
  ungroup()%>%
  glimpse()

#2017 data----
#Added as sheet 2 to 2018 data. 

dat.swell.17 <-gs_title("JDW2018")%>%
  gs_read_csv(ws="Sheet2", header=TRUE)%>%
  mutate(Date=as.Date(Date,format= "%d/%m/%Y"))%>%
  fill(2:12, .direction = c("down"))%>%
  group_by(Date) %>%
  summarise_all(funs(mean))%>%
  distinct()%>%
  dplyr::rename("Hs.m.sw"="Hs(m).sw",
                "Hs.m.sea"="Hs(m).sea",
                "T1.s.sw"="T1(s).sw",
                "T1.s.sea"="T1(s).sea")%>%
  select(Date, Hs.m.sw, Hs.m.sea, T1.s.sw, T1.s.sea)%>%
  ungroup()%>%
  glimpse()

#combine 2017 & 2018 swell data----  
dat.swell<- rbind(dat.swell.17, dat.swell.18)%>%
  glimpse()

#Join length data  with Swell data ----
glimpse(dat.all)

length.swell <- left_join(dat.all, dat.swell, by="Date")%>%
  glimpse()

#Now bring in SST data----
length.sst<-length.swell%>%
  select(Date, Longitude, Latitude)%>%
  dplyr::rename("Lat"="Latitude", "Long"="Longitude")%>%
  mutate(Lat=as.numeric(Lat))%>%
  mutate(Long=as.numeric(Long))%>%
  unite(SiteNo, c(Lat,Long), sep = "", remove = F)%>% 
  glimpse()

#Bring in SST data- using Mathew's function----
length.sst$sst<-get.sst(length.sst)
glimpse(length.sst)

#Average sst per Date per location----
av.length.sst<-length.sst%>% 
  select(Date, SiteNo, sst)%>%
  group_by(Date, SiteNo)%>%
  summarise_all(funs(mean))%>%
  glimpse()

length.swell%<>% # Need to create a similar column 'SiteNo'
  unite(SiteNo, c(Latitude,Longitude), sep ="" , remove = F)%>% 
  glimpse()

#COMBINE CATCH, SST AND SWELL DATA---- 
length.sw.sst <- left_join(length.swell, av.length.sst, by=c("SiteNo", "Date"))%>%
  glimpse()

#Save for GAM-----
setwd(data.dir)
write.csv(length.sw.sst,"length.sw.sst.csv", row.names=F)
