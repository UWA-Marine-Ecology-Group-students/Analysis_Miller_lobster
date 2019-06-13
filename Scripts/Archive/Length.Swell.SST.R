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

#For Desktop
# work.dir=("C:/Users/00097191/Google Drive/MEG/Projects/Projects_WRL/Project_WRL_low-catch zone/Fieldwork and Reporting/03_Trapping/Analysis_WRL_Reds_2018")

#For Laptop
# setwd("~/Google Drive/Analysis_WRL_Reds_2018/Data")
# setwd(work.dir)
# dir()

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

# Import and make data----
# Bring in Ash and Oscar's data
length.uwa <-read_csv("dat.all.17-18.csv")%>%
  mutate(Location=Site)%>% 
  glimpse()


length.uwa%<>%
  mutate(Location=str_replace_all(.$Location,c("Seven Mile Beach"= "Seven Mile","Little Horseshoe"="Cliff Head", "Cliff Head North"="Cliff Head","Cliff Head Mid"= "Cliff Head","Cliff Head South"="Cliff Head","Cliff Head OUT1"= "Cliff Head","CHM"="Cliff Head", "Davids Marks"="Cliff Head","CHM"= "Cliff Head", "CHS"="Cliff Head", "CHN"="Cliff Head", "Jim Bailey"="Irwin Reef", "Long Reef"="Irwin Reef", "South Dummy"="Irwin Reef","South Rig"= "Irwin Reef","Whites Lump"= "Irwin Reef","WP"= "Irwin Reef","Whitepoint"="Irwin Reef")))%>% 
  mutate(Site=str_replace_all(.$Site, c("Jim Bailey"="White Point" ,"WP"="White Point" , "Whitepoint"="White Point" , "CHS"="Cliff Head South","CHM"="Cliff Head Mid","CHN"="Cliff Head North", "Seven Mile Beach"= "Seven Mile.out")))%>%
  select(Date, Tag.number, Carapace.length, Location, Sex, Longitude, Latitude, Site)%>%
  glimpse()


#Bring in Ben's Seven Mile data----

length.smb <- gs_title("Lobster_Data_Fisheries_SMB")%>%
  gs_read_csv("Dat.smb")%>%
  mutate(Tag.number=as.character(Tag.number))%>%
  mutate(Location=Site)%>% 
  glimpse()

length.smb%<>%
  mutate(Site=str_replace_all(.$Site,c("Seven Mile Beach"="Seven Mile.in")))%>%
  mutate(Location=str_replace_all(.$Location,c("Seven Mile Beach"="Seven Mile")))%>%
  mutate(Trip=paste("T",Trip,sep=""))%>%
  mutate(month=format(as.Date(Date),'%m'))%>%
  mutate(month=month((as_date(Date))))%>%
  filter(month%in%c(5:12))%>% #Remove Jan-April
  select(Date, Tag.number, Carapace.length, Location, Sex, Longitude, Latitude, Site)%>%
  glimpse()
  
  #Combine Bens (fisheries) and uwa data----


dat.all.length<- rbind(length.uwa, length.smb)%>%
  glimpse()


#SAVE DATA----
setwd(data.dir)
write.csv(dat.all.length, "dat.all.length.csv", row.names=F)

#IMPORT DATA AND COMBINE WITH SWELL DATA----

dat.all.length<-read_csv("dat.all.length.csv")%>%
  glimpse()

#Bring in Swell Data----

dat.swell<-read_csv("dat.swell.csv")%>%
  glimpse()

#combine----

length.swell<-left_join(dat.all.length, dat.swell)%>%
  glimpse()

#SAVE SWELL DATA AND LENGTH
setwd(data.dir)
write.csv(length.swell, "length.swell.csv", row.names=F)

#IMPORT LENGTH DATA----

#GET DATA READY FOR MATTS SST FUNCTION (NOT CURRENTLY WORKING)
glimpse(length.swell)

length.sst<-length.swell%>%
  select(Date, Longitude, Latitude)%>%
  dplyr::rename("Lat"="Latitude", "Long"="Longitude")%>%
  # mutate(Lat=as.numeric(Lat))%>%
  # mutate(Long=as.numeric(Long))%>%
  unite(SiteNo, c(Lat,Long), sep = "", remove = F)%>% #Okay can't figure out how to 
    glimpse()

#USE MATTS SST FUNCTION----
length.sst$sst<-get.sst(length.sst)

#yyyyyyyyyyyaaaaaaaaaaaaaaaaaaaaaaaaaassssssssssssss it worked!

glimpse(length.sst)

av.length.sst<-length.sst%>% #Some have the same lats and longs and sst 
  select(Date, SiteNo, sst)%>%
  group_by(Date, SiteNo)%>%
  summarise_all(funs(mean))%>%
  glimpse()

length.swell%<>%
  unite(SiteNo, c(Latitude,Longitude), sep ="" , remove = F)%>% #Okay can't figure out how to 
  glimpse()

length.sw.sst <- left_join(length.swell, av.length.sst, by=c("SiteNo", "Date"))%>%
  glimpse()

#Fucking finally~~~~~~holy moly!!!!

setwd(data.dir)
write.csv(length.sw.sst, "length.sw.sst.csv", row.names = F)

