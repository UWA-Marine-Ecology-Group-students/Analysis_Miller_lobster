# Explore catch data----
rm(list=ls()) # Clears memory

# librarys----
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(sf)
library(weathermetrics)
require(raster)
require(tidyverse)
require(devtools)
require(sf)
require(magrittr)
require(ncdf4)
require(lubridate)
require(doParallel)
require(parallel)
require(foreach)
require(tidync)
require(doSNOW)
require(tcltk)


# Study name----
study<-"Lobster.Data"

# Set work directory----
work.dir=("Z:/Analysis_Miller_lobster") # FOr Ash's laptop using Git

work.dir=("C:/GitHub/Analysis_Miller_lobster_current") # Brooke's desktop

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
  data <-catch.sst[1:100, ]
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
  
  # First download the list of data products on the server
  server_data <- rerddap::ed_datasets(which = "griddap", "https://www.ncei.noaa.gov/erddap/")$Dataset.ID
  
  # Check if the "final" data are currently hosted
  if(!"ncdc_oisst_v2_avhrr_by_time_zlev_lat_lon" %in% server_data)
    stop("Final data are not currently up on the ERDDAP server")
  
  # Check if the "prelim" data are currently hosted
  if(!"ncdc_oisst_v2_avhrr_prelim_by_time_zlev_lat_lon" %in% server_data)
    stop("Prelim data are not currently up on the ERDDAP server")
  #rerddap::info(datasetid = "ncdc_oisst_v2_avhrr_by_time_zlev_lat_lon", url = "https://www.ncei.noaa.gov/erddap/")
  
  OISST_sub <- function(times){ #function to download sst data from noaa
    oisst_res <- griddap(x = "ncdc_oisst_v2_avhrr_by_time_zlev_lat_lon", 
                         url = "https://coastwatch.pfeg.noaa.gov/erddap/",
                         #url = "https://www.ncei.noaa.gov/erddap/", 
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
  # 
  # for (i in 1:length(years)){ #loop to get start and end dates
  # 
  #   start<-paste0(years[i], "-01-01T00:00:00Z")
  #   end<-paste0(years[i], "-12-31T00:00:00Z")
  #   temp <- OISST_sub(c(start, end)) #download data
  #   temp_prepped <- OISST_prep(temp) #prep data
  #   
  #   if(i==1){OISST_all<-temp_prepped}else{ 
  #     OISST_all <- rbind(OISST_all, temp_prepped)}#bind data
  #   
  # } #this downloads the data for interesting years and geographic range and binds it all together
  
  
    start<-paste0("2018", "-01-01T00:00:00Z")
    end<-paste0("2018", "-12-31T00:00:00Z")
    temp <- OISST_sub(c(start, end)) #download data
    temp_prepped <- OISST_prep(temp) #prep data
    
    OISST_all<-temp_prepped
    
  
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



devtools::source_url("https://github.com/aodn/imos-user-code-library/blob/master/R/commons/NetCDF/ncParse.R?raw=TRUE")

#get_sst

#get_sst is a function to extract the sst given a series of Dates and points (Latitudes and Longitude). The function
#returns a single column of sst corresponding to the input Dates and points.
#sst is extracted from the AODN website and corresponds to a 6 day moving average. This was used to minimise holes
#and for better reach into coastal areas. SST resolution is 0.02deg. The extraction function searches for data within 
#100 and then 10000 and then 100000m from the point. Failing that it returns a warning
#more details on source data at: https://catalogue-imos.aodn.org.au/geonetwork/srv/api/records/023ae12a-8c0c-4abc-997a-7884f9fec9cd

#ARGUMENTS
#Dates: is a data vector (created using as.Date) with format YYYY-MM-DD. 
#Latitude: is the latitude of the point files in GDA94
#Longitude is the longitude of the pont files in GDA94

get_sst<- function(Dates, Long, Lat){
  
  Points<-tibble(Dates = Dates, Long = Long,  Lat = Lat) %>%  
    sf::st_as_sf(coords = c("Long", "Lat"), crs = 4283) %>% #only extract for points on this day
    st_transform(3112) %>%
    dplyr::mutate(ID = row_number())
  
  #ggplot() +  geom_sf(data = Australia_map_Lambert) + geom_sf(data = Points) 
  
  Points <- Points#%>% mutate(land = as.integer(sf::st_intersects(geometry, Australia_map_Lambert))) #returns NA for water
  
  Points_extraction <- Points #%>% filter(is.na(land))
  Date_code <- as.character(Points_extraction$Dates)
  
  Date_code <-Date_code%>% str_replace_all(., "-", "") %>% unique() #Extra Date codes for each day desired
  
  for(i in 1:length(Date_code)) { #loop through days download and extract data
    print(i)
    file_URL<- paste0("http://thredds.aodn.org.au/thredds/dodsC/IMOS/SRS/SST/ghrsst/L3S-6d/dn/", 
                      substr(Date_code[i], 1,4), "/", 
                      Date_code[i], 
                      "212000-ABOM-L3S_GHRSST-SSTfnd-AVHRR_D-6d_dn.nc")
    
    dataset <- ncParse(file_URL, variables = "sea_surface_temperature")
    
    ## Extract data from variables and dimensions
    lat <- dataset$dimensions$lat$data
    lon <- dataset$dimensions$lon$data
    temp <- dataset$variables$sea_surface_temperature$data
    
    ## Create a raster of longitude, latitude, and temperature data
    dat1 <- list( )
    dat1$x <- c( lon)
    dat1$y <- c( lat)
    dat1$z <- t( temp)
    raster <- raster( dat1$z, xmn = range( dat1[[1]])[1], xmx = range( dat1[[1]])[2], ymn = range( dat1[[2]])[1], ymx = range( dat1[[2]])[2])
    
    crs(raster)<- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')#in WGS84
    
    #Get points ready
    temp <- Points_extraction %>% filter(Dates == ymd(Date_code[i])) %>% st_transform(4326) #only extract for points on this day
    
    temp$sst.100<-raster::extract(raster, temp, buffer = 100, fun=mean)
    temp$sst.10000<-raster::extract(raster, temp, buffer = 10000, fun=mean)
    temp$sst.100000<-raster::extract(raster, temp, buffer = 100000, fun=mean)
    temp$sst <-ifelse(is.na(temp$sst.100), ifelse(is.na(temp$sst.10000), temp$sst.100000, temp$sst.10000), temp$sst.100)
    
    if(i==1){points_extracted<-temp}else{ points_extracted<-rbind(temp,points_extracted)}
  }
  points_extracted <- points_extracted%>% st_drop_geometry()
  if( any(is.na(points_extracted$sst))) warning('raster extract returned NA consider increasing bufffer size')
  Points <- Points%>% left_join(.,points_extracted[, c("ID", "sst")], by = c("ID" = "ID"))
  Points$sst
}


# Import Catch data----
setwd(data.dir)
dir()

dat.catch <- read.csv("dat.catch.csv")%>%
  dplyr::mutate(Date=as_date(ymd(Date)))%>%
  glimpse()

#Bring in swell data for 2018----
dir()
dat.swell.18 <-read.csv("JDW2018.csv")%>%
  mutate(Date=as.Date(Date,format= "%d/%m/%Y"))%>%
  fill(2:12, .direction = c("down"))%>% #Some data is missing from certain days. k. cool. whatever.
  group_by(Date) %>%
  summarise_all(funs(mean))%>% #Find average per day
  distinct()%>%
  dplyr::rename("Hs.m.sw"="Hs.m..sw",
                "Hs.m.sea"="Hs.m..sea",
                "T1.s.sw"="T1.s..sw",
                "T1.s.sea"="T1.s..sea")%>%
  dplyr::select(Date, Hs.m.sw, Hs.m.sea, T1.s.sw, T1.s.sea)%>%
  ungroup()%>%
  glimpse()

#2017 data----
# Added as sheet 2 to 2018 data. 
dat.swell.17 <- read.csv("JDW2017.csv")%>%
  mutate(Date=as.Date(Date,format= "%d/%m/%Y"))%>%
  fill(2:12, .direction = c("down"))%>%
  group_by(Date) %>%
  summarise_all(funs(mean))%>%
  distinct()%>%
  dplyr::rename("Hs.m.sw"="Hs.m..sw",
                "Hs.m.sea"="Hs.m..sea",
                "T1.s.sw"="T1.s..sw",
                "T1.s.sea"="T1.s..sea")%>%
  dplyr::select(Date, Hs.m.sw, Hs.m.sea, T1.s.sw, T1.s.sea)%>%
  ungroup()%>%
  glimpse()

#combine 2017 & 2018 swell data----  
dat.swell<- rbind(dat.swell.17, dat.swell.18)%>%
  glimpse()


#Join Catch data  with Swell data (dat.swell)----
glimpse(dat.catch)
catch.swell <- left_join(dat.catch, dat.swell, by="Date")%>%
  glimpse()

#Now bring in SST data----
catch.sst<-catch.swell%>%
  dplyr::select(Date, Longitude, Latitude)%>%
  dplyr::rename("Lat"="Latitude", "Long"="Longitude")%>%
  mutate(Lat=as.numeric(Lat))%>%
  mutate(Long=as.numeric(Long))%>%
  unite(SiteNo, c(Lat,Long), sep = "", remove = F)%>% 
  glimpse()



sites <- catch.sst %>%
  distinct(Date, Long, Lat) %>%
  mutate(Date=as.character(Date))

dir()
recovered <-read.csv("dat.all.sst.recovered.by.mat.csv")

recovered.sites <- recovered %>%
  distinct(Date, Long, Lat) %>%
  mutate(Date=as.character(Date))

missing <- anti_join(sites,recovered.sites)
unique(missing$Date)%>%sort

recovered.brooke <- read.csv("catch.sw.sst.recovered.brooke.csv")%>%
  glimpse()

recovered.sites.brooke <- recovered.brooke %>%
  distinct(Date, Longitude, Latitude) %>%
  mutate(Date=as.character(Date))%>%
  rename(Lat=Latitude,Long=Longitude)

missing.brooke <- anti_join(sites,recovered.sites.brooke)

unique(missing.brooke$Date)%>%sort()
# 233 are from 2019 - after Ash submitted her thesis.
# where do the other 148 come from??????
381-233

length(unique(catch.sst$Long))

write.csv(catch.sst,"catch.sst.notworking.csv")


#sub.sites.sst.100$sst.c<-sub.sites.sst.100$sst-273.15
# ~ 2000 sites

sub.sites.50<-sites[1:50,]
sub.sites.sst.50<-sub.sites.50
sub.sites.sst.50$sst<-get_sst(sub.sites.50$Date, sub.sites.50$Long, sub.sites.50$Lat)

sub.sites.100<-sites[51:100,]
sub.sites.sst.100<-sub.sites.100
sub.sites.sst.100$sst<-get_sst(sub.sites.100$Date, sub.sites.100$Long, sub.sites.100$Lat)

sub.sites.150<-sites[101:150,]
sub.sites.sst.150<-sub.sites.150
sub.sites.sst.150$sst<-get_sst(sub.sites.150$Date, sub.sites.150$Long, sub.sites.150$Lat)

glimpse(sub.sites.sst)

#df2<-split(sites, rep(1:1, each = 50)) 
str(df2)
glimpse(df2)
summary(df2)

score<-sapply(df2,get_sst())[1,] 

score<-lapply(df2,get_sst(df2$Date,df2$Long,df2$Lat))[1,]

score<-sapply(df2,get_sst())[1,]

str(df2)

split.sites <- split(sites, rep(1:43, each = 50)) # split data up into chunks that will run
datalist = list() # create a blank data frame

for (i in 1:6) { # check on a smaller number first
  temp <- split.sites[[i]] # create data frame with one chunk
  temp$sst<-get_sst(temp$Date, temp$Long, temp$Lat) # use Mats SST function
  temp$i <- i  # keep track of iteration
  datalist[[i]] <- temp # add it to your list
  print(temp$i)
}

big_data = do.call(rbind, datalist) # combine into one dataframe
write.csv(big_data,"1-6 of subset.csv")
split.sites <- split(sites, rep(1:43, each = 50)) # split data up into chunks that will run
datalist2 = list() # create a blank data frame

for (i in 10:43) { # check on a smaller number first
  temp <- split.sites[[i]] # create data frame with one chunk
  temp$sst<-get_sst(temp$Date, temp$Long, temp$Lat) # use Mats SST function
  temp$i <- i  # keep track of iteration
  datalist2[[i]] <- temp # add it to your list
  print(temp$i)
}
big_data2 = do.call(rbind, datalist2) # combine into one dataframe
write.csv(big_data2,"10-43 of subset.csv")
print(1)

#Bring in SST data- using Mathew's function----
# catch.sst$sst<-get.sst(catch.sst) #Keeps getting an error: Server is down? Fixed!
glimpse(catch.sst)

#Find average
av.catch.sst<-catch.sst%>% #Some have the same lats and longs and sst 
  select(Date, SiteNo, sst)%>%
  group_by(Date, SiteNo)%>%
  summarise_all(funs(mean))%>%
  glimpse()

catch.swell%<>% # Need to create a similar column 'SiteNo'
  unite(SiteNo, c(Latitude,Longitude), sep ="" , remove = F)%>% 
  glimpse()

#COMBINE CATCH, SST AND SWELL DATA---- 
catch.sw.sst <- left_join(catch.swell, av.catch.sst, by=c("SiteNo", "Date"))%>%
  glimpse()

#Save for GAM-----
setwd(data.dir)
write.csv(catch.sw.sst,"catch.sw.sst.csv", row.names=F)

#Option for when sst function is down
old.sst <- read.csv("catch.sw.sst.csv")%>%
  select(-Count)%>%
  glimpse()

new.count<-catch.swell%>%
  select(Count, Sample, sizeclass)%>%
  glimpse()

new.data<-left_join(old.sst, new.count)%>%
  glimpse()


setwd(data.dir)
write.csv(new.data,"catch.sw.sst.csv", row.names=F)
