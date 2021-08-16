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

