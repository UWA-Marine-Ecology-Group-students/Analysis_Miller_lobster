# Explore catch data----
rm(list=ls()) # Clears memory

# librarys----
library(tidyr)
library(dplyr)
library(googlesheets)
library(stringr)
library(lubridate)

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

# work.dir=("~/GitHub/Analysis_Miller_WRL") #for Tim's github
# work.dir=("~/workspace/Analysis_Miller_WRL") #for ecocloud server
# work.dir=("C:/GitHub/Analysis_Miller_lobster")
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
# Import and make data----

# # For Rstudio Server
options(httr_oob_default=TRUE)
# options(httr_oob_default=FALSE) #for desktop
# gs_auth(new_user = TRUE) #only run once

# Import 2018 length data----

dat.length<-gs_title("Lobsters_data_2018_All")%>% # To use GoogleSheets
  gs_read_csv(ws = "Lobster.var" )%>%
  glimpse()

dat.length.1<-dat.length%>%
  mutate(Count=1)%>%        #Count for Abundance
  mutate(Carapace.length=as.numeric(as.character(Carapace.length)))%>%
  mutate(trip.day.trap=paste(Trip,Day,Trap.ID,sep="."))%>%  #Trap.ID
  mutate(Colour=str_replace_all(.$Colour,c("W"="White", "R"="Red")))%>%
  mutate(Sex=str_replace_all(.$Sex, c("M"="Male", "F"="Female")))%>%
  mutate(ID=1:nrow(.))%>%
  group_by(ID)%>%
  replace_na(list(Damage.old.a = 0, Damage.old.L = 0,Damage.new.a = 0, Damage.new.L = 0))%>%
  dplyr::mutate(Total.damage=sum(Damage.old.a+Damage.old.L+Damage.new.a+Damage.new.L,na.rm = TRUE))%>%
  glimpse()


unique(dat.length$Remarks)
length(unique(dat.length.1$Tag.number))
#7537
length(dat.length.1$Carapace.length)
#9614

# # Import 2018 pot data----

dat.pot.1<-gs_title("Lobsters_data_2018_All")%>% # To use GoogleSheets
  gs_read_csv(ws = "Pot.var")%>%
  mutate(trip.day.trap=paste(Trip,Day,Trap.ID,sep="."))%>% 
  mutate(Site.Name=str_replace_all(.$Site.Name,c( "SM"="Seven Mile", "DM"="Davids Marks",  "RM"="Rivermouth", "IR"="Irwin Reef", "LR"="Long Reef", "SD"="South Dummy", "LH"="Little Horseshoe", "CHin1_"="Cliff Head Mid","CHin2_"="Cliff Head South","CHout1_" = "Cliff Head OUT1","CHout2_" = "Cliff Head North", "JB"="Jim Bailey", "GR"="Golden Ridge", "SR"="South Rig", "WL"="Whites Lump")))%>% 
  filter(Johns=="No")%>% # turn off when I add in john's data (if ever)
  dplyr::rename(Latitude=Latitude.y, Longitude=Longitude.x)%>%
  mutate(Latitude=as.numeric(Latitude))%>%
  mutate(Longitude=as.numeric(Longitude))%>%
  #mutate(Remarks=NA)%>%
  select(Trip, Site.Name, Pot.Number, Trap.ID, Day, Date.Recovered, Longitude, Latitude, trip.day.trap)%>%
  glimpse()


#Create "sites" for 2018 Data----
sites<-dat.pot.1%>%
  distinct(Trap.ID,Site.Name)%>% #Keeps only distinct rows (Trap.ID & Site.Name)
  mutate(Trap.ID=as.character(Trap.ID))%>%
  dplyr::rename(Site=Site.Name)%>%
  glimpse()

#Add a "Site" column by Trap.ID to dat.length----
dat.length.1<-left_join(dat.length.1,sites, by="Trap.ID") 

#Check for missing sites: ones in dat.length but not in dat.pot
#missing.site.1<-anti_join(dat.length.1,sites)  # 0

#Join Lobster and pot 2018 data----

data.oct <- dat.length.1%>%
  dplyr::filter(Remarks%in%c("PWO (1)", "1 X CARDinAL PWO (1)","Dead (octopus)","PWO(X)" ,"PWO(X), WKW (4)", "PWO(1), Crab (1)",  "PWO(1), Daed crab (1)","Dead recapture (by occy) PWO (X)","Dead-Occy","Occy","PWO(!)","Dead-by Occy, Had cliiped pleopod but no tag, Occy (x)","(x) occy, (1) wkw", "Still alive, Occy (1)","Occy (X)", "Dead by occy, PWO(X)" ,"PWO(x), Dead by occy" ,"PWO(x)", "PWO(1), Dead-by occy", "PWO(x), 2 tails" , "PWO (1), Dead-by Occy"  ,"Dead-by occy, PWO(X), (2) WKW","Dead-occy" ,"PWO(X), Dead-by occy" ,"DEAD-by occy, PWO(2)" , "DEAD -by occy ,PWO(1), WKW (1)", "PWO(X), DEAD-by occy", "PWO(1), wkw(1)" , "Dead-by occy, infected tag", "PWO (X), Dead-by occy", "DEAD-BY OCCY, PWO(X), WKW(3)", "DEAD- OCCY",  "Dead-occy, wkw (2)", "Dead-occy, wkw (2)" ,  "(2) PWO", "Dead- by Occy" ,   "PWO(1), Dead- by occy", "PWO(X), Dead- by occy", "DEAD- BY OCCY, PWO (X)", "Dead by occy, PWO (X)" , "Dead- by occy","By Occy" , "Dead-by occy, (1) PWO, (1) WKW" ,"Dead-by occy, (1) PWO", "Dead- Occy", "Half dead (octopus)", "PWO(1)",  "Dead, PWO(X)", "Dead (octopus), PWO (1)" ,  "PWO (X)","MAYBE OCCY?","WOBBY 0.8 M","1.5 m wobby"))%>%
  ungroup()%>%
  select(trip.day.trap)%>%
  glimpse()

length(unique(data.oct$trip.day.trap))


#Filter out Octopus----
dat.1 <- dplyr::left_join(dat.length.1, dat.pot.1,by = c("trip.day.trap"))%>%
  ungroup()%>%
  select(trip.day.trap,  Date, Tag.number, Recapture,  Carapace.length, Sex, Colour, Site, Longitude, Latitude, Remarks)%>%
  mutate(Location=str_replace_all(Site,c("Seven Mile Beach"= "Seven Mile","Little Horseshoe"="Cliff Head", "Cliff Head North"="Cliff Head","Cliff Head Mid"= "Cliff Head","Cliff Head South"="Cliff Head","Cliff Head OUT1"= "Cliff Head","CHM"="Cliff Head", "Davids Marks"="Cliff Head","CHM"= "Cliff Head", "CHS"="Cliff Head", "CHN"="Cliff Head", "Jim Bailey"="Irwin Reef", "Long Reef"="Irwin Reef", "South Dummy"="Irwin Reef","South Rig"= "Irwin Reef","Whites Lump"= "Irwin Reef","WP"= "Irwin Reef","Whitepoint"="Irwin Reef")))%>%
  mutate(Site=str_replace_all(.$Site, c("Jim Bailey"="White Point" ,"WP"="White Point" , "Whitepoint"="White Point" , "CHS"="Cliff Head South","CHM"="Cliff Head Mid","CHN"="Cliff Head North", "Seven Mile"="Seven Mile.out")))%>%
  anti_join(data.oct)%>%
  filter(!Location=="Rivermouth")%>%
  mutate(Count=ifelse(is.na(Carapace.length),0,1))%>%
  glimpse()


#Import 2017 length data----

dat.length.2<-gs_title("Lobsters_data_20180214")%>% # To use GoogleSheets
  gs_read_csv(ws = "Lobster.var" )%>%
  mutate(Count=1)%>%        #Count for Abundance
  mutate(Carapace.length=as.numeric(as.character(Carapace.length)))%>%
  mutate(Recapture=NA)%>%
  mutate(Trip=0)%>%
  mutate(trip.day.trap=paste(Trip,Day,Trap.number,sep="."))%>%  #New column for Day and trap.number
  select(Trip, Day, Trap.number, Carapace.length, Sex, Colour, Tag.number, Recapture, trip.day.trap)%>%
  glimpse()


#Import 2017 pot data----

dat.pot.2<-gs_title("Lobsters_data_20180214")%>% # To use GoogleSheets
  gs_read_csv(ws = "Pot.var")%>%
  mutate(Trip=0)%>%
  mutate(trip.day.trap=paste(Trip, Day,Trap.number,sep="."))%>% 
  select(Trip, Day, Date, Trap.number, Location, Number,Longitude.original, Latitude.original, trip.day.trap)%>%
  glimpse()


#Create "sites" for 2017 Data----
sites.2<-dat.pot.2%>%
  distinct(trip.day.trap,Location)%>% #Keeps only distinct rows (Trap.ID & Site.Name)
  dplyr::rename(Site=Location)%>%
  glimpse()


#Add a "Site" column by Trap.ID to dat.length----
glimpse(sites.2)

dat.all.2<-left_join(dat.length.2,sites.2, by="trip.day.trap")%>%
  glimpse()

#Check for missing sites: ones in dat.length but not in dat.pot
missing.site.2<-anti_join(dat.length.2,sites.2)  # 0

#Join 2017 Lobster and pot data----
glimpse(dat.length.2)

dat.2 <- dplyr::left_join(dat.all.2, dat.pot.2)%>%
  dplyr::rename(Longitude=Longitude.original, Latitude=Latitude.original, Trap.ID=Trap.number, Trap.Number=Number)%>% 
  mutate(Latitude=as.numeric(Latitude))%>%
  mutate(Longitude=as.numeric(Longitude))%>%
  select(trip.day.trap,Date,Tag.number, Recapture,Carapace.length,Sex, Colour,Site, Longitude, Latitude)%>%
  glimpse()

dat.2%<>%
  mutate(Total.damage="0")%>%
  mutate(Remarks=NA)%>%
  mutate(Location="Seven Mile")%>%
  mutate(Count=1)%>%
  select(trip.day.trap,Date,Tag.number, Recapture,Carapace.length,Sex, Colour,Site, Longitude, Latitude, Remarks,Location,Count)%>%
  glimpse()

#Check data that's in dat.length.all but not in dat.pot
#missing.dat.2 <-anti_join(dat.length.2, dat.pot.2) #0!

#Combine 2017 and 2018 data ----
glimpse(dat.1)
glimpse(dat.2)


dat.all <- bind_rows(dat.1, dat.2)%>%
  dplyr::mutate(Date=as_date(dmy(Date)))%>%
  glimpse()

glimpse(dat.all)

dat.legal <-dat.all%>%
  mutate(sizeclass= ifelse(Carapace.length>=76.0,"Legal", "Sublegal"))%>% 
  filter(!is.na(Carapace.length))%>%
  glimpse()

sum.legal <-dat.legal%>%
  group_by(trip.day.trap, sizeclass)%>%
  dplyr::summarise(Count=sum(Count))%>%
  ungroup()%>%
  distinct()%>%
  glimpse()


#FIND SUM PER POT----
glimpse(dat.all)

sum.dat <-dat.all%>%
  group_by(trip.day.trap)%>%
  dplyr::summarise(Count=sum(Count))%>%
  ungroup()%>%
  mutate(sizeclass="All")%>%
  distinct()%>%
  bind_rows(.,sum.legal)%>%
  arrange(trip.day.trap)%>%
  glimpse()

dat.location <- dat.all%>%
  select(Date, trip.day.trap, Location, Site, Longitude, Latitude)%>%
  distinct()%>%
  group_by(trip.day.trap)%>%
  slice(1)%>% # Fix the 5 errors and turn off the group by and the slice
  glimpse()

test<-dat.location%>%
  group_by(trip.day.trap)%>%
  summarise(n=n())

length(unique(dat.location$trip.day.trap)) # 1754

#JOIN BACK WITH DATA: Trap.Id, Sum per pot, Location, Site----
test.dat<-sum.dat%>%distinct(trip.day.trap)

sum.per.pot<-full_join(sum.dat,dat.location)%>%
  complete(sizeclass,nesting(trip.day.trap))%>%
  select(sizeclass,trip.day.trap,Count)%>%
  left_join(.,dat.location)%>%
  replace_na(list(Count=0))%>%
  arrange(trip.day.trap)%>%
  glimpse()

length(unique(sum.per.pot$trip.day.trap))
1754*3

catch.per.pot<-sum.per.pot

#Join Count data (catch.per.pot) with Swell data (dat.swell)----
glimpse(catch.per.pot)
catch.sw <- left_join(catch.per.pot, dat.swell, by="Date")%>%
  glimpse()


#ALRIGHT! Let's bring in SST data for out Dates!!!!!!! - Use Mathew's script function thang----
catch.sst<-catch.sw%>%
  select(Date, Longitude, Latitude)%>%
  dplyr::rename("Lat"="Latitude", "Long"="Longitude")%>%
  mutate(Lat=as.numeric(Lat))%>%
  mutate(Long=as.numeric(Long))%>%
  #mutate(SiteNo=paste(Lat,"",Long))%>%
  glimpse()

catch.sst%<>%
  unite(SiteNo, c(Lat,Long), sep = "", remove = F)%>% 
  glimpse()


#USE MATTS SST FUNCTION----
catch.sst$sst<-get.sst(catch.sst)
glimpse(catch.sst)


av.catch.sst<-catch.sst%>% #Some have the same lats and longs and sst 
  select(Date, SiteNo, sst)%>%
  group_by(Date, SiteNo)%>%
  summarise_all(funs(mean))%>%
  glimpse()

catch.sw%<>% # Need to create a similar column 'SiteNo'
  unite(SiteNo, c(Latitude,Longitude), sep ="" , remove = F)%>% 
  glimpse()

#COMBINE CATCH, SST AND SWELL DATA---- 
sub.legal.sw.sst <- left_join(catch.sw, av.catch.sst, by=c("SiteNo", "Date"))%>%
  glimpse()

#saaaaaaaaaveeeee for GAM-----
setwd(data.dir)
write.csv(sub.legal.sw.sst,"sub.legal.sw.sst.csv", row.names=F)

