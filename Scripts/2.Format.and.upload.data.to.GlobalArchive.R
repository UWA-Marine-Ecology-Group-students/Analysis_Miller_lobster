# Export catch data to GlobalArchive ----
rm(list=ls()) # Clears memory

# librarys----
library(tidyr)
library(dplyr)
library(stringr)
library(readr)
library(purrr)

# Set work directory ----
work.dir=("C:/GitHub/Analysis_Miller_lobster") # For Brooke

# Sub directories ----
data.dir<-paste(work.dir,"Data",sep="/")
uploads<-paste(data.dir,"Uploads",sep="/")

# Import data ----
setwd(data.dir)
dir()

metadata.raw<-read.csv("metadata.csv")%>%
  filter(Source%in%c("oscar-doncel-canons-masters","ash-millers-masters"))%>%
  mutate(Source=str_replace_all(.$Source,c("oscar-doncel-canons-masters"="Dongara.Canons.Masters","ash-millers-masters"="Dongara.Millers.Masters")))%>%
  dplyr::rename(Comment=Pot.remarks)%>%
  mutate(Successful.count=ifelse(Exclude.pots%in%c("Yes"),"No","Yes"))%>%
  mutate(Successful.length=ifelse(Exclude.pots%in%c("Yes"),"No","Yes"))%>%
  mutate(Successful.count=ifelse(Comment%in%c("Pot Broke"),"No",Successful.count))%>%
  mutate(Successful.length=ifelse(Comment%in%c("Pot Broke"),"No",Successful.length))%>%
  select(-c(Exclude.pots))%>%
  mutate(Time="12:00:00")%>%
  mutate(Status="Fished")%>%
  mutate(Depth=ifelse(!is.na(Depth.lidar),Depth.lidar,0))%>%
  mutate(Observer=ifelse(Source%in%c("Dongara.Canons.Masters"),"Oscar Doncel Canon","Ash Miller"))%>%
  glimpse()

length.raw<-read.csv("length.csv")%>%
  rename(Length=Carapace.length)%>%
  mutate(Family="Palinuridae",Genus="Panulirus",Species="cygnus")%>%
  filter(Source%in%c("oscar-doncel-canons-masters","ash-millers-masters"))%>%
  mutate(Source=str_replace_all(.$Source,c("oscar-doncel-canons-masters"="Dongara.Canons.Masters","ash-millers-masters"="Dongara.Millers.Masters")))

# Make a more GlobalArchive-y CampaignID
campaigns<-metadata.raw%>%
  distinct(Trip,Source,Date)%>%
  group_by(Trip,Source)%>%
  arrange(desc(Date))%>%
  slice(n())%>%
  ungroup()%>%
  mutate(CampaignID=paste(str_sub(Date,1,7),Source,"Trapping",sep="_"))%>%
  select(-c(Date))

# Add campaigns to metadata and length ----
metadata<-left_join(metadata.raw,campaigns)%>%
  mutate(Date=str_replace_all(.$Date,c("-"=""))) # fix format of date here - cant do above as need it to make campaignid

length<-left_join(length.raw,campaigns)

# Create count (only use if GlobalArchive won't let me upload without count)
count<-length%>%
  group_by(CampaignID,Sample,Trip,Family,Genus,Species)%>%
  summarise(Count=sum(Count))%>%
  ungroup()

# Split data into campaigns based on trip ---
# Metadata must have: Sample,	Latitude,	Longitude,	Date,	Time,	Location,	Status,	Site,	Depth,	Observer,	Successful.count	Succussful.length
# Length file must have: Sample, Family, Genus, Species, Count, Length
# Count file must have: Sample,	Family,	Genus,	Species,	Count
setwd(uploads)
dir()

campaigns <- unique(unlist(metadata$CampaignID))

for (i in 1:length(campaigns)){
  # Metadata
  temp.met <- subset(metadata, CampaignID == campaigns[i])
  id<-temp.met$CampaignID
  # Remove CampaignID from dataframe and then remove all columns where all are NA
  temp.met2<-temp.met%>%select(-c(CampaignID))
  temp.met3<-temp.met2[,which(unlist(lapply(temp.met2, function(x)!all(is.na(x)))))]
  # Make an empty data frame to bring back in any columns that were removed but need to be in the data
  columns<-c("Sample","Latitude","Longitude","Date","Time","Location","Status","Site","Depth","Observer","Successful.count","Successful.length","Comment")%>%map_dfr( ~tibble(!!.x := logical() ) )
  # Add back in cols that may have been deleted
  temp.met4<-bind_rows(columns, temp.met3)
  # write file
  write.csv(temp.met4, file=paste(unique(id),"_Metadata.csv",sep=""), quote=FALSE,row.names = FALSE,na = "")

  # Length
  temp.length <- subset(length, CampaignID == campaigns[i])%>%select(-c(CampaignID)) # Remove CampaignID from dataframe
  write.csv(temp.length, file=paste(unique(id),"_Length.csv",sep=""), quote=FALSE,row.names = FALSE,na = "") # write file
  
  # Count
  temp.count <- subset(count, CampaignID == campaigns[i])%>%select(-c(CampaignID)) # Remove CampaignID from dataframe
  write.csv(temp.count, file=paste(unique(id),"_Count.csv",sep=""), quote=FALSE,row.names = FALSE,na = "") # write file
}



