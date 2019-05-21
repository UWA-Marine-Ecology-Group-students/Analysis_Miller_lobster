### Secure access to EventMeasure or generic stereo-video annotations from Campaigns, Projects and Collaborations within GlobalArchive

### OBJECTIVES ###
# 1. use an API token to access Projects and Collaborations shared with you.
# 2. securely download any number of Campaigns within a Project
# 3. combine multiple Campaigns into single Metadata, Count and Length files for subsequent validation and data analysis.

### Please forward any updates and improvements to tim.langlois@uwa.edu.au & brooke.gibbons@uwa.edu.au or raise an issue in the "globalarchive-query" GitHub repository


rm(list=ls()) # Clear memory

## Load Libraries ----
# To connect to GlobalArchive
library(devtools)
# install_github("UWAMEGFisheries/GlobalArchive") #to check for updates
library(GlobalArchive)
library(httr)
library(jsonlite)
library(R.utils)
# To connect to GitHub
library(RCurl)
# To tidy data
library(plyr)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)


## Set Study Name ----
# Change this to suit your study name. This will also be the prefix on your final saved files.
study<-"combined.lobster.dat" 

## Folder Structure ----
# This script uses one main folder ('working directory')
# Three subfolders will be created within the 'Data' folder within your working directory. They are 'Downloads','Staging' and 'Tidy data'
# The 'Downloads' folder saves files downloaded from GlobalArchive.
# The 'Staging' folder is used to save the combined files (e.g. metadata, maxn or length) NOTE: These initial outputs have not gone through any check (e.g. checks against the life-history sheet)
# **The only folder you will need to create is your working directory**

## Set your working directory ----
working.dir=("C:/GitHub/Analysis_Miller_lobster")

## Save these directory names to use later----
data.dir<-paste(working.dir,"Data",sep="/") 
staging.dir<-paste(data.dir,"Staging",sep="/") 
download.dir<-paste(data.dir,"Downloads",sep="/")
tidy.dir<-paste(data.dir,"Tidy data",sep="/")

## Delete Downloads folder ----
# It will delete any data sitting within your 'Downloads' folder 
# DO NOT SAVE ANY OTHER FILES IN YOUR DOWNLOADS FILE
# After running this line they will not be recoverable
# This avoids doubling up GlobalArchive files, or including files from other Projects.
setwd(working.dir)
unlink(download.dir, recursive=TRUE)

## Create Downloads, Staging and Tidy data folders ----
dir.create(file.path(working.dir, "Data"))
dir.create(file.path(data.dir, "Downloads"))
dir.create(file.path(data.dir, "Staging"))
dir.create(file.path(data.dir, "Tidy data"))

## Query from GlobalArchive----
# Load default values from GlobalArchive ----
source("https://raw.githubusercontent.com/UWAMEGFisheries/GlobalArchive/master/values.R")

# An API token allows R to communicate with GlobalArchive

# Finding your API token
# 1. Go to GlobalArchive and login.
# 2. On the front page Click 'Data'.
# 3. In the top right corner click on your name, then 'API token'
# 4. Generate an API token and copy it.
# 5. Paste it below

# Set as tims at the moment

# Add your personal API user token ----
API_USER_TOKEN <- "993ba5c4267b9f8cd21de73b0434c95bc72f518a4f6e725226986022"

# Set up your query ----
## Download data ----
# These files will be saved in the 'Downloads' folder within your working directory folder
# NOTE: change any spaces in the project name to '+'

ga.get.campaign.list(API_USER_TOKEN, process_campaign_object, 
                     q=query.project("FRDC+low+catch+zone"))

# Combine all downloaded data----
## Metadata files ----
metadata <-list.files.GA("Metadata.csv")%>%
  purrr::map_df(~read_files_csv(.))%>%
  glimpse()

## Length files ----
length <-list.files.GA("Length.csv")%>%
  purrr::map_df(~read_files_csv(.))%>%
  glimpse()

## Save metadata, count and length files ----
setwd(staging.dir)

write.csv(metadata,paste(study,"metadata.csv",sep="_"),row.names = FALSE)
write.csv(length,paste(study,"length.csv",sep="_"),row.names = FALSE)
