# get precip data for Tucson monsoon gridding from Synoptic Labs
# adapted from synopticAPI_historic.R
# MAC 08/22/2022

library(httr)
library(jsonlite)
library(tidyr)
#library(lubridate)

# get keys
source("/home/crimmins/RProjects/StationDrought/apiKey.R")

# get mesonet ID
networks<-GET(paste0("https://api.synopticdata.com/v2/networks?token=",token))
networksRaw <- content(networks, as = "text", encoding = "UTF-8")
networksDF <- fromJSON(networksRaw,flatten = TRUE)
networkInfo<-networksDF$MNET
#thin out to key vars
networkInfo<-networkInfo[,c("ID","LONGNAME","SHORTNAME")]
#save(networkInfo, file = "./synopticData/networkNames.RData")


##### get data for bbox from synop labs ----
fullData<-list()
# set bounding box
bbox<-"-111.3482,31.8,-110.4693,32.6"

# loop through each year
year<-seq(2007,2022,1)
for (i in 1:length(year)){
  # set dates for each year
  start<-paste0(year[i],"01011200")
  end<-paste0(year[i],"12311200")
  
  # API call
  out<-GET(paste0("https://api.synopticdata.com/v2/stations/precip?bbox=",bbox,"&start=",start,"&end=",end,"&pmode=intervals&interval=24&obtimezone=local&units=english&token=",token))
  # process data from json
  rawOut <- content(out, as = "text", encoding = "UTF-8")
  dfOut <- fromJSON(rawOut, flatten = TRUE)
  # process station data
  stations<-dfOut$STATION
  # extract all obs
  allObs<-unnest(stations, OBSERVATIONS.precipitation)
  # subset inactive stations
  #allObs<-subset(allObs, STATUS=="ACTIVE")
  
  # fixed number of columns
  allObs<-allObs[,1:22]
  
  # add df to list in loop
  fullData[[i]] <- allObs
  
  print(year[i])
}

fullData <- do.call(rbind, fullData)

  # get date field
  # allObs$last_report<- as.POSIXct(allObs$last_report, format = "%Y-%m-%dT%H:%M:%S-0700")
  fullData$last_report2<-  format(as.POSIXct(fullData$last_report, format = "%Y-%m-%dT%H:%M:%S%z"), tz="America/Phoenix",usetz=TRUE)
  fullData$precipDate<-as.Date(fullData$last_report)

  # merge network info
  fullData<-merge(fullData, networkInfo,by.x="MNET_ID",by.y="ID")

save(fullData, file = paste0("./data/TucsonSynopLabs_2007_2022.RData"))