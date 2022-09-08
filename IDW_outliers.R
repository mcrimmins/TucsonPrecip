# use cross validated interpolation to id outliers
# adapted from points_AllNetworks.R
# MAC 08/27/22

library(raster)
library(gstat)
#library(viridis)
#library(scales)

# # functions
# RMSE <- function(residuals){
#   sqrt(sum((residuals)^2)/length(residuals))
# }

#source('cvIDW.R')

# load data from processNetworkData.R
load("~/RProjects/precipPatterns/data/TucsonAllNetworks_2007_2021.RData")

# subset monsoon days
subDays<-tucsonRain[tucsonRain$dummyDate >= "2020-06-01" & tucsonRain$dummyDate <= "2020-09-30", ] # extract just monsoon days
rm(tucsonRain)

# delete rows with NA
subDays<-subDays[!is.na(subDays$precip),]

# remove above elevation
hist(subDays$elevation, breaks=50)
subDays<-subset(subDays, elevation<=1200)

# get date list from subset
dates<-as.data.frame(unique(subDays$date))
colnames(dates)<-"ymd"
dates<-dates[order(dates$ymd),]

# find a date
#i=which(dates=="2017-08-11" )
# run through a date range
#idx<-which(dates>="2021-06-15" & dates<="2021-09-30")
idx<-which(dates>="2007-06-15" & dates<="2021-09-30")

# loop through days
# prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
precipStack<-stack() # raster stack
precip_error<-list() # list of layer info

prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
r<-raster(extent(c(-111.3482, -110.4693,31.8,32.6)),res=0.01)
crs(r)<-prj_dd


# loop through days to calc grids
start_time <- Sys.time()
for(i in idx[1]:idx[length(idx)]){
  
  dayPrecip<-subset(subDays, date==dates[i])
  #dayPrecip<-subset(subDays, date %in% c(dates[i], dates[i+1]))
  daySp <- SpatialPoints(dayPrecip[,2:3], proj4string=CRS(prj_dd))
  daySp <- SpatialPointsDataFrame(daySp, dayPrecip)
  # crop to point density extent
  # daySp <- crop(daySp,resObsDens)
  dayPrecipDF<-daySp@data
  
  ##### set IDW with cross validation ----
  # set idw parameters
  beta<-3
  neighb<-5
  # IDW using gstat
  idwRain <- gstat(formula=precip~1, locations=daySp, nmax=neighb, set=list(idp = beta))
  idwRas <- interpolate(r, idwRain)
  #plot(idwRas)
  # get cross val info
  crossval <- gstat.cv(idwRain)
  crossval <- crossval@data
  dayPrecipDF$error<-crossval$residual
  dayPrecipDF$pred<-crossval$var1.pred
  dayPrecipDF$errRatio<-dayPrecipDF$pred/dayPrecipDF$precip
  #dayPrecipDF$error2<-dayPrecipDF$precip-dayPrecipDF$pred
  ######
  
  # add to stack
  precipStack <- stack( precipStack, idwRas )
  # add to list
  precip_error[[i]]<-dayPrecipDF
  
  print(dates[i])
}
end_time <- Sys.time()
end_time - start_time

# combine list into df
tucsonRain_error = do.call(rbind, precip_error)

# write raster to file
writeRaster(precipStack, filename = "/home/crimmins/RProjects/precipPatterns/interpOut/Tucson_All_IDW_1km_beta3_5ngb_errors_monsoon_2007_2021.grd", overwrite=TRUE)
# save supporting data
save(tucsonRain_error, file = "/home/crimmins/RProjects/precipPatterns/interpOut/Tucson_All_IDW_1km_beta3_5ngb_errors_monsoon_2007_2021_data.RData")

#tucsonRain_error$errorRatio<-tucsonRain_error$pred/tucsonRain_error$precip
#tucsonRain_error$errorPerc<-tucsonRain_error$error/tucsonRain_error$precip

# calculate day before/after error 

load("~/RProjects/precipPatterns/interpOut/Tucson_All_IDW_1km_beta3_5ngb_errors_monsoon_2007_2021_data.RData")

# perc rank function
perc.rank <- function(x) trunc(rank(x))/length(x)

quantile(tucsonRain_error$precip[ !tucsonRain_error$precip==0 ], c(0.25,0.5,0.66, 0.75, 0.9, 0.95, 0.99))

errorDays<-subset(tucsonRain_error, error>=0.82)
#errorDays$predBefore<-NA
#errorDays$predAfter<-NA

errorList<-list()

# loop through days to calc grids
start_time <- Sys.time()
for(i in 1442:nrow(errorDays)){
  
  shift<-c(-1,1)
  shiftError<-list()
  for(j in 1:2){
    # day before/after error
    dateShift<-errorDays$date[i]+shift[j]
      if(dateShift!=as.Date(paste0(format(dateShift,"%Y"),"-10-01"))){
      dayPrecip<-subset(subDays, date==dateShift)
      # find duplicate gauge and drop if present
      dup<-which(dayPrecip$gaugeID==errorDays$gaugeID[i])
      if(length(dup)!=0){
        dayPrecip<-dayPrecip[-dup,]
      }
      # bind out day to DF
      dayPrecip<-rbind.data.frame(dayPrecip,errorDays[i,1:9])
      # percent rank
      dayPrecip$rank<-perc.rank(dayPrecip$precip)
      # calculate z-score
      dayPrecip<-dayPrecip %>% 
        mutate(zscore = (precip - mean(precip, na.rm=TRUE))/sd(precip, na.rm=TRUE))
      
      
      # convert to spatial
      daySp <- SpatialPoints(dayPrecip[,2:3], proj4string=CRS(prj_dd))
      daySp <- SpatialPointsDataFrame(daySp, dayPrecip)
      # crop to point density extent
      # daySp <- crop(daySp,resObsDens)
      dayPrecipDF<-daySp@data
      
      ##### set IDW with cross validation ----
      # set idw parameters
      beta<-3
      neighb<-5
      # IDW using gstat
      idwRain <- gstat(formula=precip~1, locations=daySp, nmax=neighb, set=list(idp = beta))
      idwRas <- interpolate(r, idwRain)
      #plot(idwRas)
      # get cross val info
      crossval <- gstat.cv(idwRain)
      crossval <- crossval@data
      dayPrecipDF$error<-crossval$residual
      dayPrecipDF$pred<-crossval$var1.pred
      dayPrecipDF$errRatio<-dayPrecipDF$pred/dayPrecipDF$precip
      #dayPrecipDF$error2<-dayPrecipDF$precip-dayPrecipDF$pred
      # get error for gauge
      shiftError[[j]]<-dayPrecipDF$error[which(dayPrecipDF$gaugeID==errorDays$gaugeID[i] & dayPrecipDF$date==errorDays$date[i])]
      }else{
        shiftError[[j]]<-NA
      }
    }
  
  errorList[[i]]<-cbind.data.frame(shiftError[[1]],shiftError[[2]])
  
  #errorDays$predBefore[i]<-shiftError[[1]]
  #errorDays$predAfter[i]<-shiftError[[2]]
  
  ######
  
  print(round(i/nrow(errorDays),2))
}
end_time <- Sys.time()
end_time - start_time


errorList<-do.call(rbind, errorList)
colnames(errorList)<-c("errorBefore","errorAfter")

errorDays$lowestDay<-as.matrix(apply(errorDays[,c(12,10,13)],1,which.min))

# save supporting data
save(errorDays, file = "/home/crimmins/RProjects/precipPatterns/interpOut/Tucson_monsoon_day_before_after_errors_gt118_data.RData")



###### WRONG DAY OB Diagnostic ----
###### day before/after quantile and z score ----

library(dplyr)

load("~/RProjects/precipPatterns/interpOut/Tucson_All_IDW_1km_beta3_5ngb_errors_monsoon_2007_2021_data.RData")

# perc rank function
perc.rank <- function(x) trunc(rank(x))/length(x)

quantile(tucsonRain_error$precip[ !tucsonRain_error$precip==0 ], c(0.25,0.5,0.66, 0.75, 0.9, 0.95, 0.99))

errorDays<-subset(tucsonRain_error, error>0)
#errorDays$predBefore<-NA
#errorDays$predAfter<-NA

# find any duplicate gauge/day instances
tucsonRain_error<-tucsonRain_error[-which(duplicated(tucsonRain_error[,c("gaugeID","date")])==TRUE),]

#create empty list
errorList<-list()

#test<-errorDays[i,]

# loop through days to calc grids
start_time <- Sys.time()
for(i in 1:nrow(errorDays)){
  
  # day of ranks/z score
  dayPrecip<-subset(tucsonRain_error, date==errorDays$date[i])
  # percent rank
  dayPrecip$rank<-perc.rank(dayPrecip$precip)
  # calculate z-score
  dayPrecip<-dayPrecip %>% 
    mutate(zscore = (precip - mean(precip, na.rm=TRUE))/sd(precip, na.rm=TRUE))
  # grab quant and zscore
  dayOf<-dayPrecip[which(errorDays$gaugeID[i]==dayPrecip$gaugeID),c(12,13)]
  
  shift<-c(-1,1)
  shiftError<-list()
  for(j in 1:2){
    # day before/after error
    dateShift<-errorDays$date[i]+shift[j]
    if(dateShift!=as.Date(paste0(format(dateShift,"%Y"),"-10-01")) | dateShift!=as.Date(paste0(format(dateShift,"%Y"),"-06-15"))){
      dayPrecip<-subset(tucsonRain_error, date==dateShift)
      # find duplicate gauge and drop if present
      dup<-which(dayPrecip$gaugeID==errorDays$gaugeID[i])
      if(length(dup)!=0){
        dayPrecip<-dayPrecip[-dup,]
      }
      # bind out day to DF
      dayPrecip<-rbind.data.frame(dayPrecip,errorDays[i,])
      # percent rank
      dayPrecip$rank<-perc.rank(dayPrecip$precip)
      # calculate z-score
      dayPrecip<-dayPrecip %>% 
        mutate(zscore = (precip - mean(precip, na.rm=TRUE))/sd(precip, na.rm=TRUE))
      # get error for gauge
      shiftError[[j]]<-dayPrecip[which(errorDays$gaugeID[i]==dayPrecip$gaugeID),c(12,13)]
      #shiftError[[j]]<-dayPrecipDF$error[which(dayPrecipDF$gaugeID==errorDays$gaugeID[i] & dayPrecipDF$date==errorDays$date[i])]
    }else{
      shiftError[[j]]<-NA
    }
  }
  
  errorList[[i]]<-cbind.data.frame(errorDays$gaugeID[i],errorDays$date[i],errorDays$precip[i],
                                   dayOf$rank,shiftError[[1]]$rank,shiftError[[2]]$rank,
                                   dayOf$zscore,shiftError[[1]]$zscore,shiftError[[2]]$zscore)
  
  #errorDays$predBefore[i]<-shiftError[[1]]
  #errorDays$predAfter[i]<-shiftError[[2]]
  
  ######
  
  print(round(i/nrow(errorDays),2))
}
end_time <- Sys.time()
end_time - start_time

tempList<-errorList
#errorList<-tempList

errorList<-do.call(rbind, errorList)

errorList$lowRankDay<-as.matrix(apply(abs(errorList[,c(4,5,6)]),1,which.min))
errorList$lowZDay<-as.matrix(apply(abs(errorList[,c(7,8,9)]),1,which.min))
#errorList<-cbind.data.frame(errorDays$gaugeID,errorDays$date,errorDays$precip,errorList)

colnames(errorList)<-c("gaugeID","date","precip","rankDay","rankBefore","rankAfter","zDay","zBefore","zAfter","lowRankDay","lowZDay")

# look for shifts
temp<-list()  
for(i in 1:nrow(errorList)){
  idx<-errorList$lowZDay[i]+6
  temp[[i]]<-abs(errorList[i,idx])/abs(errorList$zDay[i])
}  
errorList$zRatio<-do.call(rbind, temp)
  
  
# save supporting data
save(errorList, file = "/home/crimmins/RProjects/precipPatterns/interpOut/Tucson_monsoon_day_before_after_ranks_gt0_data.RData")






