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
#i=which(dates=="2021-08-10" )
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

tucsonRain_error$errorRatio<-tucsonRain_error$pred/tucsonRain_error$precip
tucsonRain_error$errorPerc<-tucsonRain_error$error/tucsonRain_error$precip