# code to analyze gridded Rainlog data
# output from IDW_Rainlog.R
# MAC 08/19/22

library(raster)
library(dplyr)
library(ggplot2)
library(plotly)

# load data saved from IDW_Rainlog.R
# precipStack<-stack("/home/crimmins/RProjects/precipPatterns/interpOut/Tucson_Rainlog_IDW_beta3_monsoon_2007_2021.grd")
# resObsDens<-stack("/home/crimmins/RProjects/precipPatterns/interpOut/Tucson_Rainlog_densityMask.grd")
# load("/home/crimmins/RProjects/precipPatterns/interpOut/Tucson_Rainlog_IDW_tuned_monsoon_2007_2021_data.RData")

# load data saved from IDW_AllNetworks.R
# precipStack<-stack("/home/crimmins/RProjects/precipPatterns/interpOut/Tucson_All_IDW_1km_beta3_monsoon_2007_2021.grd")
# resObsDens<-stack("/home/crimmins/RProjects/precipPatterns/interpOut/Tucson_All_1km_densityMask.grd")
# load("/home/crimmins/RProjects/precipPatterns/interpOut/Tucson_All_IDW_1km_beta3_monsoon_2007_2021_data.RData")

# # load data from getTucsonPRISM.R
# precipStack2<-stack("/home/crimmins/RProjects/precipPatterns/data/Tucson_PRISM_monsoon_2007_2021.grd")
# precipStack2<-crop(precipStack2,precipStack)
# precipStack2<-resample(precipStack2,precipStack)

##### get raw station data ----
# load data from processNetworkData.R
#load("~/RProjects/precipPatterns/data/TucsonAllNetworks_2007_2021.RData")

# from IDW_outliers.R
#load("/home/crimmins/RProjects/precipPatterns/interpOut/Tucson_All_IDW_1km_beta3_allngb_errors_monsoon_2007_2021_data.RData")
#load("~/RProjects/precipPatterns/interpOut/Tucson_All_IDW_1km_beta3_5ngb_errors_monsoon_2007_2021_data.RData")
load("~/RProjects/precipPatterns/interpOut/Tucson_All_IDW_1km_beta3_5ngb_wGridErrors_monsoon_2007_2022_data.RData")

resObsDens<-stack("/home/crimmins/RProjects/precipPatterns/interpOut/Tucson_All_1km_densityMask_2007_2022.grd")

#####
# use error extremes as filter, top/bottom 5% of errors, assess sensitivity with 2/5/10 tiles

# tucsonRain_error$errRatio2<-tucsonRain_error$error/tucsonRain_error$precip
# 
# quantile(tucsonRain_error$error[tucsonRain_error$error!=0],0.98)
# quantile(tucsonRain_error$error[tucsonRain_error$error!=0],0.02)
# 
# test<-tucsonRain_error[tucsonRain_error$error!=0,]
#   summary(test$precip)  
# test<-test[test$error>-(11.6),]
#   test<-test[test$error<12.9,]
# 
# test<-subset(test, errRatio2==1)    
# ggplot(test, aes(precip,error, color=errRatio2))+
#   geom_point()+
# scale_colour_gradientn(colours=rainbow(4))

#####

# load supporting grids
# load comparison grids
prism<-stack("./gridded/Tucson_PRISM_monsoon_2007_2022.grd")
mpe<-stack("./gridded/Tucson_MRMS_monsoon_2007_2022.grd")

prism<-mask(prism, resample(resObsDens,prism))
mpe<-mask(mpe, resample(resObsDens,mpe))
# convert to mm
prism<-prism*25.4
mpe<-mpe*25.4

tucsonRain<-tucsonRain_error
rm(tucsonRain_error)
tucsonRain$errorPerc<-(tucsonRain$error/tucsonRain$precip)

# drop KDMA, problematic station
tucsonRain<-subset(tucsonRain, gaugeID!="KDMA")

# subset network if needed
#tucsonRain<-subset(tucsonRain, network!="rainlog")
#tucsonRain<-subset(tucsonRain, network=="acis")

# calc grid error
tucsonRain$errorMPE<-tucsonRain$precip-tucsonRain$MPE
tucsonRain$errorPRISM<-tucsonRain$precip-tucsonRain$PRISM
  quantile(tucsonRain$errorPRISM[tucsonRain$precip>0], c(0.05,0.5,0.95))
  #length(which(tucsonRain$errorPRISM[!is.na(tucsonRain$precip)]>=1.20))
  #length(which(tucsonRain$errorPRISM[!is.na(tucsonRain$precip)]<=-0.56))
tucsonRain$errorPRISMperc<-tucsonRain$errorPRISM/tucsonRain$precip
tucsonRain$errorMPEperc<-tucsonRain$errorMPE/tucsonRain$precip

# precip quantiles
quantile(tucsonRain$precip[tucsonRain$precip>0], c(0.05,0.1,0.25,0.5,0.95))

length(which(abs(tucsonRain$errorPerc)>1))
test<-tucsonRain[which(tucsonRain$error==tucsonRain$precip),]


# add in quality flag based on error -- predicted precip 0 based on neighbors
tucsonRain$flag<-ifelse(tucsonRain$precip>0 & tucsonRain$error==tucsonRain$precip, 1, 0) ## TEST WITH 1mm 
tucsonRain$flag2<-ifelse(tucsonRain$precip>0 & tucsonRain$errorPerc>=0.85 & tucsonRain$errorPerc<=1, 1, 0) ## TEST WITH precip>3.8mm 50th tile
#tucsonRain$flag2<-ifelse(tucsonRain$precip>0.01 & tucsonRain$errorPerc>=0.85, 1, 0)
tucsonRain$flagSum<-tucsonRain$flag+tucsonRain$flag2
hist(tucsonRain$errorPerc[tucsonRain$errorPerc>0])
length(which((tucsonRain$flagSum>0)==TRUE))

# check flag1
temp<-subset(tucsonRain, flag2>0)
hist(temp$precip, breaks=100)
summary(temp$precip)

# add in grid perc error flag
tucsonRain$flagPRISM<-ifelse(tucsonRain$precip>0 & tucsonRain$errorPRISM==tucsonRain$precip, 1, 0)
tucsonRain$flagMPE<-ifelse(tucsonRain$precip>0 & tucsonRain$errorMPE==tucsonRain$precip, 1, 0)
length(which((tucsonRain$flagPRISM==1)==TRUE))

# add in gridded data error flag
#quantile(tucsonRain$errorPRISM[tucsonRain$precip>0], c(0.05,0.5,0.95))
#tucsonRain$flagGridpos<-ifelse(tucsonRain$errorPRISM>1.20, 1,0)
#tucsonRain$flagGridneg<-ifelse(tucsonRain$errorPRISM<1.20, 1,0)
#tucsonRain$flagGrid<-ifelse(tucsonRain$errorPRISM, 1,0)


# diagnostic plots
# p<-ggplot(subset(tucsonRain,errorPerc>0), aes(errorPerc,precip, color=error, label=date))+
#   geom_point()+
#   scale_colour_gradientn(colours=rainbow(4))+
#   geom_vline(xintercept = 0.85)
# #geom_smooth()
# #geom_vline(xintercept = 1.2)
# ggplotly(p)

#####
# flag 3 - top observations with errPerc==1
# largest precip obs with 100% prediction error, predicted 0, but ob>0

bigErr<-quantile(tucsonRain$precip[tucsonRain$errorPerc>0.9],0.5, na.rm=TRUE)
  tucsonRain$flag3<-ifelse(tucsonRain$errorPerc>0.9 & tucsonRain$precip>=bigErr, 1, 0)
  
#####

# number of flags by network
table(tucsonRain[which((tucsonRain$flag+tucsonRain$flag2>0)==TRUE),]$network)/length(which((tucsonRain$flag+tucsonRain$flag2>0)==TRUE))
table(tucsonRain$network)/nrow(tucsonRain)

# subset monsoon days
subDays<-tucsonRain[tucsonRain$dummyDate >= "2020-06-01" & tucsonRain$dummyDate <= "2020-09-30", ] # extract just monsoon days
# delete rows with NA
subDays<-subDays[!is.na(subDays$precip),]
# remove above elevation
#hist(subDays$elevation, breaks=50)
subDays<-subset(subDays, elevation<=1200)
# create polygon
bounds<-rasterToPolygons(resObsDens, dissolve = TRUE)
#plot(bounds)

# clip to bounds
prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
subDaysSp <- SpatialPoints(subDays[,2:3], proj4string=CRS(prj_dd))
subDaysSp <- SpatialPointsDataFrame(subDaysSp, subDays)
# crop to point density extent
subDaysSp<-subDaysSp[complete.cases(over(subDaysSp,bounds)), ]

# extract DF
subDaysDF<-subDaysSp@data
# add in date fields
subDaysDF$month<-as.numeric(format(subDaysDF$date,"%m"))
subDaysDF$day<-as.numeric(format(subDaysDF$date,"%d"))
subDaysDF$year<-as.numeric(format(subDaysDF$date,"%Y"))
subDaysDF$doy<-as.numeric(format(subDaysDF$dummyDate,"%j"))

# hist on non zero values
hist( subDaysDF$precip[ !subDaysSp$precip==0 ])
quantile(subDaysDF$precip, c(0.9,0.5))
quantile(subDaysDF$precip[ !subDaysDF$precip==0 ], c(0.25,0.5,0.66, 0.75, 0.9, 0.95, 0.99))

# get date list from subset
dates<-as.data.frame(unique(subDays$date))
colnames(dates)<-"ymd"
dates<-dates[order(dates$ymd),]

# find a date
#i=which(dates=="2021/08/01" )
# run through a date range
#idx<-which(dates>="2021-06-15" & dates<="2021-09-30")
idx<-which(dates>="2007-06-01" & dates<="2022-09-30")

# daySp<-subset(subDaysSp, date=="2010/09/02")
#   temp<-subDaysSp@data
#   temp<-daySp@data

# load in zones
load("~/RProjects/precipPatterns/data/zones.RData")

# loop through to create more daily point metrics
  pptMetrics<-list()
  subDaysRev<-list()
  
  for(i in idx[1]:idx[length(idx)]){
    
    dayPrecip<-subset(subDaysDF, date==dates[i])
    #dayPrecip<-dayPrecip@data
    #dayPrecip<-subset(dayPrecip,flag==0) # data quality flag
    #dayPrecip<-subset(dayPrecip, flag2==0) # second flag
    #dayPrecip<-subset(dayPrecip, flagPRISM==0)
    #dayPrecip<-subset(dayPrecip, flag3==0)
   
    # check PRISM or MPE values...if all zero, set obs to zero
    flagVal<-ifelse(sum(dayPrecip$PRISM, na.rm = TRUE)==0 & sum(dayPrecip$MPE, na.rm = TRUE)==0 , 0,1)
    # drop non zero values if all zero
    #if(flagVal==0){
    #  dayPrecip<-subset(dayPrecip, precip==0)
    #}
    
    # calculate z-score
    dayPrecip<-dayPrecip %>% 
      mutate(zscore = (precip - mean(precip, na.rm=TRUE))/sd(precip, na.rm=TRUE))
    
    # add zone to obs
    dayPrecipSp <- SpatialPoints(dayPrecip[,2:3], proj4string=CRS(prj_dd))
    dayPrecipSp <- SpatialPointsDataFrame(dayPrecipSp, dayPrecip)
    #dayPrecip<-cbind.data.frame(dayPrecip, over( dayPrecipSp , zones , fn = NULL)) 
    
    # save obs to list
    subDaysRev[[i]]<-dayPrecip
    
    # set 0 to NA
    temp<-dayPrecip
    temp$precip[temp$precip==0]<-NA
    
    # outliers
    # boxplot outlier of non-zero data
    #boxplot.stats(temp$precip)$out
    #boxplot(temp$precip)
    # diff between 1/2 and 1/3
    top5<- temp %>%                                     
      arrange(desc(precip)) %>%
      #group_by(col1) %>%
      slice(1:5)
    
    # mean of top 3 with 0's
    top3<- dayPrecip %>%                                     
      arrange(desc(precip)) %>%
      #group_by(col1) %>%
      slice(1:3)
    
    mae<-sum(abs(dayPrecip$error),na.rm=TRUE)/nrow(dayPrecip)
    maePRISM<-sum(abs(dayPrecip$precip-dayPrecip$PRISM),na.rm=TRUE)/nrow(dayPrecip)
    maeMPE<-sum(abs(dayPrecip$precip-dayPrecip$MPE),na.rm=TRUE)/nrow(dayPrecip)
    
    #####
    # Moran's I on non-zero obs
    #temp<-subset(dayPrecip, precip!=0)
    temp<-dayPrecip
    gauge.dists <- as.matrix(dist(cbind(temp$lon, temp$lat)))
      gauge.dists.inv <- 1/gauge.dists
      diag(gauge.dists.inv) <- 0
    MI<-ape::Moran.I(temp$precip, gauge.dists.inv)$observed
    MIpval<-ape::Moran.I(temp$precip, gauge.dists.inv)$p.value
    #####
    
    pptMetrics[[i]]<-cbind.data.frame(dates[i],nrow(dayPrecip), max(dayPrecip$precip), 
                                    min(dayPrecip$precip),  
                                    mean(dayPrecip$precip), sd(dayPrecip$precip),
                                    median(dayPrecip$precip),mad(dayPrecip$precip),
                                    IQR(dayPrecip$precip), length(which(dayPrecip$precip==0)),
                                    length(which(dayPrecip$precip>=3.8)),
                                    length(which(dayPrecip$precip>=7.1)),
                                    length(which(dayPrecip$precip>=20.3)),
                                    length(which(dayPrecip$precip>=29.2)),
                                    length(which(dayPrecip$precip>=50.8)),
                                    mean(temp$precip, na.rm=TRUE), sd(temp$precip, na.rm = TRUE),
                                    median(temp$precip, na.rm = TRUE),mad(temp$precip, na.rm = TRUE),
                                    IQR(temp$precip, na.rm = TRUE),
                                    top5$precip[1]-top5$precip[2],
                                    top5$precip[1]-top5$precip[3],
                                    top5$precip[1]-top5$precip[5],
                                    top5$precip[1]/top5$precip[2],
                                    max(abs(dayPrecip$error), na.rm = TRUE),
                                    max(abs(dayPrecip$errorPerc), na.rm = TRUE),
                                    max(dayPrecip$error, na.rm = TRUE),
                                    max(dayPrecip$zscore, na.rm = TRUE),
                                    max(dayPrecip$errorPerc, na.rm = TRUE),
                                    mae, maePRISM, maeMPE,
                                    mean(dayPrecip$PRISM, na.rm=TRUE), sd(dayPrecip$PRISM, na.rm = TRUE),
                                    mean(dayPrecip$MPE, na.rm=TRUE), sd(dayPrecip$MPE, na.rm = TRUE),
                                    flagVal,
                                    mean(top5$precip[1:3]),
                                    mean(top3$precip[1:3]),
                                    MI,
                                    MIpval
                                                                      )
  }
  
  # combine list into df
  pptMetrics = do.call(rbind, pptMetrics)
  subDaysRev<-do.call(rbind, subDaysRev)

  # save supporting data
  colnames(pptMetrics)<-c("date","n","maxRain","minRain","meanRain","sdRain","medianRain","madRain","IQRRain","zeroRain",
                         "p50_38", "p66_71", "p90_203","p95_292","p99_508",
                         "mean_nz","sd_nz","med_nz","mad_nz","IQR_nz",
                         "top1_2","top1_3","top1_5","top1_2_ratio",
                         "maxAbsError","maxAbsErrorPerc", "maxError", "maxZscore", "maxErrorPerc", "mae", "maePRISM", "maeMPE",
                         "meanPRISM","sdPRISM", "meanMPE", "sdMPE","allZeroFlag", "top3mean", "top3mean_w0", "MI", "MIpval"
                         )
  
  pptMetrics$CV<-pptMetrics$sdRain/pptMetrics$meanRain
  pptMetrics$percZero<-pptMetrics$zeroRain/pptMetrics$n
  #pptMetrics$CV_med<-pptMetrics$IQRRain/pptMetrics$medianRain
  #pptMetrics$CV_nz<-pptMetrics$sd_nz/pptMetrics$mean_nz
  #pptMetrics$CV_med_nz<-pptMetrics$IQR_nz/pptMetrics$med_nz
  pptMetrics$nRain<-pptMetrics$n-pptMetrics$zeroRain
  pptMetrics$maxErrorPerc[pptMetrics$maxErrorPerc<0]<-NA
  
  pptMetrics$prismCV<-pptMetrics$sdPRISM/pptMetrics$meanPRISM
  pptMetrics$mpeCV<-pptMetrics$sdMPE/pptMetrics$meanMPE
  
  # add in date fiels
  pptMetrics$month<-as.numeric(format(pptMetrics$date,"%m"))
  pptMetrics$day<-as.numeric(format(pptMetrics$date, "%d"))
  pptMetrics$year<-as.numeric(format(pptMetrics$date, "%Y"))
  pptMetrics$doy<-as.numeric(format(as.Date(paste0("2020-",pptMetrics$month,"-",pptMetrics$day)), "%j"))
  
  # add mean rain day-1, day+2
  # pptMetrics$maxRain_after<-c(pptMetrics$maxRain[2:nrow(pptMetrics)],NA)
  # pptMetrics$maxRain_before<-c(NA, pptMetrics$maxRain[1:nrow(pptMetrics)-1])
  # pptMetrics$maxRain_after_diff<-pptMetrics$maxRain-pptMetrics$maxRain_after
  # pptMetrics$maxRain_before_diff<-pptMetrics$maxRain-pptMetrics$maxRain_before
  
  ##### general stats 
  # daily/seasonal stats of counts by network
  tempRev<-subDaysRev[!subDaysRev$precip==0,]
  tempRev<-subDaysRev
  
  networkStat<- tempRev %>% group_by(network, year) %>%
                summarize(n=n(),
                          medPrec=median(precip),
                          maxPrec=max(precip))
  ggplot(networkStat, aes(year, n, fill=network))+
        geom_bar(stat = 'identity')
  
  quantile(subDaysRev$precip[ !subDaysRev$precip==0 ], c(0.25,0.5,0.66, 0.75, 0.9, 0.95, 0.99))
  summary(subDaysRev$precip[ !subDaysRev$precip==0 ])
  
  ggplot(networkStat, aes(year, maxPrec, fill=network, group=network))+
    geom_bar(stat = 'identity',position = position_dodge())

  ggplot(networkStat, aes(year, medPrec, fill=network, group=network))+
    geom_bar(stat = 'identity',position = position_dodge())
  
  # rain days anywhere in network
  raindayStats<- subDaysRev %>% group_by(date) %>%
                  summarize(rainDay=sum(precip>0),
                            avgPrecip=mean(precip, na.rm=TRUE),
                            year=first(year),
                            month=first(month))
  raindayStats$rainDay<-ifelse(raindayStats$rainDay>0,1,0)
  ggplot(raindayStats, aes(year, rainDay))+
    geom_bar(stat = 'identity')
  # average seasonal rain day count
  mean(aggregate(raindayStats$rainDay, list(raindayStats$year), FUN=sum)$x)
  # average season areawide precip
  mean(aggregate(raindayStats$avgPrecip, list(raindayStats$year), FUN=sum)$x)
  
  
  
  
  #####
  
  #####
  # stats by zones
  
  # total obs by zones
  table(subDaysRev$name)
  
  # daily summaries
  library(dplyr)
  dayZones<-subDaysRev %>% group_by(date,name) %>%
                  summarize(n=n(),
                            maxPrecip=max(precip, na.rm = TRUE),
                            meanPrecip=mean(precip, na.rm = TRUE),
                            percZero=sum(precip==0, na.rm = TRUE)/n(),
                            rainDays=as.numeric(any(precip>0)),
                            days1inch=as.numeric(any(precip>=1)))
  dayZones$year<-as.numeric(format(dayZones$date,"%Y"))
  
  # summarize to annual
  annZones<-dayZones %>% group_by(name,year) %>%
                        summarize(sumPrecip=sum(meanPrecip, na.rm = TRUE),
                                  avgPercZero=mean(percZero, na.rm=TRUE),
                                  sumRainDays=sum(rainDays, na.rm = TRUE),
                                  sumDays1inch=sum(days1inch, na.rm = TRUE))
  avgZones<-annZones %>% group_by(name) %>%
                        summarize(meanPrecip=mean(sumPrecip, na.rm=TRUE),
                                  meanRainDays=mean(sumRainDays),
                                  meanDays1inch=mean(sumDays1inch)
                                  )
  
  # annual time series
  ggplot(annZones, aes(year, sumDays1inch, fill=name, group=name))+
    geom_bar(stat = 'identity', position = 'dodge')
  
  
  # average daily gauge density by zone
  gaugeDens<- dayZones %>% group_by(name) %>%
                            summarize(nAvg=mean(n, na.rm=TRUE),
                                      nMax=max(n, na.rm = TRUE),
                                      nMin=min(n, na.rm = TRUE))
  
  gaugeDens$area<-(geosphere::areaPolygon(zones)*1e-6)
  gaugeDens$densityAvg<-gaugeDens$nAvg/(geosphere::areaPolygon(zones)*1e-6)
  gaugeDens$densityMax<-gaugeDens$nMax/(geosphere::areaPolygon(zones)*1e-6)
  gaugeDens$densityMin<-gaugeDens$nMin/(geosphere::areaPolygon(zones)*1e-6)
  
  #####
  
  
  
  # ####
  # # IDW grid of subDaysRev
  #  library(gstat)
  # #
  # precipStack<-stack() # raster stack
  # 
  # prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  # #r<-raster(extent(c(-111.3482, -110.4693,31.8,32.6)),res=0.01)
  # r<-raster(extent(resObsDens),res=0.01)
  # crs(r)<-prj_dd
  # 
  # idx<-unique(subDaysRev$date)
  # 
  # # loop through days to calc grids
  # start_time <- Sys.time()
  # for(i in 1:length(idx)){
  # 
  #   dayPrecip<-subset(subDaysRev, date==dates[i])
  #   #dayPrecip<-subset(subDays, date %in% c(dates[i], dates[i+1]))
  #   daySp <- SpatialPoints(dayPrecip[,2:3], proj4string=CRS(prj_dd))
  #   daySp <- SpatialPointsDataFrame(daySp, dayPrecip)
  #   # crop to point density extent
  #   # daySp <- crop(daySp,resObsDens)
  #   dayPrecipDF<-daySp@data
  # 
  #   ##### set IDW with cross validation ----
  #   # set idw parameters
  #   beta<-3
  #   neighb<-5
  #   # IDW using gstat
  #   idwRain <- gstat(formula=precip~1, locations=daySp, nmax=neighb, set=list(idp = beta))
  #   idwRas <- interpolate(r, idwRain)
  #   #plot(idwRas)
  #   # get cross val info
  # 
  #   #dayPrecipDF$error2<-dayPrecipDF$precip-dayPrecipDF$pred
  #   ######
  # 
  #   # add to stack
  #   precipStack <- stack( precipStack, idwRas )
  #   # add to list
  # 
  #   print(dates[i])
  # }
  # end_time <- Sys.time()
  # end_time - start_time
  # 
  # names(precipStack)<-dates
  # #
  # # # write raster to file
  #  writeRaster(precipStack, filename = "/home/crimmins/RProjects/precipPatterns/interpOut/subDaysRev_IDW_1km_beta3_5ngb_errors_monsoon_2007_2022.grd", overwrite=TRUE)

  # load raster
  precipStack<-stack("/home/crimmins/RProjects/precipPatterns/interpOut/subDaysRev_IDW_1km_beta3_5ngb_errors_monsoon_2007_2022.grd")
  
  resMask<-resample(resObsDens, precipStack)
  precipStack<-mask(precipStack, resMask)
  
  
  #####
  
  ##### add in radiosonde data from read_UWsoundings.R ---- 
  load("~/RProjects/radiosonde/UWsoundings/KTUS_2007_2022_sounding_values.RData")
  
  indices<-subset(indexAll, hour=="00Z")
    #indices$date<-indices$date-1
    
  mb500<-subset(mb500all, hour=="12Z")
    mb500<-mb500[,c("date","temp500","drct500","sknt500")]  
  mb850<-subset(mb850all, hour=="12Z")
    mb850<-mb850[,c("date","temp850","drct850","sknt850")]
    
  pptMetrics<-merge(pptMetrics, indices, by="date")
  pptMetrics<-merge(pptMetrics, mb850, by="date")
  pptMetrics<-merge(pptMetrics, mb500, by="date")
      
  #####
  
  pptMetrics_CV<-pptMetrics[!is.na(pptMetrics$CV),]
  
  perc.rank <- function(x) trunc(rank(x))/length(x)
  pptMetrics_CV$rank_CV_maxPrecip<-perc.rank(pptMetrics_CV$CV*pptMetrics_CV$maxRain)
  pptMetrics_CV$extFlag<-pptMetrics_CV$rank_CV_maxPrecip>=0.95
  
  p<-ggplot(pptMetrics, aes(CV,top3mean, color=percZero, label=date))+
    geom_point()+
    scale_colour_gradientn(colours=rainbow(4))+
    geom_smooth()
    #geom_vline(xintercept = 5)+
    #geom_hline(yintercept = 50)
  ggplotly(p)
  
  ggplot(pptMetrics, aes(x=CV, y=maxRain) ) +
    #geom_bin2d() +
    #stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white")+
    #scale_fill_continuous(type = "viridis") +
    geom_density_2d(bins=100)+
    geom_point()+
    theme_bw()
  
  
    ggplot(pptMetrics, aes(sdRain,CV, color=maxRain))+
      geom_point()+
      scale_colour_gradientn(colours=rainbow(4))
    
    
  ggplot(subset(pptMetrics,p50_38>=1), aes(CV,maxRain, color=percZero))+
    geom_point()
    #geom_vline(xintercept = 1.2)
  
  # look at subsets based on thresholds
  #thresh<-subset(pptMetrics, p50_38>1)
  # thresh<-subset(pptMetrics, nRain>=10)
  # thresh<-subset(thresh, top1_2_ratio<5)
  # 
  # ggplot(thresh, aes(CV,maxRain, color=percZero))+
  #   geom_point()+
  #   scale_colour_gradientn(colours=rainbow(4))
    #geom_vline(xintercept = 1.2)
 
  # filtering and grouping ideas
  noRainDays<-subset(pptMetrics, percZero==1)
  lightDays <- pptMetrics[ which(pptMetrics$p50_38==0 & pptMetrics$percZero<1), ]
  
  # obs that are single nrain ob of day
  #subMetrics<-subset(pptMetrics, nRain>=3 & p50_38>=1)
  #subMetrics<-subset(subMetrics, p50_38>=1)
  #subMetrics<-subset(subMetrics, maxError<=2)
  subMetrics<-subset(pptMetrics, nRain>=3)

  # check to make sure all days are counted 
  nrow(noRainDays)+nrow(lightDays)+nrow(subMetrics)==nrow(pptMetrics)
  
  # stats on CV
  quantile(subMetrics$CV, c(0.5,0.9,0.95))
  #subMetrics<-subset(subMetrics, CV<=quantile(subMetrics$CV, c(0.9)))
  
 p<-ggplot(subMetrics, aes(CV,top3mean, color=percZero, label=date))+
    geom_point()+
    scale_colour_gradientn(colours=rainbow(4))
 ggplotly(p) 
 
  ggplot(subMetrics, aes(CV,maxRain, color=maxZscore))+
    geom_point()+
    scale_colour_gradientn(colours=rainbow(4))
    #geom_smooth()
    #geom_hline(yintercept = 0.15)+
    geom_hline(yintercept = 0.82)+
    geom_hline(yintercept = 2)+
    geom_vline(xintercept = 2.62)
   
    
    # explore day
    subMetricsObs<-subset(subDaysRev, date %in% unique(subMetrics$date))
    #subMetricsObs<<-subset(subDaysRev, date %in% unique(pptMetrics$date))
    
    dt<-"2018-09-03"
    #dt<-dates[1527]
    #plot(precipStack[[which(layerInfo==dt)]])
    #temp1<-subset(subDaysRev, date==dt)
    #temp1<-subset(subDaysDF, date==dt)
    temp1<-subset(subMetricsObs, date==dt)
    # delete 0's
    temp1<-subset(temp1, precip!=0)
    
    #temp<-subset(temp, flag==0)
    #temp<-subset(temp, flag2==0)
    #boxplot(subset(temp,precip!=0)$precip)
    
    
    plot(prism[[which(dates==dt )]])
    plot(mpe[[which(dates==dt )]])
    
    # station labs
    labs <- lapply(seq(nrow(temp1)), function(i) {
      paste0( '<p> <b> Station name:', temp1[i, "gaugeID"], '</b></p>', 
              '<p> Network Name:', temp1[i, "network"], '</p>',
              '<p> <b> <font color="green"> Precip (mm):', temp1[i, "precip"], '</b></font></p>',
              '<p> <font color="red"> Error:', temp1[i, "error"], '</font></p>',
              '<p> <font color="red"> Error %:', temp1[i, "errorPerc"], '</font></p>') 
    })
    
    library(leaflet)
    pal <- colorNumeric(
      palette = grDevices::colorRampPalette(c('lightblue','blue','green','yellow','orange','red'))(length(temp1$precip)), 
      domain = temp1$precip)
    
    leaflet() %>% addProviderTiles(providers$Esri.WorldTopoMap) %>%
      setView(-110.909479, 32.221551, zoom = 10) %>%
      addCircleMarkers(temp1$lon, temp1$lat,
                       radius = 3,
                       color =  pal(temp1$precip),
                       label = lapply(labs, htmltools::HTML))
     
    
#####

##### climo analysis of obs and metrics ----
    
# use both censored and uncensored data
    summary(subDaysSp$precip[ !subDaysSp$precip==0 ]) 
    summary(subDaysRev$precip[ !subDaysRev$precip==0 ])
    # further filtered data
    subMetricsObs<-subset(subDaysRev, date %in% unique(subMetrics$date))
    
    # number of dry days
    length(which(pptMetrics$percZero==1))/nrow(pptMetrics)
    
    # heat map of daily metrics
    ggplot(pptMetrics, aes(doy,year, fill=CV))+
      geom_tile()+
      scale_fill_distiller(palette = "YlGnBu")+
      theme_bw()
      
    # daily boxplots
    ggplot(subDaysRev, aes(dummyDate,precip, group=dummyDate))+
      geom_boxplot()
  
    # stats on CV
  
    library("PerformanceAnalytics")
    my_data <- subMetrics[, c("n","maxRain","meanRain","sdRain","CV","percZero","nRain")]
    chart.Correlation(my_data, histogram=TRUE, pch=19)    
    
    summary(pptMetrics$CV)
    summary(subMetrics$CV)
    # swap versions of daily summaries
    temp<-subMetrics
    #temp<-pptMetrics
    # 
    qCV<-quantile(temp$CV, c(0.33,0.66), na.rm=TRUE)
    #qMx<-quantile(temp$maxRain, c(0.33,0.66))
    qMx<-quantile(temp$top3mean, c(0.33,0.66), na.rm=TRUE)
    
    p<-ggplot(temp, aes(CV,top3mean, color=nRain, label=date))+
      geom_point()+
      scale_colour_gradientn(colours=rainbow(4))+
      geom_vline(xintercept = qCV[1])+
      geom_vline(xintercept = qCV[2])+
      geom_hline(yintercept = qMx[1])+
      geom_hline(yintercept = qMx[2])
    ggplotly(p) 
    
    # categories
    temp$cvGroup<-cut(temp$CV, breaks=c(0,qCV[1],qCV[2],Inf),
                      labels=c('widespread','scattered','isolated'))
    temp$maxRainGroup<-cut(temp$top3mean, breaks=c(0,qMx[1],qMx[2],Inf),
                      labels=c('light','moderate','heavy'))
    temp$groups<-paste0(temp$cvGroup,"-",temp$maxRainGroup)
    groupCounts<-as.data.frame(table(temp$groups))
    
    table(temp$cvGroup)
    table(temp$maxRainGroup)
    # category statistics
    subMetricsObs<-merge(subMetricsObs, temp[,c("date","groups")], by="date")
    
    groupDayStats<- subMetricsObs %>% group_by(groups) %>%
                    summarise(n=n(),
                              meanPrecip=mean(precip, na.rm=TRUE),
                              sdPrecip=sd(precip, na.rm = TRUE),
                              maxPrecip=max(precip, na.rm = TRUE),
                              zeroCount=sum(precip==0),
                              gt1Count=sum(precip>=1)
                              )
    
    groupStats<- temp %>% group_by(groups) %>%
                  summarise(nDays=n(),
                            meanCV=mean(CV, na.rm=TRUE),
                            meanMaxRain=mean(maxRain, na.rm=TRUE),
                            meanPercZero=mean(percZero, na.rm=TRUE)
                            )
    
    # climo stats of groups
    groupCounts$percent<-groupCounts$Freq/sum(groupCounts$Freq)
    groupCounts$percFull<-groupCounts$Freq/nrow(pptMetrics)
    # yearly stats
    groupYear<- temp %>% group_by(year, groups) %>%
                    summarise(n=n())
    ggplot(groupYear, aes(year,n,fill=groups))+
      geom_bar(stat = 'identity')
    
    
    # groups vs zones
    table(subMetricsObs$name, subMetricsObs$groups)
    
    # remove zeros
    temp3<-subset(subMetricsObs, precip>0)
    
    zoneGroups<-temp3 %>% group_by(date, groups, name) %>%
                                  summarise(n=n())
    table(zoneGroups$groups, zoneGroups$name)
    
#####    
    

#####  analyze grids metrics ----

# get area and gauge density
# https://caucasus-spiders.info/r-spatial/raster-basics-3/
    #get sizes of all cells in raster [km2]
    cell_size<-area(resObsDens, na.rm=TRUE, weights=FALSE)
    #delete NAs from vector of all raster cells
    ##NAs lie outside of the rastered region, can thus be omitted
    cell_size<-cell_size[!is.na(cell_size)]
    #compute area [km2] of all cells in geo_raster
    raster_area<-length(cell_size)*median(cell_size)
    #print area of Georgia according to raster object
    print(paste("Area of mask (raster):",round(raster_area, digits=1),"km2"))
    hist(layerInfo$n/raster_area) # hist of gauge density per day
    
  # test<- spatstat::ppp(daySp@coords[,1],daySp@coords[,1],
  #                      c(min(daySp@coords[,1]),max(daySp@coords[,1])),
  #                      c(min(daySp@coords[,2]),max(daySp@coords[,2])))
    
# apply mask to precip grids
precipStack<-mask(precipStack, resObsDens)
precipStack2<-mask(precipStack2, resObsDens)

# look at histogram of values
temp<-values(precipStack)
hist( temp[temp < 0.05] )

# grid stats
plot(max(precipStack, na.rm = TRUE))
plot(max(precipStack2))

#count wet days
b.wet<-reclassify(precipStack,c(-Inf,0.15,0))
b.wet<-reclassify(b.wet,c(0.15,+Inf,1))
#plot(b.wet[[100]])
#plot(sum(b.wet), zlim=c(300,700))
hist(sum(b.wet, na.rm = TRUE))
plot(sum(b.wet, na.rm = TRUE),zlim=c(100,250))

test<-values(sum(b.wet, na.rm = TRUE))
tempGrid<-sum(b.wet)

# annual mean
annSum<-stackApply(precipStack, as.numeric(format(dates, "%Y")), fun=sum)
annSum[annSum <= 0] <- NA
annSum<-trim(annSum)
rasterVis::levelplot(annSum, par.settings=rasterTheme(viridis::viridis_pal(option = "D")(255)),
                     margin=FALSE, main="Precip Zones")
meanPrecip<-calc(annSum, mean)
rasterVis::levelplot(meanPrecip, par.settings=rasterTheme(viridis::viridis_pal(option = "D")(255)),
                     margin=FALSE, main="Precip Zones")
cellStats(meanPrecip, mean)


#####
# kmean clustering on days

library(RStoolbox)
#library(tidyr)
#library(dplyr)
library(parallel)
library(rasterVis)
library(RColorBrewer)

# get finer res zones
highRes <- raster(resolution=c(0.005,0.005), crs=proj4string(precipStack), ext=extent(precipStack))
resPrecip<-resample(precipStack, highRes)
#resPrecip<-precipStack
#LatLon <- stack(init(resPrecip[[1]], 'y'),init(resPrecip[[1]], 'x'))
#resPrecip<-stack(LatLon,resPrecip)

rasterOptions(progress="text")
rsOpts(verbose=TRUE)

# cluster data
ptm <- proc.time()
  set.seed(1234)
  clusterN<-8
  unC <- unsuperClass(resPrecip, nSamples = 5000000, nClasses = clusterN, nStarts = 25, nIter = 1000, norm = FALSE) # or allGDD
proc.time() - ptm

resMask<-resample(resObsDens, unC$map)
classMap<-mask(unC$map, resMask)
# get polygons
#zones<-rasterToPolygons(classMap, dissolve = TRUE)

# smooth regions https://gis.stackexchange.com/questions/152517/smoothing-raster-map-using-r
#y <- disaggregate(, 5)
#y <- focal(unC$map, w=matrix(1, 5, 5), max, na.rm=TRUE)
#y <- focal(unC$map, w=focalWeight(unC$map, 0.015, "circle", fillNA=FALSE), max, na.rm=TRUE)
#resMask<-resample(resObsDens, y)
#test<-mask(y, resMask)
#test <- focal(test, w=matrix(1, 5, 5), min, na.rm=TRUE)
#test<-mask(y, resMask)

# test<-rasterToPolygons(test, dissolve = TRUE)
# test2<-rasterToPolygons(unC$map, dissolve = TRUE)
# plot(test)
# plot(test2, add=TRUE)

# random colors -- https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
darkcols<-sample(col_vector, clusterN)

classMap<-as.factor(classMap)
rat <- levels(classMap)[[1]]
# cluster names
rat[["cluster"]]<-rev(c("Eastside","Oro Valley","Rita Ranch","Gates Pass",
                        "Twin Peaks","Saddlebrook","Foothills","Central"))

#rat[["cluster"]] <- as.character(seq(1, clusterN, by=1))
levels(classMap) <- rat 

# create polygon
zones<-rasterToPolygons(classMap, dissolve = TRUE)
zones$name<-rev(c("Eastside","Oro Valley","Rita Ranch","Gates Pass",
                  "Twin Peaks","Saddlebrook","Foothills","Central"))
save(zones, file="~/RProjects/precipPatterns/data/zones.RData")

# plot classified map
rasterVis::levelplot(classMap, col.regions=darkcols, par.settings=list(panel.background=list(col="white")),
          margin=FALSE, main="Precip Zones")

# leaflet map
library(leaflet)
pal <- colorNumeric(darkcols, values(classMap),
                    na.color = "transparent")

leaflet() %>% addTiles() %>%
  #addRasterImage(classMap, colors = pal, opacity = 0.3) %>%
  addPolygons(data=zones, color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.1,
              fillColor = NA) %>%
  addLegend(pal = pal, values = values(classMap),
            title = "Precip Zones")





#####


#####

##### analyze layerInfo metrics -----
library(ggplot2)

layerInfo$CV<-layerInfo$sdRain/layerInfo$meanRain
layerInfo$percZero<-layerInfo$zeroRain/layerInfo$n
layerInfo$CV_med<-layerInfo$IQRRain/layerInfo$medianRain

ggplot(layerInfo, aes(CV,maxRain, color=CV_med))+
  geom_point()+
  geom_vline(xintercept = 0.5)

plot(precipStack[[which(layerInfo=="2010/09/02")]])

#####


###### mapping for checking grids ----
library(ggmap)
library(scales)
library(viridis)
# API key
source('~/RProjects/RainlogAPI/APIkey.R')
# get map and bounding box
where<-geocode("tucson", source = "google")
where$lat=32.221551; where$lon=-110.909479 # center of download from getTucsonRainObs_listAppend.R

TucsonMap <- qmap(location = c(lon=where$lon,lat=where$lat), zoom = 10,
                  color = "bw")


nn <- as(tempGrid, "SpatialPixelsDataFrame")
nn <- as.data.frame(nn)
colnames(nn) <- c("value", "x", "y")

# colorramp for total precip
# precipCols<-colorRampPalette(c("lightblue", "dodgerblue3", "palegreen","green4","salmon","orangered3",
#                                "lightgoldenrod1","orange2","plum2","purple"))(50)
# precBreaks<-seq(0,6,0.5)
# precLabs<-as.character(seq(0,6,0.5))
# precLabs[13]<-">6"
# precLabs[1]<-"0.01"


TucsonMap +  
  geom_tile(data=nn, aes(x=x, y=y, fill=value), alpha=0.8) + 
  #geom_point(data=dayPrecip, aes(x=position.lng ,y=position.lat, fill=rainAmount),size=1.5, shape=21)+
  #scale_color_viridis()+
  scale_fill_viridis() 
  #coord_equal() +
  #theme_map() +
  # scale_fill_gradientn(colours = precipCols, na.value="burlywood4", 
  #                      name="inches", limits=c(0,6),oob=squish, breaks=precBreaks, labels=precLabs, expand=NULL)+
  # guides(fill= guide_colorbar(barwidth=20,nbin = 500, raster = FALSE))+
  # theme(legend.position="bottom")+
  # ggtitle("IDW RainLog - 8/10/22")




