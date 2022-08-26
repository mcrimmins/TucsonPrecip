# IDW interpolation - Tucson Rainlog Data
# MAC 08/16/22

#library(ggmap)
library(dplyr)
library(ggplot2)
library(raster)
library(gstat)
#library(viridis)
#library(scales)

# functions
RMSE <- function(residuals){
  sqrt(sum((residuals)^2)/length(residuals))
}

source('cvIDW.R')


#####
# load and clean Rainlog data
  load("./data/TucsonRainlogObs_2007_2021_allQuality.RData") # from getTucsonRainObs_listAppend.R
  #load("~/RProjects/RainlogAPI/manuscript/TIA_COOP.RData")
  load("TucsonGauges_Elevations.RData") # getElevs.R
  
  # add elevations to gauges
  elevs<-LatLons_Elevs@data
  gaugeStack<-merge(gaugeStack,elevs, by="gaugeRevisionId", all.x = TRUE)
  
  #
  # data processing from Tucson_Rainlog_Analysis.R
  ###
  # join data frames
  mergedData <- merge(dataStack,gaugeStack,by="gaugeRevisionId", all.x = TRUE)
  # fix dates
  mergedData$readingDate<-as.Date(mergedData$readingDate)-1
  
  #test<-mergedData[which(mergedData$readingDate=="2012-07-15"),]
  
  # add in year month
  mergedData$yearMonth<-as.Date(paste0(format(mergedData$readingDate, "%m"),
                                       "-","01-",format(mergedData$readingDate,"%Y")),format = "%m-%d-%Y")
  mergedData$year<-as.numeric(format(mergedData$readingDate, "%Y"))
  mergedData$month<-as.numeric(format(mergedData$readingDate, "%m"))
  mergedData$season<-as.factor(ifelse(mergedData$month>4 & mergedData$month<11, "May-Oct", "Nov-Apr"))
  mergedData$dummyDate<-as.Date(paste0("2020-",mergedData$month,"-",format(mergedData$readingDate,"%d")))
  preClean<-nrow(mergedData)
  
  ## ----- DATA CLEANING -----
  # scrub out extremes >5.24" TIA 1000 year 24 hr total
  # https://hdsc.nws.noaa.gov/hdsc/pfds/pfds_map_cont.html?bkmrk=az
  mergedData <- mergedData[-which(mergedData$rainAmount>5.24),]
  
  # eliminate Summerhaven gauge obs, 10565
  #mergedData<-mergedData[-which(mergedData$gaugeId.x==10565),] # Summerhaven gauge
  #mergedData<-mergedData[-which(mergedData$gaugeId.x==630),] # Mt Kimball gauge
  
  # remove NA in lng/lat
  mergedData<-mergedData[!is.na(mergedData$position.lng),]
  
  # find some key words in remarks col - looking for cumulative totals
  mergedData<-mergedData[-which(grepl("cumulative", mergedData$remarks, ignore.case = TRUE)==TRUE),]
  mergedData<-mergedData[-which(grepl("accumulated", mergedData$remarks, ignore.case = TRUE)==TRUE),]
  mergedData<-mergedData[-which(grepl("gone", mergedData$remarks, ignore.case = TRUE)==TRUE),]
  mergedData<-mergedData[-which(grepl("out of town", mergedData$remarks, ignore.case = TRUE)==TRUE),]
  mergedData<-mergedData[-which(grepl("vacation", mergedData$remarks, ignore.case = TRUE)==TRUE),]
  mergedData<-mergedData[-which(grepl("trip", mergedData$remarks, ignore.case = TRUE)==TRUE),]
  mergedData<-mergedData[-which(grepl("total", mergedData$remarks, ignore.case = TRUE)==TRUE),]
  # keep only Good and Trace
  mergedData<-mergedData[mergedData$quality %in% c('Good','Trace'),]
  mergedData$rainAmount<-ifelse(mergedData$quality=="Trace",0.001,mergedData$rainAmount)
  # remove if still missing rainAmount
  mergedData<-mergedData[!is.na(mergedData$rainAmount),]
  
  postClean<-nrow(mergedData)
  cleanedRecs<-preClean-postClean
# ----- END CLEANING
#####
  
##### point density masking ----  
  library(pointdensityP)
  # more info at https://rpubs.com/msgc/point_pattern_analysis
  # create raster
  #r<-raster(daySp,res=0.01)
  prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  r<-raster(extent(c(-111.3482, -110.4693,31.8,32.6)),res=0.01)
  crs(r)<-prj_dd
  
  
  temp<-mergedData[c("readingDate","position.lat","position.lng")]
  temp<-temp[complete.cases(temp), ]
  
  reading_density <- pointdensity(df = temp, lat_col = "position.lat", lon_col = "position.lng",
                                  date_col = "readingDate", grid_size = 5, radius = 10)
  
  reading_density <- reading_density[!duplicated(reading_density[ , c("lat", "lon")]), ]
  
  coordinates(reading_density) <- ~ lon + lat # Convert data frame to spatial object
  obsDensity <- rasterize(reading_density, r, "count", update = TRUE) # put point in raster
  
  resObsDens<-aggregate(obsDensity,5, fun=mean)
  plot(resObsDens)
  resObsDens[resObsDens < 45000] <- NA
  plot(resObsDens)
  resObsDens<-trim(resObsDens)
  plot(resObsDens)
  # resample to fine grid
  r<-raster(extent(resObsDens),res=0.005)
  crs(r)<-prj_dd
  resObsDens<-resample(resObsDens,r)
  plot(resObsDens)
  #nn<-mask(nn, resObsDens)
#####  
  
##### subset data for monsoon season ----
  
  subDays<-mergedData[mergedData$dummyDate >= "2020-06-15" & mergedData$dummyDate <= "2020-09-30", ] # extract just monsoon days
  # get date list from subset
  dates<-as.data.frame(unique(subDays$readingDate))
  colnames(dates)<-"ymd"
  dates<-dates[order(dates$ymd),]
  
  # records by date
  recsDate<- subDays %>% group_by(readingDate) %>%
    tally()
  
  
  # find a date
  #i=which(dates=="2021-08-10" )
  # run through a date range
  #idx<-which(dates>="2021-06-15" & dates<="2021-09-30")
  idx<-which(dates>="2007-06-15" & dates<="2021-09-30")
  
  # loop through days
  prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  precipStack<-stack() # raster stack
  infoList<-list() # list of layer info
  
  # loop through days to calc grids
  start_time <- Sys.time()
  for(i in idx[1]:idx[length(idx)]){
    
    dayPrecip<-subset(subDays, readingDate==dates[i])
    daySp <- SpatialPoints(dayPrecip[,20:19], proj4string=CRS(prj_dd))
    daySp <- SpatialPointsDataFrame(daySp, dayPrecip)
    # crop to point density extent
    daySp <- crop(daySp,resObsDens)
    
    # ##### set IDW with cross validation ----
    # # set idw parameters
    # beta<-3
    # neighb<-nrow(dayPrecip)-1
    # # IDW using gstat
    # idwRain <- gstat(formula=rainAmount~1, locations=daySp, nmax=neighb, set=list(idp = beta))
    # idwRas <- interpolate(resObsDens, idwRain)
    # #plot(idwRas)
    # # get cross val info
    # crossval <- gstat.cv(idwRain)
    # crossval_rmse <- RMSE(crossval$residual)
    # # save info in dataframe
    # infoList[[i]]<-cbind.data.frame(dates[i],nrow(dayPrecip), max(dayPrecip$rainAmount), median(dayPrecip$rainAmount),crossval_rmse, beta, neighb)
    # #####
    
    ##### IDW with parameter tuning ----
    tuneIDW <- cv.IDW(spatialDF = daySp,
                     stat.formula = formula(rainAmount ~ 1),
                     evalGridSize = res(r)[1],
                     seqNeighbors = c(5,25,50,nrow(dayPrecip)-1),
                     #seqBeta = seq(from = 1, to = 3, 1),
                     seqBeta = c(2,3),
                     evalRaster = r,
                     verbose = TRUE)
    # save info in dataframe
    infoList[[i]]<-cbind.data.frame(dates[i],nrow(dayPrecip), max(dayPrecip$rainAmount), 
                                    mean(dayPrecip$rainAmount), sd(dayPrecip$rainAmount),
                                    median(dayPrecip$rainAmount),mad(dayPrecip$rainAmount),
                                    IQR(dayPrecip$rainAmount), length(which(dayPrecip$rainAmount==0)),
                                    tuneIDW$bestRMSE, tuneIDW$bestBeta, tuneIDW$bestNeighbors)
    idwRas<-raster(tuneIDW$idwBestRaster, layer=1, values=TRUE)
    idwRas<-resample(idwRas, resObsDens)
    ######
    
    # add to stack
    precipStack <- stack( precipStack, idwRas )
    print(dates[i])
  }
  end_time <- Sys.time()
  end_time - start_time
  
  # add names to layers in stack
  names(precipStack)<-dates[idx[1]:idx[length(idx)]]
  # combine list into df
  layerInfo = do.call(rbind, infoList)
  
  # write raster to file
  writeRaster(precipStack, filename = "/home/crimmins/RProjects/precipPatterns/interpOut/Tucson_Rainlog_IDW_beta23_monsoon_2007_2021.grd", overwrite=TRUE)
  writeRaster(resObsDens, filename = "/home/crimmins/RProjects/precipPatterns/interpOut/Tucson_Rainlog_densityMask.grd", overwrite=TRUE)
  # save supporting data
  colnames(layerInfo)<-c("date","n","maxRain","meanRain","sdRain","medianRain","madRain","IQRRain","zeroRain","rmse","beta","neighbors")
      
  save(layerInfo, subDays, file = "/home/crimmins/RProjects/precipPatterns/interpOut/Tucson_Rainlog_IDW_beta23_monsoon_2007_2021_data.RData")
  
  
  # stack sum
  # precipSum <- calc(precipStack, sum)
  # precipSum <- focal(precipSum, w=matrix(1/9, nc=3, nr=3))
  # precipSum<-mask(precipSum, resObsDens)
  # plot(precipSum)
  
  # try focal for smoothing
  #idwRas <- focal(idwRas, w=matrix(1/9, nc=3, nr=3))
  # apply mask
  #idwRas<-mask(idwRas, resObsDens)
  
  
  
  
  
  
  
  
  
  
  ###### mapping for checking grids ----
  library(ggmap)
  library(scales)
  # API key
  source('~/RProjects/RainlogAPI/APIkey.R')
  # get map and bounding box
  where<-geocode("tucson", source = "google")
  where$lat=32.221551; where$lon=-110.909479 # center of download from getTucsonRainObs_listAppend.R
  
  TucsonMap <- qmap(location = c(lon=where$lon,lat=where$lat), zoom = 10,
                    color = "bw")
  
  
  nn <- as(idwRas, "SpatialPixelsDataFrame")
  nn <- as.data.frame(nn)
  colnames(nn) <- c("value", "x", "y")
  
  # colorramp for total precip
  precipCols<-colorRampPalette(c("lightblue", "dodgerblue3", "palegreen","green4","salmon","orangered3",
                                 "lightgoldenrod1","orange2","plum2","purple"))(50)
  precBreaks<-seq(0,6,0.5)
  precLabs<-as.character(seq(0,6,0.5))
  precLabs[13]<-">6"
  precLabs[1]<-"0.01"
  
  
  TucsonMap +  
    geom_tile(data=nn, aes(x=x, y=y, fill=value), alpha=0.8) + 
    #geom_point(data=dayPrecip, aes(x=position.lng ,y=position.lat, fill=rainAmount),size=1.5, shape=21)+
    #scale_color_viridis()+
    #scale_fill_viridis() +
    #coord_equal() +
    #theme_map() +
    scale_fill_gradientn(colours = precipCols, na.value="burlywood4", 
                         name="inches", limits=c(0,6),oob=squish, breaks=precBreaks, labels=precLabs, expand=NULL)+
    guides(fill= guide_colorbar(barwidth=20,nbin = 500, raster = FALSE))+
    theme(legend.position="bottom")+
    ggtitle("IDW RainLog - 8/10/22")
  
  # rasterVis
  elev<-raster("~/RProjects/SOMs/monsoonPrecip/shapes/PRISM_us_dem_4km_asc.asc")
  test<-mask(precipStack, resObsDens)
  # try focal for smoothing
  #test <- focal(test, w=matrix(1/9, nc=3, nr=3))
  test[test <= 0.001] <- NA
  
  at<-c(seq(0,5,0.05))
  #at<-c(seq(0,30,2.5))
  mapTheme <- rasterVis::rasterTheme(region = c("lightblue", "blue","green","green4","yellow","red", "red4"))
  
  rasterVis::levelplot(test, contour=FALSE, margin=FALSE, at=at,
            par.settings=mapTheme)+
    rasterVis::contourplot(elev, at=c(750,1000,1250), labels=FALSE, lwd = 0.3, par.settings = rasterVis::GrTheme)
            
######  
  
  
# testing out spatial metrics
  library(geodiv)
  
  precipStack<-mask(precipStack, resObsDens)
  layerInfo$maxVal<-maxValue(precipStack)
  layerInfo$minVal<-minValue(precipStack)
  layerInfo$maxDiff<-layerInfo$`max(dayPrecip$rainAmount)`-layerInfo$maxVal
  #peaks <- findpeaks(precipStack[[1]])
  
  