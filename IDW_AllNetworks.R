# IDW interpolation - Tucson All Network Data
# MAC 08/23/22
# adapted from IDW_Rainlog.R

#library(ggmap)
#library(dplyr)
#library(ggplot2)
library(raster)
library(gstat)
#library(viridis)
#library(scales)

# functions
RMSE <- function(residuals){
  sqrt(sum((residuals)^2)/length(residuals))
}

source('cvIDW.R')

# load data from processNetworkData.R
load("~/RProjects/precipPatterns/data/TucsonAllNetworks_2007_2022.RData")

# subset monsoon days
subDays<-tucsonRain[tucsonRain$dummyDate >= "2020-06-01" & tucsonRain$dummyDate <= "2020-09-30", ] # extract just monsoon days

# delete rows with NA
subDays<-subDays[!is.na(subDays$precip),]

# remove above elevation
hist(subDays$elevation, breaks=50)
subDays<-subset(subDays, elevation<=1200)

##### point density masking ----  
obs<-subDays
coordinates(obs) <- ~ lon + lat
obs<-crop(obs, extent(-111.25,-110.65,32,32.6)) # crop obs down to Tucson area


library(pointdensityP)
# more info at https://rpubs.com/msgc/point_pattern_analysis
# create raster
#r<-raster(daySp,res=0.01)
prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
r<-raster(extent(c(-111.3482, -110.4693,31.8,32.6)),res=0.01) # 0.01
crs(r)<-prj_dd

#temp<-subDays[c("date","lat","lon")]
#temp<-temp[complete.cases(temp), ]

temp<-cbind.data.frame(obs@data$date,obs@coords)
  colnames(temp)[1]<-"date"

reading_density <- pointdensity(df = temp, lat_col = "lat", lon_col = "lon",
                                date_col = "date", grid_size = 5, radius = 10) # 5, 10

reading_density <- reading_density[!duplicated(reading_density[ , c("lat", "lon")]), ]

coordinates(reading_density) <- ~ lon + lat # Convert data frame to spatial object
obsDensity <- rasterize(reading_density, r, "count", update = TRUE) # put point in raster

  # create optimal mask
  plot(obsDensity)
  resObsDens<-aggregate(obsDensity,5, fun=mean) # 5
  plot(resObsDens)
  resObsDens[resObsDens < 6000] <- NA #24000
  plot(resObsDens)
  resObsDens<-trim(resObsDens)
  plot(resObsDens)
  # reclassify to 1/NA
  resObsDens<-reclassify(resObsDens,c(0,+Inf,1))
  plot(resObsDens)
  bounds<-rasterToPolygons(resObsDens, dissolve = TRUE)
  plot(bounds)
  # resample to fine grid
  r<-raster(extent(resObsDens),res=0.01)
  crs(r)<-prj_dd
  resObsDens<-resample(resObsDens,r)
  resObsDens<-reclassify(resObsDens,c(0,+Inf,1))
  resObsDens<-mask(resObsDens, bounds)
  plot(resObsDens)
  plot(bounds, add=TRUE)

  plot(obs)
  plot(bounds, add=TRUE)
  
  crs(obs)<-prj_dd
  pts<-over(obs, bounds)
  table(pts$layer)/nrow(obs@data)
  
  # #nn<-mask(nn, resObsDens)
  # # plot density mask
  # # leaflet map
  # library(leaflet)
  # pal <- colorNumeric("RdYlBu", values(resObsDens),
  #                     na.color = "transparent")
  # 
  # leaflet() %>% addTiles() %>%
  #   #addRasterImage(resObsDens, colors = "red", opacity = 0.5) %>%
  #    addPolygons(data=bounds, color = "#444444", weight = 1, smoothFactor = 0.5,
  #                opacity = 1.0, fillOpacity = 0.5,
  #                fillColor = NA) 
  #   # addLegend(pal = pal, values = values(resObsDens),
  #   #           title = "Density Mask")
  
  writeRaster(resObsDens, filename = "/home/crimmins/RProjects/precipPatterns/interpOut/Tucson_All_1km_densityMask_2007_2022.grd", overwrite=TRUE)
  
  
#####  

# get date list from subset
dates<-as.data.frame(unique(subDays$date))
colnames(dates)<-"ymd"
dates<-dates[order(dates$ymd),]

# find a date
#i=which(dates=="2021-08-10" )
# run through a date range
#idx<-which(dates>="2021-06-15" & dates<="2021-09-30")
idx<-which(dates>="2007-06-15" & dates<="2022-09-30")

# loop through days
prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
precipStack<-stack() # raster stack
infoList<-list() # list of layer info

# loop through days to calc grids
start_time <- Sys.time()
for(i in idx[1]:idx[length(idx)]){
  
  dayPrecip<-subset(subDays, date==dates[i])
  daySp <- SpatialPoints(dayPrecip[,2:3], proj4string=CRS(prj_dd))
  daySp <- SpatialPointsDataFrame(daySp, dayPrecip)
  # crop to point density extent
  daySp <- crop(daySp,resObsDens)
  dayPrecip<-daySp@data
  
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
                    stat.formula = formula(precip ~ 1),
                    evalGridSize = res(r)[1],
                    #seqNeighbors = c(5,50,nrow(dayPrecip)-1),
                    seqNeighbors = 5,
                    #seqBeta = seq(from = 1, to = 3, 1),
                    seqBeta = c(3),
                    evalRaster = r,
                    verbose = TRUE)
  # save info in dataframe
  infoList[[i]]<-cbind.data.frame(dates[i],nrow(dayPrecip), max(dayPrecip$precip), 
                                  mean(dayPrecip$precip), sd(dayPrecip$precip),
                                  median(dayPrecip$precip),mad(dayPrecip$precip),
                                  IQR(dayPrecip$precip), length(which(dayPrecip$precip==0)),
                                  tuneIDW$bestRMSE, tuneIDW$bestBeta, tuneIDW$bestNeighbors)
  idwRas<-raster(tuneIDW$idwBestRaster, layer=1, values=TRUE)
  idwRas<-resample(idwRas, resObsDens)
  idwRas[idwRas < 0.01] <- NA
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
writeRaster(precipStack, filename = "/home/crimmins/RProjects/precipPatterns/interpOut/Tucson_All_IDW_1km_beta3_ngb5_monsoon_2007_2022.grd", overwrite=TRUE)
writeRaster(resObsDens, filename = "/home/crimmins/RProjects/precipPatterns/interpOut/Tucson_All_1km_densityMask_2007_2022.grd", overwrite=TRUE)
# save supporting data
colnames(layerInfo)<-c("date","n","maxRain","meanRain","sdRain","medianRain","madRain","IQRRain","zeroRain","rmse","beta","neighbors")

save(layerInfo, subDays, file = "/home/crimmins/RProjects/precipPatterns/interpOut/Tucson_All_IDW_1km_beta3_ngb5_monsoon_2007_2022_data.RData")


