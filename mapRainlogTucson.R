# testing of precip patterns analysis with Tucson Rainlog data
# using pre-processing code from Tucson_Rainlog_Analysis.R
# MAC 08/12/22

library(ggmap)
library(dplyr)
library(ggplot2)
library(viridis)
library(scales)

# API key
source('~/RProjects/RainlogAPI/APIkey.R')
# get map and bounding box
where<-geocode("tucson", source = "google")
where$lat=32.221551; where$lon=-110.909479 # center of download from getTucsonRainObs_listAppend.R

TucsonMap <- qmap(location = c(lon=where$lon,lat=where$lat), zoom = 10,
                  color = "bw")

# TucsonMap <- qmap(location = c(lon = where[1,1], lat = where[1,2]), zoom = 10,
#                   source = "stamen", maptype = "terrain", color="bw")


# load data
load("TucsonRainlogObs_2007_2021_allQuality.RData") # from getTucsonRainObs_listAppend.R
#load("~/RProjects/RainlogAPI/manuscript/TIA_COOP.RData")
load("TucsonGauges_Elevations.RData") # getElevs.R

# add elevations to gauges
elevs<-LatLons_Elevs@data
gaugeStack<-merge(gaugeStack,elevs, by="gaugeRevisionId", all.x = TRUE)

#####
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
preClean<-nrow(mergedData)

## ----- DATA CLEANING -----
# scrub out extremes >5.24" TIA 1000 year 24 hr total
# https://hdsc.nws.noaa.gov/hdsc/pfds/pfds_map_cont.html?bkmrk=az
#mergedData <- mergedData[-which(mergedData$rainAmount>5.24),]

# eliminate Summerhaven gauge obs, 10565
#mergedData<-mergedData[-which(mergedData$gaugeId.x==10565),] # Summerhaven gauge
#mergedData<-mergedData[-which(mergedData$gaugeId.x==630),] # Mt Kimball gauge

# find some key words in remarks col - looking for cumulative totals
mergedData<-mergedData[-which(grepl("cumulative", mergedData$remarks, ignore.case = TRUE)==TRUE),]
mergedData<-mergedData[-which(grepl("accumulated", mergedData$remarks, ignore.case = TRUE)==TRUE),]
mergedData<-mergedData[-which(grepl("gone", mergedData$remarks, ignore.case = TRUE)==TRUE),]
mergedData<-mergedData[-which(grepl("out of town", mergedData$remarks, ignore.case = TRUE)==TRUE),]
mergedData<-mergedData[-which(grepl("vacation", mergedData$remarks, ignore.case = TRUE)==TRUE),]
mergedData<-mergedData[-which(grepl("trip", mergedData$remarks, ignore.case = TRUE)==TRUE),]
# keep only Good and Trace
mergedData<-mergedData[mergedData$quality %in% c('Good','Trace'),]
postClean<-nrow(mergedData)
cleanedRecs<-preClean-postClean
# ----- END CLEANING

# find days to map
# records by date
recsDate<- dataStack %>% group_by(readingDate) %>%
                          tally()


# subset to one day
dayPrecip<-subset(mergedData, readingDate=="2018-07-10")

theme_set(theme_bw(16))
p<-TucsonMap +
  geom_point(data=dayPrecip, aes(x=position.lng ,y=position.lat, color=rainAmount),size=1.5)+ # removed from AES
  
  #facet_wrap(~readingDate, ncol = 15, nrow = ceiling(length(allDates)/15))+ # 15 for whole season
  #scale_shape_manual(values=c(16,utf8ToInt("T"),utf8ToInt("0")), guide=FALSE)+ # outline=21, plus=3
  # scale_color_gradient2(limits=c(0, 1), mid=("yellow"), high="red", low="cyan", oob=squish, midpoint = 0.5, name="Precip (in)",
  #                       labels = c("0.00", "0.25", "0.5", "0.75", "≥ 1.00"),
  #                       breaks = c(0, 0.25, 0.5, 0.75, 1.0), na.value="black")+
  # scale_color_gradient2(limits=c(0, 3), mid=("#41b6c4"), high="#081d58", low="#edf8b1", oob=squish, midpoint = 1.5, name="Precip (in)",
  #                       labels = c("0.01", "1.00", "2.00", "≥ 3.00"),
  #                       breaks = c(0.01, 1.00, 2.00, 3.00),
  #                       na.value="darkred")+
  scale_color_viridis(limits=c(0, 3), oob=squish, name="Precip (in)",
                        labels = c("0.01", "1.00", "2.00", "≥ 3.00"),
                        breaks = c(0.01, 1.00, 2.00, 3.00),
                        na.value="darkred", direction=-1)+
# scale_color_viridis(limits=c(0, 75), oob=squish, name="Precip (mm)",
#                     labels = c("1", "25", "50", "≥ 75"),
#                     breaks = c(1, 25, 50, 75),
#                     na.value="darkred", direction=-1)+
  #labs(title="Top Extreme Days - Tucson Rainlog Network (2007-2018)")+
  theme(legend.position="bottom",plot.title = element_text(size = 8, face = "bold"))+
  theme(legend.text = element_text(colour="black", size=10))+
  theme(legend.title = element_text(colour="black", size=10))
  

# interpolation and mapping
library(rgdal)
library(sp)
library(raster)

# remove NA in lng/lat
dayPrecip<-dayPrecip[!is.na(dayPrecip$position.lng),]
# convert trace to 0.001
dayPrecip$rainAmount<-ifelse(dayPrecip$quality=="Trace",0.001,dayPrecip$rainAmount)

prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

daySp <- SpatialPoints(dayPrecip[,20:19], proj4string=CRS(prj_dd))
daySp <- SpatialPointsDataFrame(daySp, dayPrecip)

# proximity polygons
# library(dismo)
# v <- voronoi(daySp)
# plot(v)
# spplot(v, 'rainAmount', col.regions=rev(get_col_regions()))


#r<-raster(daySp,res=0.01)
r<-raster(extent(c(-111.3482, -110.4693,31.8,32.6)),res=0.01)
crs(r)<-prj_dd

library(gstat)
gs <- gstat(formula=rainAmount~1, locations=daySp, nmax=48, set=list(idp = 2))
nn <- interpolate(r, gs)
plot(nn)
## [inverse distance weighted interpolation]
#nnmsk <- mask(nn, vr)
#plot(nnmsk)

# get elevation data
# elevation <- raster::getData('alt', country='USA')
# elevation<-elevation[[1]]
# elevTuc<-crop(elevation,r)
# elevTuc<-resample(elevTuc,r)
# elevTuc[elevTuc > 1100] <- NA
# nn <- mask(nn, elevTuc)


# mask on point density calculations ----
library(pointdensityP)

temp<-mergedData[c("readingDate","position.lat","position.lng")]
temp<-temp[complete.cases(temp), ]

reading_density <- pointdensity(df = temp, lat_col = "position.lat", lon_col = "position.lng",
                                date_col = "readingDate", grid_size = 5, radius = 10)

reading_density <- reading_density[!duplicated(reading_density[ , c("lat", "lon")]), ]

coordinates(reading_density) <- ~ lon + lat # Convert data frame to spatial object
obsDensity <- rasterize(reading_density, r, "count", update = TRUE) # put point in raster

resObsDens<-aggregate(obsDensity,5, fun=mean)
plot(resObsDens)
resObsDens[resObsDens < 50000] <- NA
plot(resObsDens)
resObsDens<-resample(resObsDens,r)
nn<-mask(nn, resObsDens)


# https://stackoverflow.com/questions/33530055/add-raster-to-ggmap-base-map-set-alpha-transparency-and-fill-color-to-inset-r
#TucsonMap + 
#inset_raster(as.raster(nn), xmin = nn@extent[1], xmax = nn@extent[2],
#             ymin = nn@extent[3], ymax = nn@extent[4])  


nn <- as(nn, "SpatialPixelsDataFrame")
nn <- as.data.frame(nn)
colnames(nn) <- c("value", "x", "y")

TucsonMap +  
  geom_tile(data=nn, aes(x=x, y=y, fill=value), alpha=0.8) + 
  #geom_point(data=dayPrecip, aes(x=position.lng ,y=position.lat, fill=rainAmount),size=1.5, shape=21)+
  #scale_color_viridis()+
  scale_fill_viridis() +
  #coord_equal() +
  #theme_map() +
  theme(legend.position="bottom")

#####
# IDW cross validation
# https://www.geo.fu-berlin.de/en/v/soga/Geodata-analysis/geostatistics/Inverse-Distance-Weighting/Model-selection/index.html

# set parameter
neighbors = length(daySp)-1
beta = 3
# build model
idw_rain = gstat(formula = rainAmount ~ 1, # intercept only model
                 data = daySp, 
                 nmax = neighbors, 
                 set = list(idp = beta))

crossval <- gstat.cv(idw_rain)
crossvalDF<-crossval@data

RMSE <- function(residuals){
  sqrt(sum((residuals)^2)/length(residuals))
}

crossval_rmse <- RMSE(crossval$residual)

nn <- interpolate(r, idw_rain)
plot(nn)

plot(crossval[318])

nn <- interpolate(r, crossval[318])
plot(nn)

# loop through all combos
cv.IDW  <- function(spatialDF, stat.formula = NULL,
                    seqNeighbors = NULL, seqBeta = NULL,
                    evalGridSize = NULL, 
                    evalRaster = NULL, 
                    verbose = TRUE){
  
  ### LOAD LIBRARIES ###
  library(sp)
  #library(sf)
  library(gstat)
  library(raster)
  
  
  # spatialDF = daySp 
  # stat.formula = formula(rainAmount ~ 1) 
  # evalGridSize = 0.01 # in units of prs
  # evalRaster = r
  # seqNeighbors = seq(from = 6, to = 8, 1)
  # seqBeta = seq(from = 1, to = 2, 0.5)
  # verbose = T
  
  ### PROVIDE DEFAULT VALUES FOR FUNCTION ARGUMENTS ###
  if (is.null(seqNeighbors)){
    seqNeighbors <- round(seq(3, length(spatialDF), length.out = 10))
  }
  if (is.null(seqBeta)){
    seqBeta <- c(0.1, seq(0.5, 3, 0.5))
  }
  if (is.null(evalGridSize)){
    x.interval <- extent(spatialDF)@xmax - extent(spatialDF)@xmin
    y.interval <- extent(spatialDF)@ymax - extent(spatialDF)@ymin
    evalGridSize <- round(min(x.interval, y.interval) *0.05)
  }
  if (is.null(stat.formula)){
    print('Please provide a formula!!')
    return()
  }
  if (is.null(evalRaster)){
    extent.evalGrid <- extent(spatialDF)
  }else{
    extent.evalGrid <- extent(evalRaster)
  }
  
  
  ### BUILD A GRID FOR PARAMETER COMBINATIONS ###
  cv.Grid <- expand.grid(Beta = seqBeta,
                         Neighbors = seqNeighbors)
  cv.Grid$RMSE <- NA
  
  ### LOOP THROUGH ALL PARAMETER COMBINATIONS ###
  for (i in 1:nrow(cv.Grid)){
    ### BUILD IDW MODEL ###
    idw <- gstat(formula = stat.formula,
                 data = spatialDF, 
                 nmax = cv.Grid[i, 'Neighbors'], 
                 set = list(idp = cv.Grid[i, 'Beta']))
    ### PERFORM LOOCV ###
    crossval <- gstat.cv(idw, 
                         nmax = cv.Grid[i, 'Neighbors'],
                         beta = v.Grid[i, 'Beta'],
                         debug.level = 0)
    cv.Grid[i, 'RMSE'] <- RMSE(crossval$residual)
    if (verbose){
      print(paste('Function call', i, 'out of',  nrow(cv.Grid)))
      print(paste('Evaluating beta =', 
                  cv.Grid[i, 'Beta'], 
                  'and neighbors =',  
                  cv.Grid[i, 'Neighbors']))
      print(paste('RMSE=', RMSE(crossval$residual)))
    }
  }
  
  ### GET BEST PARAMTER VALUES ###
  idx.min <- which.min(cv.Grid$RMSE)
  best.Beta <- cv.Grid$Beta[idx.min]
  best.Neighbors <- cv.Grid$Neighbors[idx.min]
  min.RMSE <- cv.Grid$RMSE[idx.min]
  
  ### BUILD IDW MODEL BASED ON BEST PARAMTER VALUES ###
  idw.best <- gstat(formula = stat.formula,
                    data = spatialDF, 
                    nmax = best.Neighbors, 
                    set = list(idp = best.Beta))
  
  ### PREPARE EVALUATION GRID ###
  grid.evalGrid  <- expand.grid(x = seq(from = round(extent.evalGrid@xmin,2),
                                        to = round(extent.evalGrid@xmax,2),
                                        by = evalGridSize), 
                                y = seq(from = round(extent.evalGrid@ymin,2),
                                        to = round(extent.evalGrid@ymax,2), 
                                        by = evalGridSize))
  
  
  coordinates(grid.evalGrid) <- ~x + y
  proj4string(grid.evalGrid) <- proj4string(spatialDF)
  gridded(grid.evalGrid) <- TRUE
  
  
  ### INTERPOLATE VALUES FOR EVALUATION GRID USING THE BEST MODEL ###
  idw.best.predict <- predict(object = idw.best,
                              newdata = grid.evalGrid,
                              debug.level = 0)
  
  ### RETURN RESULTS AND OBJECTS ###
  return(list('idwBestModel' = idw.best,
              'idwBestRaster' = idw.best.predict,
              'bestBeta' = best.Beta,
              'bestNeighbors' = best.Neighbors,
              'bestRMSE' = min.RMSE,
              'gridCV' = cv.Grid))
}


my_IDW <- cv.IDW(spatialDF = daySp, 
                 stat.formula = formula(rainAmount ~ 1), 
                 evalGridSize = 0.01,
                 #seqNeighbors = seq(from = 6, to = 8, 1),
                 #seqBeta = seq(from = 1, to = 2, 0.5),
                 evalRaster = r,
                 verbose = TRUE)

nn<-raster(my_IDW$idwBestRaster, layer=1, values=TRUE)
nn<-resample(nn, resObsDens)
nn<-mask(nn, resObsDens)

# try focal for smoothing
# gf <- focalWeight(r, 2, "Gauss")
# rg <- focal(r, w=gf)

nn <- as(nn, "SpatialPixelsDataFrame")
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
  geom_point(data=dayPrecip, aes(x=position.lng ,y=position.lat, fill=rainAmount),size=1.5, shape=21)+
  #scale_color_viridis()+
  #scale_fill_viridis() +
  #coord_equal() +
  #theme_map() +
  scale_fill_gradientn(colours = precipCols, na.value="burlywood4", 
                       name="inches", limits=c(0,6),oob=squish, breaks=precBreaks, labels=precLabs, expand=NULL)+
  guides(fill= guide_colorbar(barwidth=20,nbin = 500, raster = FALSE))+
  theme(legend.position="bottom")

#####


# testing out spatial metrics





