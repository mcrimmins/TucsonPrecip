# combine, clean different network data for Tucson monsoon analysis
# MAC 08/23/22

#### RAINLOG DATA ----
# load and clean Rainlog data
load("./data/TucsonRainlogObs_2007_2021_allQuality.RData") # from getTucsonRainObs_listAppend.R

# add elevations to gauges
# https://cran.r-project.org/web/packages/elevatr/vignettes/introduction_to_elevatr.html#get_point_elevation_data
# library(elevatr)
# 
# prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# LatLons<-gaugeStack[c(10,9,1)]
# colnames(LatLons)<-c("x","y","gaugeRevisionId")
# LatLons_Elevs <- get_elev_point(LatLons, prj = prj_dd, src = "epqs")

#load("TucsonGauges_Elevations.RData") # getElevs.R

# add elevations to gauges
#elevs<-LatLons_Elevs@data
#gaugeStack<-merge(gaugeStack,elevs, by="gaugeRevisionId", all.x = TRUE)

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
mergedData$rainAmount<-ifelse(mergedData$quality=="Trace",0,mergedData$rainAmount)
# remove if still missing rainAmount
mergedData<-mergedData[!is.na(mergedData$rainAmount),]

postClean<-nrow(mergedData)
cleanedRecs<-preClean-postClean

rainlog<-mergedData[,c(20,19,3,5,9)]
colnames(rainlog)<-c("lon","lat","gaugeID","date","precip")
rainlog$network<-"rainlog"
rainlog$gaugeID<-as.character(rainlog$gaugeID)

# ----- END CLEANING

##### TUCSON ACIS DATA -----
load("~/RProjects/precipPatterns/data/TucsonACISObs_2007_2021.RData")
allData <- allData[-which(allData$precip>5.24),] # trim extreme values
allData$date<-allData$date-1 # shift date back 1 day
# remove duplicated gauge/date entries
allData <- allData[!duplicated(allData[,3:4]),]

acis<-allData
acis$network<-"acis"

#####

##### TUCSON SYNOP LABS DATA ----
load("~/RProjects/precipPatterns/data/TucsonSynopLabs_2007_2021.RData")
fullData <- fullData[-which(fullData$total>5.24),] # trim extreme values
fullData$precipDate<-fullData$precipDate-1 # shift date back 1 day

synop<-fullData[,c(7,10,5,24,22)]
synop$LONGITUDE<-as.numeric(synop$LONGITUDE)
synop$LATITUDE<-as.numeric(synop$LATITUDE)
colnames(synop)<-c("lon","lat","gaugeID","date","precip")
synop$network<-"mesowest"

# combine all networks into one dataframe
tucsonRain<-rbind.data.frame(rainlog,acis,synop)

# add in dummydate for selections
tucsonRain$dummyDate<-as.Date(paste0("2020-",format(tucsonRain$date,"%m"),"-",format(tucsonRain$date,"%d")))

# get elevations for all stations in final dataset
# add elevation
gaugesTucsonAllNet<-tucsonRain[!duplicated(tucsonRain$gaugeID), ]

library(elevatr)
prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
LatLons<-gaugesTucsonAllNet[c(1,2,3)]
colnames(LatLons)<-c("x","y","gaugeID")
LatLons_Elevs <- get_elev_point(LatLons, prj = prj_dd, src = "epqs")
# add elevations to gauges
elevs<-LatLons_Elevs@data
tucsonRain<-merge(tucsonRain,elevs, by="gaugeID", all.x = TRUE)

# save full data file
save(tucsonRain, file = paste0("./data/TucsonAllNetworks_2007_2021.RData"))

#####





##### Network diagnostics

# find gauge locations
tucsonRain$latlon<-paste0(tucsonRain$lat,"_",tucsonRain$lon)

gauges<-tucsonRain[!duplicated(tucsonRain$gaugeID), ]
locs<-tucsonRain[!duplicated(tucsonRain$latlon), ]
setdiff(gauges$gaugeID,locs$gaugeID)

# subset monsoon days
subDays<-tucsonRain[tucsonRain$dummyDate >= "2020-06-15" & tucsonRain$dummyDate <= "2020-09-30", ] # extract just monsoon days

# count of observations per station
library(dplyr)
gaugeCount<-subDays %>% group_by(gaugeID) %>%
                           summarize(n=n(),
                                     lat=first(lat),
                                     lon=first(lon))

dayCount<-subDays %>% group_by(date) %>%
  summarize(n=n())



library(ggmap)
# API key
source('~/RProjects/RainlogAPI/APIkey.R')
# get map and bounding box
where<-geocode("tucson", source = "google")
where$lat=32.221551; where$lon=-110.909479 # center of download from getTucsonRainObs_listAppend.R

TucsonMap <- qmap(location = c(lon=where$lon,lat=where$lat), zoom = 10,
                  color = "bw")

theme_set(theme_bw(16))
p<-TucsonMap +
  geom_point(data=gaugeCount, aes(x=lon,y=lat, color=n),size=1.5)+
  scale_color_viridis_b()# removed from AES
  
##### point density masking ----  
library(raster)
library(pointdensityP)
# more info at https://rpubs.com/msgc/point_pattern_analysis
# create raster
#r<-raster(daySp,res=0.01)
prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
r<-raster(extent(c(-111.3482, -110.4693,31.8,32.6)),res=0.01)
crs(r)<-prj_dd

temp<-subDays[c("date","lat","lon")]
temp<-temp[complete.cases(temp), ]

reading_density <- pointdensity(df = temp, lat_col = "lat", lon_col = "lon",
                                date_col = "date", grid_size = 5, radius = 10)

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




