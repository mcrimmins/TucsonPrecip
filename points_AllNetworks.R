# testing contouring as mapping solution

library(raster)

# load data from processNetworkData.R
load("~/RProjects/precipPatterns/data/TucsonAllNetworks_2007_2021.RData")

# subset monsoon days
subDays<-tucsonRain[tucsonRain$dummyDate >= "2020-06-15" & tucsonRain$dummyDate <= "2020-09-30", ] # extract just monsoon days

# delete rows with NA
subDays<-subDays[!is.na(subDays$precip),]

# remove above elevation
hist(subDays$elevation, breaks=50)
subDays<-subset(subDays, elevation<=1200)


##### point density masking ----  
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
resObsDens[resObsDens < 18000] <- NA
plot(resObsDens)
resObsDens<-trim(resObsDens)
plot(resObsDens)
# reclassify to 1/NA
resObsDens<-reclassify(resObsDens,c(0,+Inf,1))
plot(resObsDens)
bounds<-rasterToPolygons(resObsDens, dissolve = TRUE)
plot(bounds)

# clip to bounds
subDaysSp <- SpatialPoints(subDays[,2:3], proj4string=CRS(prj_dd))
subDaysSp <- SpatialPointsDataFrame(subDaysSp, subDays)
# crop to point density extent
subDaysSp<-subDaysSp[complete.cases(over(subDaysSp,bounds)), ]

plot(bounds)
plot(subDaysSp, add=TRUE)

#####  

# get date list from subset
dates<-as.data.frame(unique(subDays$date))
colnames(dates)<-"ymd"
dates<-dates[order(dates$ymd),]

# find a date
#i=which(dates=="2021-08-10" )
# run through a date range
#idx<-which(dates>="2021-06-15" & dates<="2021-09-30")
idx<-which(dates>="2007-06-15" & dates<="2021-09-30")


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


nn <- as(resObsDens, "SpatialPixelsDataFrame")
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

