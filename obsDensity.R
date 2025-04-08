# define Tucson precip study area based on observatiob density
# MAC 10/26/22
# adapted from IDW_AllNetworks.R

library(raster)
library(spatstat)
library(maptools)

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
obs<-crop(obs, extent(-111.30,-110.5,31.99,32.6)) # crop obs down to Tucson area

# point process from spatstat
temp<-cbind.data.frame(obs@data$date,obs@coords)

  bbox <- as(raster::extent(obs), "SpatialPolygons")
  proj4string(bbox) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  bbox<-as.owin(bbox)
  # convert to ppp
  obsPts  <- as.ppp(temp[,c(2,3)], W=bbox)

# Q <- quadratcount(obsPts, nx= 6, ny=6)
# plot(obsPts, pch=20, cols="grey70", main=NULL)  # Plot points
# plot(Q, add=TRUE)  # Add quadrat grid

# calculate density grid  
k1<-density(obsPts, 0.04,edge=TRUE) # 0.05 
  plot(k1, main=NULL, las=1)
  contour(k1, add=TRUE)

# convert grid to poly bound  
rKDE<-raster(k1)  
rKDE <- reclassify(rKDE, cbind(-Inf, 900000, NA)) #1200000
rKDE <- reclassify(rKDE, cbind(900000,Inf, 1))
rKDE <-trim(rKDE)
# get polygon from grid
bounds <- rasterToPolygons(rKDE, dissolve = TRUE)
plot(bounds)
#plot(obs, pch=20)
#plot(bounds, add=TRUE)

# resample to project grid
prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
r<-raster(extent(c(-111.3482, -110.4693,31.8,32.6)),res=0.01) # 0.01
crs(r)<-prj_dd

# resample to fine grid
r<-raster(extent(rKDE),res=0.01)
crs(r)<-prj_dd
resObsDens<-resample(rKDE,r)
resObsDens<-reclassify(resObsDens,c(0,+Inf,1))
resObsDens<-trim(resObsDens)
resObsDens<-mask(resObsDens, bounds)
plot(resObsDens)
plot(bounds, add=TRUE)

# save study area grid
writeRaster(resObsDens, filename = "/home/crimmins/RProjects/precipPatterns/interpOut/Tucson_All_1km_densityMask_2007_2022.grd", overwrite=TRUE)

# stats and diagnostics
prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
crs(obs)<-prj_dd; crs(bounds)<-prj_dd
pts<-over(obs, bounds)
table(pts$layer)/nrow(obs@data)

plot(obs, pch=20)
plot(bounds, add=TRUE)

#nn<-mask(nn, resObsDens)
# plot density mask
# leaflet map
library(leaflet)
# pal <- colorNumeric("RdYlBu", values(resObsDens),
#                     na.color = "transparent")

leaflet() %>% addTiles() %>%
  #addRasterImage(resObsDens, colors = "red", opacity = 0.5) %>%
  addPolygons(data=bounds, color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = NA)
# addLegend(pal = pal, values = values(resObsDens),
#           title = "Density Mask")


##### 
# # point density method
# library(pointdensityP)
# # more info at https://rpubs.com/msgc/point_pattern_analysis
# # create raster
# #r<-raster(daySp,res=0.01)
# prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# r<-raster(extent(c(-111.3482, -110.4693,31.8,32.6)),res=0.01) # 0.01
# crs(r)<-prj_dd
# 
# #temp<-subDays[c("date","lat","lon")]
# #temp<-temp[complete.cases(temp), ]
# 
# temp<-cbind.data.frame(obs@data$date,obs@coords)
# colnames(temp)[1]<-"date"
# 
# reading_density <- pointdensity(df = temp, lat_col = "lat", lon_col = "lon",
#                                 date_col = "date", grid_size = 5, radius = 10) # 5, 10
# 
# reading_density <- reading_density[!duplicated(reading_density[ , c("lat", "lon")]), ]
# 
# coordinates(reading_density) <- ~ lon + lat # Convert data frame to spatial object
# obsDensity <- rasterize(reading_density, r, "count", update = TRUE) # put point in raster
# 
# # create optimal mask
# plot(obsDensity)
# resObsDens<-aggregate(obsDensity,5, fun=mean) # 5
# plot(resObsDens)
# resObsDens[resObsDens < 6000] <- NA #24000
# plot(resObsDens)
# resObsDens<-trim(resObsDens)
# plot(resObsDens)
# # reclassify to 1/NA
# resObsDens<-reclassify(resObsDens,c(0,+Inf,1))
# plot(resObsDens)
# bounds<-rasterToPolygons(resObsDens, dissolve = TRUE)
# plot(bounds)
# # resample to fine grid
# r<-raster(extent(resObsDens),res=0.01)
# crs(r)<-prj_dd
# resObsDens<-resample(resObsDens,r)
# resObsDens<-reclassify(resObsDens,c(0,+Inf,1))
# resObsDens<-mask(resObsDens, bounds)
# plot(resObsDens)
# plot(bounds, add=TRUE)
# 
# plot(obs)
# plot(bounds, add=TRUE)
# 
# crs(obs)<-prj_dd
# pts<-over(obs, bounds)
# table(pts$layer)/nrow(obs@data)

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

#writeRaster(resObsDens, filename = "/home/crimmins/RProjects/precipPatterns/interpOut/Tucson_All_1km_densityMask_2007_2022.grd", overwrite=TRUE)
