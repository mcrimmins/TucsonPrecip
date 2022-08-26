# code to analyze gridded Rainlog data
# output from IDW_Rainlog.R
# MAC 08/19/22

library(raster)

# load data saved from IDW_Rainlog.R
precipStack<-stack("/home/crimmins/RProjects/precipPatterns/interpOut/Tucson_Rainlog_IDW_beta3_monsoon_2007_2021.grd")
resObsDens<-stack("/home/crimmins/RProjects/precipPatterns/interpOut/Tucson_Rainlog_densityMask.grd")
load("/home/crimmins/RProjects/precipPatterns/interpOut/Tucson_Rainlog_IDW_tuned_monsoon_2007_2021_data.RData")

# load data saved from IDW_AllNetworks.R
precipStack<-stack("/home/crimmins/RProjects/precipPatterns/interpOut/Tucson_All_IDW_1km_beta3_monsoon_2007_2021.grd")
resObsDens<-stack("/home/crimmins/RProjects/precipPatterns/interpOut/Tucson_All_1km_densityMask.grd")
load("/home/crimmins/RProjects/precipPatterns/interpOut/Tucson_All_IDW_1km_beta3_monsoon_2007_2021_data.RData")



# load data from getTucsonPRISM.R
precipStack2<-stack("/home/crimmins/RProjects/precipPatterns/data/Tucson_PRISM_monsoon_2007_2021.grd")
precipStack2<-crop(precipStack2,precipStack)
precipStack2<-resample(precipStack2,precipStack)

#####  analyze layerInfo metrics ----

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
b.wet<-reclassify(precipStack,c(-Inf,1.5,0))
b.wet<-reclassify(b.wet,c(1.5,+Inf,1))
#plot(b.wet[[100]])
#plot(sum(b.wet), zlim=c(300,700))
hist(sum(b.wet, na.rm = TRUE))
plot(sum(b.wet, na.rm = TRUE), zlim=c(0,15))

test<-values(sum(b.wet, na.rm = TRUE))

tempGrid<-sum(b.wet)

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




