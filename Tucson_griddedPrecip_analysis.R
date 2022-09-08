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
load("~/RProjects/precipPatterns/interpOut/Tucson_All_IDW_1km_beta3_5ngb_errors_monsoon_2007_2021_data.RData")
resObsDens<-stack("/home/crimmins/RProjects/precipPatterns/interpOut/Tucson_All_1km_densityMask.grd")
tucsonRain<-tucsonRain_error
tucsonRain$errorPerc<-(tucsonRain$error/tucsonRain$precip)

# drop KDMA, problematic station
tucsonRain<-subset(tucsonRain, gaugeID!="KDMA")


# subset network if needed
#tucsonRain<-subset(tucsonRain, network!="rainlog")
#tucsonRain<-subset(tucsonRain, network=="acis")

# add in quality flag based on error -- predicted precip 0 based on neighbors
tucsonRain$flag<-ifelse(tucsonRain$precip>0 & tucsonRain$error==tucsonRain$precip, 1, 0)
tucsonRain$flag2<-ifelse(tucsonRain$precip>0.15 & tucsonRain$errorPerc>=0.85 & tucsonRain$errorPerc<=1, 1, 0)
#tucsonRain$flag2<-ifelse(tucsonRain$precip>0.01 & tucsonRain$errorPerc>=0.85, 1, 0)
tucsonRain$flagSum<-tucsonRain$flag+tucsonRain$flag2
hist(tucsonRain$errorPerc[tucsonRain$errorPerc>0])
length(which((tucsonRain$flagSum>1)==TRUE))

# diagnostic plots
p<-ggplot(subset(tucsonRain,errorPerc>0), aes(errorPerc,precip, color=error, label=date))+
  geom_point()+
  scale_colour_gradientn(colours=rainbow(4))+
  geom_vline(xintercept = 0.85)
#geom_smooth()
#geom_vline(xintercept = 1.2)
ggplotly(p)


# number of flags by network
table(tucsonRain[which((tucsonRain$flag+tucsonRain$flag2>0)==TRUE),]$network)/length(which((tucsonRain$flag+tucsonRain$flag2>0)==TRUE))
table(tucsonRain$network)/nrow(tucsonRain)

# subset monsoon days
subDays<-tucsonRain[tucsonRain$dummyDate >= "2020-06-15" & tucsonRain$dummyDate <= "2020-09-30", ] # extract just monsoon days
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

# hist on non zero values
hist( subDaysDF$precip[ !subDaysSp$precip==0 ])
quantile(subDaysDF$precip, c(0.9,0.5))
quantile(subDaysDF$precip[ !subDaysDF$precip==0 ], c(0.25,0.5,0.66, 0.75, 0.9, 0.95, 0.99))

# get date list from subset
dates<-as.data.frame(unique(subDays$date))
colnames(dates)<-"ymd"
dates<-dates[order(dates$ymd),]

# find a date
#i=which(dates=="2020/07/22" )
# run through a date range
#idx<-which(dates>="2021-06-15" & dates<="2021-09-30")
idx<-which(dates>="2007-06-15" & dates<="2021-09-30")

# daySp<-subset(subDaysSp, date=="2010/09/02")
#   temp<-subDaysSp@data
#   temp<-daySp@data

# loop through to create more daily point metrics
  pptMetrics<-list()
  subDaysRev<-list()
  
  for(i in idx[1]:idx[length(idx)]){
    
    dayPrecip<-subset(subDaysDF, date==dates[i])
    #dayPrecip<-dayPrecip@data
    dayPrecip<-subset(dayPrecip,flag==0) # data quality flag
    dayPrecip<-subset(dayPrecip, flag2==0) # second flag
   
    # calculate z-score
    dayPrecip<-dayPrecip %>% 
      mutate(zscore = (precip - mean(precip, na.rm=TRUE))/sd(precip, na.rm=TRUE))
    
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
    
    mae<-sum(abs(dayPrecip$error),na.rm=TRUE)/nrow(dayPrecip)
    
    pptMetrics[[i]]<-cbind.data.frame(dates[i],nrow(dayPrecip), max(dayPrecip$precip), 
                                    min(dayPrecip$precip),  
                                    mean(dayPrecip$precip), sd(dayPrecip$precip),
                                    median(dayPrecip$precip),mad(dayPrecip$precip),
                                    IQR(dayPrecip$precip), length(which(dayPrecip$precip==0)),
                                    length(which(dayPrecip$precip>=0.15)),
                                    length(which(dayPrecip$precip>=0.3)),
                                    length(which(dayPrecip$precip>=0.8)),
                                    length(which(dayPrecip$precip>=1.2)),
                                    length(which(dayPrecip$precip>=2)),
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
                                    mae
                                                                      )
  }
  
  # combine list into df
  pptMetrics = do.call(rbind, pptMetrics)
  subDaysRev<-do.call(rbind, subDaysRev)
  # save supporting data
  colnames(pptMetrics)<-c("date","n","maxRain","minRain","meanRain","sdRain","medianRain","madRain","IQRRain","zeroRain",
                         "p50_15", "p66_30", "p90_80","p95_120","p99_200",
                         "mean_nz","sd_nz","med_nz","mad_nz","IQR_nz",
                         "top1_2","top1_3","top1_5","top1_2_ratio",
                         "maxAbsError","maxAbsErrorPerc", "maxError", "maxZscore", "maxErrorPerc", "mae"
                         )
  
  pptMetrics$CV<-pptMetrics$sdRain/pptMetrics$meanRain
  pptMetrics$percZero<-pptMetrics$zeroRain/pptMetrics$n
  pptMetrics$CV_med<-pptMetrics$IQRRain/pptMetrics$medianRain
  pptMetrics$CV_nz<-pptMetrics$sd_nz/pptMetrics$mean_nz
  pptMetrics$CV_med_nz<-pptMetrics$IQR_nz/pptMetrics$med_nz
  pptMetrics$nRain<-pptMetrics$n-pptMetrics$zeroRain
  
  pptMetrics$maxErrorPerc[pptMetrics$maxErrorPerc<0]<-NA
  
  # add mean rain day-1, day+2
  pptMetrics$maxRain_after<-c(pptMetrics$maxRain[2:nrow(pptMetrics)],NA)
  pptMetrics$maxRain_before<-c(NA, pptMetrics$maxRain[1:nrow(pptMetrics)-1])
  pptMetrics$maxRain_after_diff<-pptMetrics$maxRain-pptMetrics$maxRain_after
  pptMetrics$maxRain_before_diff<-pptMetrics$maxRain-pptMetrics$maxRain_before
  
  # heat map of % of obs with rain
  
  p<-ggplot(pptMetrics, aes(CV,maxRain, color=mae, label=date))+
    geom_point()+
    scale_colour_gradientn(colours=rainbow(4))
    #geom_smooth()
    #geom_vline(xintercept = 1.2)
  ggplotly(p)
  
    ggplot(pptMetrics, aes(sdRain,CV, color=maxRain))+
      geom_point()+
      scale_colour_gradientn(colours=rainbow(4))
    
    
  ggplot(subset(pptMetrics,p50_15>=1), aes(CV,maxRain, color=percZero))+
    geom_point()
    #geom_vline(xintercept = 1.2)
  
  # look at subsets based on thresholds
  #thresh<-subset(pptMetrics, p50_15>1)
  thresh<-subset(pptMetrics, nRain>=10)
  thresh<-subset(thresh, top1_2_ratio<5)
  
  ggplot(thresh, aes(CV,maxRain, color=percZero))+
    geom_point()+
    scale_colour_gradientn(colours=rainbow(4))
    #geom_vline(xintercept = 1.2)
 
  # filtering ideas
  # obs that are single nrain ob of day
  subMetrics<-subset(pptMetrics, nRain>=5)
  subMetrics<-subset(subMetrics, p50_15>=1)
  #subMetrics<-subset(subMetrics, maxError<=2)

  quantile(subMetrics$CV, c(0.5,0.9,0.95))
  #subMetrics<-subset(subMetrics, CV<=quantile(subMetrics$CV, c(0.9)))
  
 p<-ggplot(subMetrics, aes(CV,maxRain, color=mae, label=date))+
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
    dt<-"2021-08-26"
    #dt<-dates[1527]
    #plot(precipStack[[which(layerInfo==dt)]])
    temp<-subset(subDaysRev, date==dt)
    temp<-subset(subDaysDF, date==dt)
    #temp<-subset(temp, flag==0)
    #temp<-subset(temp, flag2==0)
    #boxplot(subset(temp,precip!=0)$precip)
    
    # station labs
    labs <- lapply(seq(nrow(temp)), function(i) {
      paste0( '<p> <b> Station name:', temp[i, "gaugeID"], '</b></p>', 
              '<p> Network Name:', temp[i, "network"], '</p>',
              '<p> <b> <font color="green"> Precip (in):', temp[i, "precip"], '</b></font></p>',
              '<p> <font color="red"> Error:', temp[i, "error"], '</font></p>',
              '<p> <font color="red"> Error %:', temp[i, "errorPerc"], '</font></p>') 
    })
    
    library(leaflet)
    pal <- colorNumeric(
      palette = grDevices::colorRampPalette(c('lightblue','blue','green','red'))(length(temp$precip)), 
      domain = temp$precip)
    
    leaflet() %>% addProviderTiles(providers$Esri.WorldTopoMap) %>%
      setView(-110.909479, 32.221551, zoom = 10) %>%
      addCircleMarkers(temp$lon, temp$lat,
                       radius = 3,
                       color =  pal(temp$precip),
                       label = lapply(labs, htmltools::HTML))
     
    
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
b.wet<-reclassify(precipStack,c(-Inf,1.5,0))
b.wet<-reclassify(b.wet,c(1.5,+Inf,1))
#plot(b.wet[[100]])
#plot(sum(b.wet), zlim=c(300,700))
hist(sum(b.wet, na.rm = TRUE))
plot(sum(b.wet, na.rm = TRUE), zlim=c(0,15))

test<-values(sum(b.wet, na.rm = TRUE))
tempGrid<-sum(b.wet)

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




