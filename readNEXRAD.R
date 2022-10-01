# read in NEXRAD MOSAIC radar data from IEM mesonet
# https://mesonet.agron.iastate.edu/docs/nexrad_mosaic/
# MAC 09/10/22

library(raster)

# helper functions for raster image
# based on https://stackoverflow.com/questions/42100791/rdgal-tiff-files-and-worldfile
Lon = function(JJ) 0.01 * JJ + -126
Lat = function(II) -0.01 * II + 50.0

# create date string
dates<-as.data.frame(seq(as.Date("2007-01-01"),as.Date("2021-12-31"),by='day'))
  colnames(dates)<-"date"
dates$month<-format(dates$date,"%m")
dates$day<-format(dates$date, "%d")
dates$year<-format(dates$date,"%Y")
dates$string<-format(dates$date,"%Y%m%d")
dates$dummyDate<-paste0("1981-",dates$month,"-",dates$day)

# subset monsoon season dates
dates<-dates[dates$dummyDate >= "1981-06-15" & dates$dummyDate <= "1981-09-30", ]
  
# empty stack
radarDays <- stack()

# loop through days
for(i in 1:nrow(dates)){
  # download image
  download.file(paste0("https://mesonet.agron.iastate.edu/archive/data/",dates$year[i],"/",dates$month[i],"/",dates$day[i],"/GIS/uscomp/max_n0r_0z0z_",dates$string[i],".png"), destfile = "./radar/temp.png", method="curl") # created subdir 'shapes'
  # read into raster
  rr<-raster("./radar/temp.png")
  ext.rr <- extent(rr)
  rr2 <- raster(nrows=nrow(rr), ncols=ncol(rr), xmn=Lon(ext.rr@xmin), xmx=Lon(ext.rr@xmax), ymn=Lat(ext.rr@ymax), ymx=Lat(ext.rr@ymin))
  values(rr2) <- values(rr)
  # crop down to save space
  rr2<-crop(rr2,extent(-111.3482,-110.4693,31.8,32.6))
  #values(rr2) <- (values(rr) - 7) * 5
  radarDays <- stack( radarDays , rr2 )
  print(dates$date[i])
  #Sys.sleep(2)
}

# load in project extent
resObsDens<-stack("/home/crimmins/RProjects/precipPatterns/interpOut/Tucson_All_1km_densityMask.grd")
# get new mask
tucRadarDays<-crop(radarDays, resObsDens)
densMask<-resample(resObsDens, tucRadarDays)
  tucRadarDays<-mask(tucRadarDays, densMask)

# add in names
names(tucRadarDays)<-dates$date  
  
# get layer stats
  dates$maxZ <- cellStats(tucRadarDays, "m")
  
# plot some data
  library(rasterVis)
  
levelplot(tucRadarDays)

