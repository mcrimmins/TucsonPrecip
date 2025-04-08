# get monsoon daily PRISM for Tucson area
# adapted from ClimPlot/PRISMPrecipPerc.R
# MAC 08/22/2022

library(RCurl)
library(jsonlite)
library(raster)

# loop through each and create growing stack of cumulative precip - does not work with webservice ----
# write to file
allPrecip <- stack()
for(year in 2007:2022){
  # create current date
  dateRangeStart=paste0(year,"-06-02")
  dateRangeEnd= paste0(year,"-10-01")
  
  # generate dates -- keep with PRISM date
  allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),1)
  
  # AZ/NM bbox -115.004883,31.184609,-102.524414,37.387617
  #  r<-raster(extent(c(-111.3482, -110.4693,31.8,32.6)),res=0.01) from IDW_Rainlog.R
  ACISbbox<-"-111.3482,31.8,-110.4693,32.6"
  
  # ACIS query
  jsonQuery=paste0('{"bbox":"',ACISbbox,'","sdate":"',dateRangeStart,'","edate":"',dateRangeEnd,'","grid":"21","elems":"pcpn","meta":"ll,elev","output":"json"}') # or uid
  #jsonQuery=paste0('{"bbox":"',ACISbbox,'","sdate":"',dateRangeStart,'","edate":"',dateRangeEnd,'","grid":"2","elems":"pcpn","meta":"ll","output":"json"}') # or uid
  
  out<-postForm("http://data.rcc-acis.org/GridData",
                .opts = list(postfields = jsonQuery,
                             httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
  out<-fromJSON(out)
  
  # convert to list of matrices, flipud with PRISM
  matrixList <- vector("list",length(out$data))
  for(i in 1:length(out$data)){
    matrixList[[i]]<-apply(t(out$data[[i]][[2]]),1,rev)
  }
  
  # read into raster stack
  rasterList<-lapply(matrixList, raster)
  gridStack<-stack(rasterList)
  gridExtent<-extent(min(out$meta$lon), max(out$meta$lon), min(out$meta$lat), max(out$meta$lat))
  gridStack<-setExtent(gridStack, gridExtent, keepres=FALSE, snap=FALSE)
  names(gridStack)<-allDates
  # set 0 and neg to NA
  gridStack[gridStack < 0] <- NA
  ## add dates as names, adjusted to day of precip
  names(gridStack)<-allDates-1
  
  # add to main stack
  allPrecip<-stack(allPrecip,gridStack)
  
  # cumPrecipAll <- stack(cumPrecipAll , tempGrid)
  print(year)
}
# ----

writeRaster(allPrecip,filename=paste0("/home/crimmins/RProjects/precipPatterns/gridded/Tucson_PRISM_monsoon_2007_2022.grd"), overwrite=TRUE)
