# station data from RCC-ACIS for Tucson monsoon precip gridding
# adapted from multinetworkSPIv2.R
# MAC 08/22/22

library(RCurl)
library(jsonlite)

# set date ranges
dateRangeStart="2007-01-02"
dateRangeEnd="2023-01-01"
allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),1)


#  r<-raster(extent(c(-111.3482, -110.4693,31.8,32.6)),res=0.01) from IDW_Rainlog.R
ACISbbox<-"-111.3482,31.8,-110.4693,32.6" # 
#ACISbbox<-paste0(-114.8154,',',31.32917,',',-109.0449,',',37.00459)

jsonQuery=paste0('{"bbox":"',ACISbbox,'","sdate":"',dateRangeStart,'","edate":"',dateRangeEnd,'","elems":[{"name":"pcpn","add":"f"}],"meta":"name,ll"}') # or uid
out<-postForm("http://data.rcc-acis.org/MultiStnData", 
              .opts = list(postfields = jsonQuery, 
                           httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
outACIS<-fromJSON(out)

# subset ACIS
# format into dataframe
ll<-data.frame(matrix(unlist(outACIS$data$meta$ll), nrow=length(outACIS$data$meta$ll), byrow=T))
meta<-outACIS$data$meta
# get summary formatted

# deal with data flags
dataList<-list()
for(i in 1:length(outACIS$data$data)){
  temp<-as.data.frame(outACIS[["data"]][["data"]][[i]])
    temp$V1<-as.numeric(as.character(temp$V1))
    temp$V2<-as.character(temp$V2)
    temp$V1<-ifelse(temp$V2=="A",NA,temp$V1) # NA for accum values
    temp$V1<-ifelse(temp$V2=="T",0,temp$V1) # NA for trace values
    temp$V1<-ifelse(temp$V2=="S",NA,temp$V1) # NA for snow values
    dataList[[i]]<-as.data.frame(t(temp$V1))
}
allData = do.call(rbind, dataList)

#allData<- data.frame(matrix(unlist(outACIS$data$data), nrow=nrow(outACIS$data), byrow=T))
colnames(allData)<-allDates

# replace Traces to 0
#allData[allData == "T"] <- "0.00"

# convert obs to numeric
#allData[1:ncol(allData)] <- sapply(allData[1:ncol(allData)],as.character)
#allData[1:ncol(allData)] <- sapply(allData[1:ncol(allData)],as.numeric)

# add in metaData to observations
allData<-cbind.data.frame(ll,meta$name, allData)

# melt into long format
allData <- tidyr::gather(allData,date,precip, 4:ncol(allData), factor_key=TRUE)
colnames(allData)<-c("lon","lat","gaugeID","date","precip")
allData$gaugeID<-as.character(allData$gaugeID)
allData$date<-as.Date(allData$date)

save(allData, file="~/RProjects/precipPatterns/data/TucsonACISObs_2007_2022.RData")
