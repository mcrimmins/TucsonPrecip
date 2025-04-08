# nearest neighbor outlier detection
# MAC 11/1/2022


library(RANN)

load("~/RProjects/precipPatterns/interpOut/Tucson_All_IDW_1km_beta3_5ngb_wGridErrors_monsoon_2007_2022_data.RData")

# subset monsoon days
subDays<-tucsonRain_error[tucsonRain_error$dummyDate >= "2020-06-01" & tucsonRain_error$dummyDate <= "2020-09-30", ] # extract just monsoon days
# delete rows with NA
subDays<-subDays[!is.na(subDays$precip),]

# remove above elevation
#hist(subDays$elevation, breaks=50)
subDays<-subset(subDays, elevation<=1200)

# get date list from subset
dates<-as.data.frame(unique(subDays$date))
colnames(dates)<-"ymd"
dates<-dates[order(dates$ymd),]

# date index
idx<-which(dates>="2007-06-01" & dates<="2022-09-30")

for(i in idx[1]:idx[length(idx)]){
  
  # subset data
  dayPrecip<-subset(subDays, date==dates[i])
  # find nns
  kn=6
  nns<-nn2(dayPrecip[,c(2,3)],dayPrecip[,c(2,3)], k=kn)
  # get precip data 
  nidx<-list()
  ndist<-list()
  for(k in 2:kn){
    nidx[[k]]<-dayPrecip$precip[nns[["nn.idx"]][,k]]
    ndist[[k]]<-nns[["nn.dists"]][,k]
  }
 
  nidx<-as.data.frame(do.call(cbind, nidx))
  ndist<-as.data.frame(do.call(cbind, ndist))
  # calculate sd across nn obs, cv, mean dist
  ndist$meanDist<-rowMeans(ndist) 
}


