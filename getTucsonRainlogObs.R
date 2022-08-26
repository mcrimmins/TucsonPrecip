# download data for Tucson Rainlog Analysis
# MAC 03/21/19

library(RCurl)
library(jsonlite)

# set date ranges
dateRangeStart="2007-01-02"
dateRangeEnd="2021-12-31"
allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),1)

# timing of query
start_time <- Sys.time()

# while statement to loop through pages
limit<-1000
i<-0
done<-0

# using geographic center of US 39.828165, -98.579480
while (done==0) {
  #jsonQuery=paste0('{"quality":["Good"],"pagination":{"offset":',i,',"limit":',limit,'},"dateRangeStart":"',dateRangeStart,'","dateRangeEnd":"',dateRangeEnd,'","region":{"type":"Circle","center":{"lat":32.221551,"lng":-110.909479},"radius":25.0}}')
  jsonQuery=paste0('{"pagination":{"offset":',i,',"limit":',limit,'},"dateRangeStart":"',dateRangeStart,'","dateRangeEnd":"',dateRangeEnd,'","region":{"type":"Circle","center":{"lat":32.221551,"lng":-110.909479},"radius":25.0}}')
  
    out<-postForm("https://rainlog.org/api/1.0/Reading/getFiltered", 
                .opts = list(postfields = jsonQuery, 
                             httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
  out<-fromJSON(out)
  if(exists("out")==TRUE){
    if (i==0){
      dataStack<-out
    }else{
      dataStack<-rbind(dataStack, out)
    }
    if (out$readingDate[length(out$readingDate)]==dateRangeEnd){
      done<-1
      break
    }else{}
    
  }else{
    break
    done<-1
  }
  
  i <-i+limit
  rm(out)
  print(i)
}

# get gauges
# while statement to loop through gauges
limit<-1000
i<-0
done<-0

while (done==0) {
  jsonQuery=paste0('{"dateRangeStart":"',dateRangeStart,'","dateRangeEnd":"',dateRangeEnd,'","region":{"type":"Circle","center":{"lat":32.221551,"lng":-110.909479},"radius":25.0},"pagination":{"offset":',i,',"limit":',limit,'}}')
  out<-postForm("https://rainlog.org/api/1.0/GaugeRevision/getFiltered", 
                .opts = list(postfields = jsonQuery, 
                             httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
  out<-fromJSON(out)
  if(exists("out")==TRUE){
    if (i==0){
      gaugeStack<-flatten(out)
    }else{
      gaugeStack<-rbind(gaugeStack, flatten(out))
    }
  }else{
    break
    done<-1
  }
  
  i <-i+limit
  rm(out)
  print(i)
}

# end of timing
end_time <- Sys.time()
end_time - start_time

save(dataStack, gaugeStack, file="./manuscript/TucsonRainlogObs_2014_2018_allQuality.RData")