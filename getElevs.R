# add elevations to Tucson Rainlog gauges
# MAC 04/18/2019

#load("~/RProjects/RainlogAPI/manuscript/TucsonRainlogObs_2014_2018.RData")
load("TucsonRainlogObs_2007_2021_allQuality.RData")

# add elevations to gauges
# https://cran.r-project.org/web/packages/elevatr/vignettes/introduction_to_elevatr.html#get_point_elevation_data
library(elevatr)

prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

LatLons<-gaugeStack[c(10,9,1)]
colnames(LatLons)<-c("x","y","gaugeRevisionId")

LatLons_Elevs <- get_elev_point(LatLons, prj = prj_dd, src = "epqs")

save(LatLons_Elevs, file="TucsonGauges_Elevations.RData" )



