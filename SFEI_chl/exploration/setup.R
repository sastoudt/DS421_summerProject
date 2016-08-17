setwd("~/Desktop/sfei")
sfei<-read.csv("DWR_wq_data.csv",stringsAsFactors=FALSE)
View(sfei)

names(sfei)

# [1] "Date"      "Station"   "Longitude" "Latitude"  "Depth"     "nh4"       "bod"       "cl"       
# [9] "clt"       "chl"       "EC"        "tkn"       "no3"       "no2"       "no23"      "doc"      
# [17] "toc"       "don"       "ton"       "po4"       "do"        "pH"        "pheo"      "tp"       
# [25] "secchi"    "sio2"      "tds"       "tss"       "vss"       "temp"      "turb"      "sal"      
# [33] "do_per"    "din"       "tn" 
# 
# Depth: feet
# nh4: ammonia mg/L as N
# bod: biochemical oxygen demand
# cl: Chloride mg/L
# clt:
# chl: chhlorophyll a microgram/L
# EC: conductivity microS/cm at 25°C
# tkn: total kjeldahl nitrogen mg/L as N
# no3: nitrate mg/L as N
# no2: nitrogen dioxite
# no23: nitrogen oxide
# doc: dissolved organic carbon
# toc: total organic carbon
# don: dissolved organic nitrogen? mg/L as N
# ton: total oxidized nitrogen
# po4: phosphate
# do: dissolved oxygen
# pH: pH
# pheo: pheophytin a migrogram/L
# tp: total phosphorus mg/L
# secchi: secchi depth? cm
# sio2: silica mg/L
# tds: total dissolved solids mg/L
# tss: total suspended solids mg/L
# vss: volatile suspended solid mg/L
# temp: deg C
# turb: turbidity NTU
# sal: salinity
# do_per:
# din: dissolved inorganic nitrogen mg/L as N
# tn: total nitrogen

require(WRTDStidal)
require(dplyr)
require(lubridate)

## get dates in right format and add day of year
sfei$Date=as.Date(as.character(sfei$Date),format="%m/%d/%Y")
sfei$date_dec = dec_time(sfei$Date)[['dec_time']]
sfei$doy = yday(sfei$Date)

plot(density(sfei$chl,na.rm=T))
summary(sfei$chl)
boxplot(sfei$chl)

plot(density(log(sfei$chl),na.rm=T)) ## definitely need log transform

## so transform or GLM with link 
##http://stats.stackexchange.com/questions/47840/linear-model-with-log-transformed-response-vs-generalized-linear-model-with-log
## suggests using GLM 
## family=gaussian(link="log")

require(mgcv)

length(unique(sfei$Station))
stations=unique(sfei$Station)


## make list where each entry is a dataframe with one station's worth of data
perStation<- vector(mode = "list", length = length(unique(sfei$Station)))

for(i in 1:length(unique(sfei$Station))){
  perStation[[i]]=subset(sfei,Station==stations[i])
  perStation[[i]]$Date=as.Date(perStation[[i]]$Date)
}
names(perStation)=stations

write.csv(sfei,"sfeiPlusDates.csv",row.names=F)
save(perStation,file="perStation.Rda")

