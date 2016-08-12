## D10- missing tn, tp, sio2, tss, nh4
## D12- missing tn, tp, sio2, tss, nh4
## D22- missing tn, tp, sio2, tss, nh4
## so basically it is fine that these are cut off in the full model, but remove tn from 
## parsimonious model for these
## also just noticed that I forgot to put salinity in the full model
## for the record these have salinity

## missing part of series
index=which(names(perStation)=="D10")
#5,7,13

# data
data<-perStation[[index]]
mod<-perStationParsMod[[index]]

toUse=na.omit(data[,c("doy","date_dec","pheo","tn","do_per","Date")]) ## missing tn
fullPred=predict(mod,toUse,type="response")
toUse=as.data.frame(cbind.data.frame(toUse,fullPred))
names(toUse)[7]="fitted.values"
ggplot(data,aes(x = Date, y = chl))+geom_point()+
  geom_line(data=toUse,aes(x=Date,y =fitted.values ,col="red"),lwd=1)+
  ggtitle(paste(names(perStation)[index], "Fitted Values Parsimonious Model",sep=" "))+
  theme(legend.position='none')+
  scale_x_date(limits = dt_rng)+ylab("chl a (microgram/L)")+xlab("Date")


## D7 has weird date_dec fit

