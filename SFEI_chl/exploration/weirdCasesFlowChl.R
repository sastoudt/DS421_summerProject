
stat <-"D19"
index=which(names(perStation)==stat)

# data
data<- perStationAdd[[index]]
data$TOT=0.028316847*data$TOT
mod<-perStationFlowTOT[[index]]
toUse=na.omit(data[,c("doy","date_dec","Date","TOT")])
byTerm=predict(mod,toUse,type="terms")
toName=c("doy","date_dec","tot","date","intercept")

nestPred=as.data.frame(cbind.data.frame(byTerm,toUse$Date,rep(summary(mod)$p.coeff,nrow(toUse))))
names(nestPred)=toName
terms<-c("ti(doy)","ti(date_dec)","ti(tot)","intercept")
ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
  geom_line(data=nestPred,aes(x=date,y = doy, color = 'ti(doy)'), lwd=1)+
  geom_line(data=nestPred,aes(x=date,y=date_dec, color = 'ti(date_dec)'), lwd=1)+
  geom_line(data=nestPred,aes(x=date,y = tot, color = 'ti(tot)'), lwd=1)+
  geom_line(data=nestPred,aes(x=date,y = intercept, color = 'intercept'), lwd=1)+
  scale_colour_manual(name = '',
                      labels =c('red'=terms[1],"dodgerblue"=terms[2],
                                "forestgreen"=terms[3],"purple"=terms[4]),values=c("red",
                                                                                   "dodgerblue","forestgreen","purple")
  ) 

stat <-"D26"
index=which(names(perStation)==stat)

# data
data<- perStationAdd[[index]]
data$TOT=0.028316847*data$TOT
mod<-perStationFlowTOT[[index]]
toUse=na.omit(data[,c("doy","date_dec","Date","TOT")])
byTerm=predict(mod,toUse,type="terms")
toName=c("doy","date_dec","tot","date","intercept")

nestPred=as.data.frame(cbind.data.frame(byTerm,toUse$Date,rep(summary(mod)$p.coeff,nrow(toUse))))
names(nestPred)=toName
terms<-c("ti(doy)","ti(date_dec)","ti(tot)","intercept")
ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
  geom_line(data=nestPred,aes(x=date,y = doy, color = 'ti(doy)'), lwd=1)+
  geom_line(data=nestPred,aes(x=date,y=date_dec, color = 'ti(date_dec)'), lwd=1)+
  geom_line(data=nestPred,aes(x=date,y = tot, color = 'ti(tot)'), lwd=1)+
  geom_line(data=nestPred,aes(x=date,y = intercept, color = 'intercept'), lwd=1)+
  scale_colour_manual(name = '',
                      labels =c('red'=terms[1],"dodgerblue"=terms[2],
                                "forestgreen"=terms[3],"purple"=terms[4]),values=c("red",
                                                                                   "dodgerblue","forestgreen","purple")
  ) 



data=perStationAdd[[index]]

ggplot(data,aes(x = Date, y = chl))+geom_point()+
  #geom_line(aes(x=Date,y =names(data)[index] ,col="red"),lwd=1)+
  geom_line(data=data,aes(x=Date,y = flowPred, color = "red"),lwd=1)+
  
  ggtitle("Flow Fitted Values Model")+
 
  summary(data$flowPred)


