

months<-1:12
attributes(tmp$date)
yearBeg=as.numeric(format(tmp$date[1],'%Y')) ## assumes ordered for now
yearEnd=as.numeric(format(tmp$date[nrow(tmp)],"%Y"))
years=yearBeg:yearEnd

tmp$year=as.numeric(unlist(lapply(tmp$date,function(x){format(x,"%Y")})))
tmp$month=as.numeric(unlist(lapply(tmp$date,function(x){format(x,"%m")})))

normalGrid=expand.grid(months,years)
normalGrid$normVal=rep(NA,nrow(normalGrid))
names(normalGrid)[1:2]=c("month","year")

for(i in 1:nrow(normalGrid)){
  
  sub=subset(tmp,year==normalGrid[i,2] & month==normalGrid[i,1])
  
  normalGrid$normVal[i]=mean(sub$res,na.rm=T)
  print(i)
  
}

merged=merge(tmp,normalGrid,by.x=c("year","month"),by.y=c("year","month"))
head(merged)

ylabel=with(lablk, lngs[shrt == merged$resdup[1]])

data=merged[!is.na(merged$normVal),]
ggplot(data, aes(x = date, y = normVal))+geom_point()

ggplot(data, aes(x = date, y = res))+geom_point()
+geom_point(data,aes(x=date,y=res),color="red")
  
geom_line(aes(y = mod$fitted.values),color='darkblue',lwd=1)+
  geom_line(aes(y = modNoFlow$fitted.values),lwd=1,color="orange")+
  xlim(xlim)+
  xlab("")+
  ylab(ylabel)#+


###
### above normalization is all the same
### now just aggregate same month across all years
### aggregate predictions from GAM, not inputs
normalGrid=as.data.frame(months)

normalGrid$normVal=rep(NA,nrow(normalGrid))
names(normalGrid)[1]=c("month")

for(i in 1:nrow(normalGrid)){
  
  sub=subset(tmp,  month==normalGrid[i,1])
  
  normalGrid$normVal[i]=mean(sub$res,na.rm=T)
  print(i)
  
}

merged=merge(tmp,normalGrid,by.x="month",by.y="month")
head(merged)

head(merged)

merged=merged[order(merged$date),]

data=merged[!is.na(merged$normVal),]
data=data[!is.na(merged$flo),]
