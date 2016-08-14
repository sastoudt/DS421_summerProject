### investigate P8 annual flow-normalized
setwd("~/Desktop/DS421_summerProject/GAM_weightedReg_comparison/shiny/data")
load("dataNice_nolag.RData")
grep("P8",names(dataNiceNoLag)) ## 7, 8, 9

load("modelsNoLag_NoFlow_Nested.RData") ##modelsNoLag_NoFlow_Nested
load("modelsNoLag_Nested.RData") ##modelsNoLag_Nested

flowPlotNorm_SAS=function(data,mod,modNoFlow,xlim=range(data$date),scale=F,annual=F){
  ylabel=with(lablk, lngs[shrt == data$resdup[1]])
  # if(scale){
  #   data$res=exp(data$res)
  #   mod$fitted.values=exp(mod$fitted.values)
  #   modNoFlow$fitted.values=exp(modNoFlow$fitted.values)
  #   ylabel <- gsub('ln-|log-', '', as.character(ylabel))
  #   ylabel <- as.expression(parse(text = ylabel))
  # 
  # }
  data=data[!is.na(data$res),]
  data=data[!is.na(data$flo),]
  data$month=as.numeric(strftime(data$date, '%m'))
  data$year=as.numeric(strftime(data$date, '%Y'))
  monthly<-group_by(data,month)
  monthFlow<-summarise(monthly,meanFlow=mean(flo,na.rm=T))
  monthFlow=as.data.frame(monthFlow)
  names(monthFlow)=c("month","avgFlow")
  
  mergeData=merge(data,monthFlow,by.x="month",by.y="month")
  
  normVal=getFlowNormalized(mod,data,monthFlow$avgFlow)
  normValNoFlow=getFlowNormalized(modNoFlow,data,monthFlow$avgFlow)
  
  normVal$date=as.Date(paste(normVal$year,normVal$month,"01",sep="-"))
  normValNoFlow$date=as.Date(paste(normValNoFlow$year,normValNoFlow$month,"01",sep="-"))
  normVal=normVal[order(normVal$date),]
  normValNoFlow=normValNoFlow[order(normValNoFlow$date),]
  #print(head(normValNoFlow))
  #print(head(normVal))
  data2=as.data.frame(cbind.data.frame(normVal$date,normVal$res,normValNoFlow$res,normVal$month,normVal$year))
  names(data2)=c("date","normVal","normValNoFlow","month","year")
  
  mergeData2=merge(mergeData,data2,by.x=c("year","month"),by.y=c("year","month"))
  # data$month=as.numeric(unlist(lapply(data$date,function(x){format(x,"%m")})))
  # data$predVal=mod$fitted.values
  # data$predValNoFlow=modNoFlow$fitted.values
  # months=1:12
  # normalGrid=as.data.frame(months)
  # 
  # normalGrid$normVal=rep(NA,nrow(normalGrid))
  # normalGrid$normValNoFlow=rep(NA,nrow(normalGrid))
  # names(normalGrid)[1]=c("month")
  # 
  # for(i in 1:nrow(normalGrid)){
  #   
  #   sub=subset(data,  month==normalGrid[i,1])
  #   
  #   normalGrid$normVal[i]=mean(sub$predVal,na.rm=T)
  #   normalGrid$normValNoFlow[i]=mean(sub$predValNoFlow,na.rm=T)
  #   #print(i)
  
  # }
  titleLab=ifelse(scale,data$resdup[1],paste("ln(",data$resdup[1],")",sep=""))
  
  if(annual){
    forAgg=mergeData2[,c("date.x","res","normVal")]
    forAgg2=mergeData2[,c("date.x","res","normValNoFlow")]
    names(forAgg)=names(forAgg2)=c("date","res","norm")
    data1=annual_agg(forAgg,min_mo=11)
    data2=annual_agg(forAgg2,min_mo=11)
    
    if(scale){
      data1$res=exp(data1$res)
      data2$res=exp(data2$res)
      data1$norm=exp(data1$norm)
      data2$norm=exp(data2$norm)
      
      ylabel <- gsub('ln-|log-', '', as.character(ylabel))
      ylabel <- as.expression(parse(text = ylabel))
      
    }
    data1=data1[order(data1$date),]
    data2=data2[order(data2$date),]
    txt <- paste0("Flow Normalized: ",titleLab, ' ~ s(time) + s(season) + [s(flo)]') 
    
    ggplot(data1, aes(x = date, y = res))+geom_point()+
      geom_line(aes(y = norm, color = 'With Flow'),lwd=1)+
      geom_line(data=data2,aes(y = norm, color = 'No Flow'),lwd=1)+
      xlim(xlim)+
      xlab("")+
      ylab(ylabel)+
      scale_colour_manual(name = '',
                          labels = c('With Flow', 'No Flow'),
                          values =c('darkblue','orange')
      ) +
      ggtitle(txt)
    
  }
  
  else{
    
    # title
    txt <- paste0("Flow Normalized: ",titleLab, ' ~ s(time) + s(season) + [s(flo)]') 
    
    #data=merge(data,normalGrid,by.x="month",by.y="month")
    
    #data=data[order(data$date),]
    data=mergeData2[order(mergeData2$date.x),]
    
    if(scale){
      data$res=exp(data$res)
      data$normVal=exp(data$normVal)
      data$normValNoFlow=exp(data$normValNoFlow)
      ylabel <- gsub('ln-|log-', '', as.character(ylabel))
      ylabel <- as.expression(parse(text = ylabel))
      
    }
    
    
    ggplot(data, aes(x = date.x, y = res))+geom_point()+
      geom_line(aes(y = normVal, color = 'With Flow'),lwd=1)+
      geom_line(aes(y = normValNoFlow, color = 'No Flow'),lwd=1)+
      xlim(xlim)+
      xlab("")+
      ylab(ylabel)+
      scale_colour_manual(name = '',
                          labels = c('With Flow', 'No Flow'),
                          values =c('darkblue','orange')
      ) +
      ggtitle(txt)
    
  }
  
}


dat=dataNiceNoLag[[dat()]]
flowPlotNorm_SAS(dat,modelsNoLag_NoFlow_Nested[[mod()]],modelsNoLag_Nested[[mod()]],xlim=dt_rng,scale=logspace,annual=annuals)
